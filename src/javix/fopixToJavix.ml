(** This module implements a compiler from Fopix to Javix. *)
let error pos msg = Error.error "compilation" pos msg

(** As in any module that implements {!Compilers.Compiler}, the source
    language and the target language must be specified. *)
module Source = Fopix
module Target = Javix

module S = Source.AST
module T = Target.AST

(** We will need the following pieces of information to be carrying
    along the translation: *)
type environment = {
  nextvar          : int;
  variables        : (S.identifier * T.var) list;
  function_labels  : (S.function_identifier * T.label) list;
  (** [function_formals] maintains the relation between function identifiers
      and their formal arguments. *)
  function_formals : (S.function_identifier * S.formals) list;
  other : T.label;
}

(** Initially, the environment is empty. *)
let initial_environment () = {
    nextvar          = 0;
    variables        = [];
    function_labels  = [];
    function_formals = [];
    other            = T.Label "dispatch";
}

(** [lookup_variables f env] returns the variable of [x] in [env]. *)
let lookup_variables x env = List.assoc x env.variables

(** [get_last_variables env] returns th last variable in [env] *)
let get_last_variable env = List.hd env.variables |> snd

(** [lookup_function_label f env] returns the label of [f] in [env]. *)
let lookup_function_label f env = List.assoc f env.function_labels

(** [lookup_function_formals f env] returns the formal arguments of
    [f] in [env]. *)
let lookup_function_formals f env = List.assoc f env.function_formals

(** [fresh_function_label f] returns a fresh label starting with [f]
    that will be used for the function body instructions. *)
let fresh_function_label =
  let r = ref 0 in
  fun f -> incr r; T.Label (f ^ "_body_" ^ string_of_int !r)

let fresh_label =
  let r = ref 0 in
  fun f -> incr r; T.Label ("continue_" ^ string_of_int !r)

let fresh_var =
  let r = ref 0 in
  fun f -> incr r; "_v" ^ string_of_int !r

(** Variables *)

(** [bind_variable env x] associates Fopix variable x to the next
    available Javix variable, and return this variable and the updated
    environment *)
let bind_variable env x =
  let v = T.Var env.nextvar in
  v,
  { env with
    nextvar = env.nextvar + 1;
    variables = (x,v) :: env.variables }

(** [bind_function_label env f] associates Fopix function identifier to
    an unique javix label, and return this unique label and updated
    environment *)
let bind_function_label env f =
  let fl = fresh_function_label f in
  fl, { env with function_labels = (f, fl) :: env.function_labels }

(** [bind_function_formals env f formals] associates Fopix function
    identifier to his formals *)
let bind_function_formals env f formals =
  { env with function_formals = (f, formals) :: env.function_formals }

let bind_function_variables env formals =
  List.fold_left (fun a x -> bind_variable a x |> snd) env formals

(** [bind_function env0 f formals] update environment with
    bind_function_label and bind_function_formals and return
    the unique label and the new environment *)
(* let bind_function env0 f formals = *)
(*   let fl, env1 = bind_function_label env0 f          in *)
(*   let env2 = bind_function_formals env1 f formals    in *)
(*   let env3 = bind_function_variables env2 formals    in *)
(*   fl, env3 *)

let clear_all_variables env = {env with variables = []; nextvar = 0}

(** For return addresses (or later higher-order functions),
    we encode some labels as numbers. These numbers could then
    be placed in the stack, and will be used in a final tableswitch *)
module Labels :
sig
  val encode : T.label -> int
  val all_encodings : unit -> (int * T.label) list
  val lookup_lbl_addr : T.label -> int
end
=
struct
  let nextcode = ref 0
  let allcodes = ref ([]:(int * T.label) list)
  let encode lab =
    let n = !nextcode in
    incr nextcode;
    allcodes := (n,lab) :: !allcodes;
    n
  let all_encodings () = !allcodes
  let lookup_lbl_addr lbl =
    List.(assoc lbl (map (fun (x,y) -> (y,x)) (all_encodings ())))
end

let basic_program code =
  { T.classname = "Fopix";
    T.code = code;
    T.varsize = 1000;
    T.stacksize = 10000; }

let arith_op =
  S.([(Add,( + ));(Sub,( - ));(Mul,( * ));(Div,( / ));(Mod,( mod )) ])

(** [rewrite_fopix env ast] apply simplification on fopix ast *)
let rec rewrite_fopix env = function
  | S.DefVal (x,e) ->
    let e' = rewrite_fopix_expression env e                         in
    S.DefVal (x, e')

  | S.DefFun (fid, formals, e) ->
    let env' = List.map (fun x -> (x, fresh_var ())) formals        in
    let e' = rewrite_fopix_expression  env' e                       in
    let formals = List.map snd env'                                 in
    S.DefFun (fid, formals, e')

and rewrite_fopix_expression env = function
  | S.Num _
  | S.FunName _ as expr -> expr

  | S.Var v as var ->
    (try S.Var (List.assoc v env)
     with Not_found -> var)

  | S.Def (x, e1, e2) ->
    let e1' = rewrite_fopix_expression env e1                       in
    let x' = fresh_var () in
    let env = (x, x'):: (List.remove_assoc x env ) in
    let e2' = rewrite_fopix_expression env e2                       in
    S.Def (x', e1', e2')

  | S.(IfThenElse(b, t, f)) ->
    let b' = rewrite_cond env b                                     in
    let t' = rewrite_fopix_expression env t                         in
    let f' = rewrite_fopix_expression env f                         in
    S.IfThenElse (b', t', f')

  | S.(BinOp (Add|Sub|Mul|Div|Mod as op, e1, e2)) ->
    let e1' = rewrite_fopix_expression env e1                       in
    let e2' = rewrite_fopix_expression env e2                       in
    begin
      match e1', e2' with
      | S.Num n1, S.Num n2 ->
        S.Num( (List.assoc op arith_op) n1 n2 )
      | _ -> S.BinOp (op, e1', e2')
    end

  | S.(BinOp (Eq|Le|Lt|Ge|Gt as op, e1, e2)) ->
    let e1' = rewrite_fopix_expression env e1                       in
    let e2' = rewrite_fopix_expression env e2                       in
    let b= S.BinOp (op, e1', e2')                                   in
    let t = S.Num 1                                                 in
    let f = S.Num 0                                                 in
    S.IfThenElse (b, t, f)

  | S.BlockNew e ->
    let e' = rewrite_fopix_expression env e                         in
    S.BlockNew e'

  | S.BlockGet (e1, e2) ->
    let e1' = rewrite_fopix_expression env e1                       in
    let e2' = rewrite_fopix_expression env e2                       in
    S.BlockGet (e1', e2')
  | S.BlockSet (e1, e2, e3) ->
    let e1' = rewrite_fopix_expression env e1                       in
    let e2' = rewrite_fopix_expression env e2                       in
    let e3' = rewrite_fopix_expression env e3                       in
    S.BlockSet (e1', e2', e3')

  | S.FunCall (e, el) ->
    let e'  = rewrite_fopix_expression env e                        in
    let el' = List.map (fun x -> rewrite_fopix_expression env x) el in
    S.FunCall (e',el')

and rewrite_cond env = function
  | S.(BinOp(Eq|Le|Lt|Ge|Gt as op,e1,e2)) ->
    let e1' = rewrite_fopix_expression env e1                       in
    let e2' = rewrite_fopix_expression env e2                       in
    S.BinOp(op, e1', e2')
  | b -> rewrite_fopix_expression env b

(** convert a fopix arith op to a javix binop *)
let binop_to_javix binop =
  let binop =
    match binop with
    | S.Add -> T.Add
    | S.Mul -> T.Mul
    | S.Sub -> T.Sub
    | S.Div -> T.Div
    | S.Mod -> T.Rem
    | _ -> assert false (* only binop *)
  in
  T.Binop binop

(** convert a fopix cmp op to a javix cmpop *)
let cmpop_to_javix label cmpop =
  let cmpop =
    match cmpop with
    | S.Eq -> T.Eq
    | S.Le -> T.Le
    | S.Lt -> T.Lt
    | S.Ge -> T.Ge
    | S.Gt -> T.Gt
    | _ -> assert false (* only cmpop *)
  in
  T.If_icmp (cmpop, label)

let no_lbl instr = [(None, instr)]
let no_lbl_l instr_l = List.map no_lbl instr_l
let with_lbl lbl instr = [(Some lbl, instr)]
let with_lbl_s str instr = with_lbl (T.Label str) instr

let comment str  = no_lbl (T.Comment str)
let push int     = no_lbl_l [T.Bipush int ; T.Box]
let goto_lbl lbl = no_lbl (T.Goto lbl)
let goto str     = goto_lbl (T.Label str)
let continue lbl = with_lbl lbl (T.Comment "continue")
let load v       = no_lbl (T.Aload v)
let store v      = no_lbl (T.Astore v)
let unbox        = no_lbl T.Unbox
let box          = no_lbl T.Box
let push n       = no_lbl (T.Bipush n)

(** [rename_first_op lbl code] rename the first label of [code]
    with [lbl] *)
let rename_first_op lbl = function
  | [] -> assert false (* impossible *)
  | (_, instr) :: tl -> with_lbl lbl instr @ tl

let rec save_register n =
  if n = 0 then []
  else no_lbl (T.Aload (T.Var (n-1))) @ save_register (n-1)

let rec load_register n =
  if n = 0 then []
  else no_lbl T.Swap @ no_lbl (T.Astore(T.Var(n-1))) @ load_register(n-1)

let dispatch_lbl = T.Label "dispatch"
let dispatch () =
  let lbl_l = List.(map snd (Labels.all_encodings ()) |> rev) in
  let tableinstr = no_lbl (T.Tableswitch (0, lbl_l, T.Label "error")) in
  let default = with_lbl_s  "error"  (T.Comment "Oups") in
  rename_first_op dispatch_lbl ( tableinstr @  default)

let boxed = true
let unboxed = false

let return_it boxed instr_l =
  let boxed =
    if boxed then []
    else box
  in
  instr_l @ boxed @ no_lbl T.Swap @ goto_lbl dispatch_lbl

let box_it is_boxed instr_l =
  if is_boxed then instr_l
  else instr_l @ box

let unbox_it is_boxed instr_l =
  if is_boxed then instr_l @ unbox
  else instr_l

let bind_function env f formals =
  let  flbl,env = bind_function_label env f in
  let env = bind_function_formals env f formals in
  ignore(Labels.encode flbl);
  env

let tail = true
let not_tail = false

(** [translate p env] turns a Fopix program [p] into a Javix program
    using [env] to retrieve contextual information. *)
let rec translate p env0 : T.t * environment =
  let p   = List.map (fun x -> rewrite_fopix [] x) p in
  let env = bind_all_functions env0 p in
  let f_fold (v,f,e) x =
    let v',f',e'= definition e x in
    v@v',f@f',e'
  in
  let val_ins, fun_ins, env1 =
    List.fold_left f_fold ([],[],env) p
  in
  let last_v = get_last_variable env1 in
  let return = load last_v @ unbox @ no_lbl T.Ireturn in
  let dispatch_instr = dispatch () in
  let code   = val_ins @ return @ fun_ins @ dispatch_instr in
  basic_program code, env1

(** [bind_function env0 f formals] update environment with
    bind_function_label and bind_function_formals and return
    the new environment *)
and bind_all_functions env p =
  let f_fold env = function
    | S.DefVal _ -> env
    | S.DefFun (fid, formals,_) ->
      bind_function env fid formals
  in
  List.fold_left f_fold env p

and definition env = function
  | S.DefVal (x, e) ->
    let (v, env') = bind_variable env x in
    let defval = expression not_tail box_it env e @ store v in
    defval, [], env'

  | S.DefFun (f, formals, e) ->
    let fl = lookup_function_label f env in
    let fenv0 = clear_all_variables env in
    let fenv1 = bind_function_variables fenv0 formals in
    let fcode = expression tail return_it fenv1 e   in
    [], rename_first_op fl fcode , env

and expression is_tail fun_box env = function

  | S.Num i -> fun_box unboxed (push i)

  | S.Var x -> fun_box boxed (load (lookup_variables x env))

  | S.(BinOp (Add|Mul|Div|Sub|Mod as op, e1, e2)) ->
    let e1' = expression not_tail unbox_it env e1 in
    let e2' = expression not_tail unbox_it env e2 in
    let binop = no_lbl (binop_to_javix op) in
    e1' @ e2' @ fun_box unboxed binop

  | S.(IfThenElse (b,t,f)) ->
    let tl = fresh_function_label "_true" in
    let nl = fresh_function_label "_next" in
    let ifb =
      let expr = expression is_tail fun_box env t @ goto_lbl nl in
      rename_first_op tl expr
    in
    let elseb = expression is_tail fun_box env f @ goto_lbl nl in
    let cond_instr = compile_cond_instr env tl b in
    cond_instr @ elseb @ ifb @ continue nl

  | S.FunName f ->
    let fl = lookup_function_label f env in
    let addr = Labels.lookup_lbl_addr fl in
    fun_box unboxed (push addr)

  | S.FunCall (S.FunName fid, args) when is_tail ->
    let args_instr = function_arguments env args in
    let instr_goto = goto_lbl (lookup_function_label fid env) in
    args_instr @ instr_goto

  | S.FunCall (e, args) when is_tail ->
    let args_instr = function_arguments env args in
    let e' = expression not_tail unbox_it env e in
    e' @ args_instr @ goto_lbl dispatch_lbl

  | S.FunCall (S.FunName fid, args) ->
    let args_instr = function_arguments env args in
    let return_lbl = fresh_label () in
    let return_addr = Labels.encode return_lbl in
    let instr_goto =goto_lbl (lookup_function_label fid env) in
    save_register env.nextvar
    @ push return_addr @ args_instr @ instr_goto @ continue return_lbl
    @ fun_box boxed (load_register env.nextvar)

  | S.FunCall (e, args) ->
    let args_instr = function_arguments env args in
    let e' = expression not_tail unbox_it env e in
    let return_lbl = fresh_label() in
    let return_addr = Labels.encode return_lbl in
    save_register env.nextvar
    @ push return_addr @ e' @ args_instr @ goto_lbl dispatch_lbl
    @ continue return_lbl
    @ fun_box boxed (load_register env.nextvar)

  | S.BlockNew e ->
    let e' = expression not_tail unbox_it env e in
    e' @ no_lbl T.Anewarray

  | S.BlockGet (e1, e2) ->
    let e1' = expression not_tail box_it env e1 in
    let e2' = expression not_tail unbox_it env e2 in
    e1' @ no_lbl T.Checkarray @ e2' @ fun_box boxed (no_lbl T.AAload)

  | S.BlockSet (e1, e2, e3) ->
    let e1' = expression not_tail box_it env e1 in
    let e2' = expression not_tail unbox_it env e2 in
    let e3' = expression not_tail box_it env e3 in
    e1' @ no_lbl T.Checkarray @ e2' @ e3'
    @ no_lbl T.AAstore @ fun_box unboxed (push 0)

  | S.Def (x, e1, e2) ->
    let e1' = expression not_tail box_it env e1 in
    let v, env' = bind_variable env x in
    let e2' = expression is_tail fun_box env' e2  in
    e1' @ store v @ e2'

  | _ -> assert false (* Error *)

and function_arguments env args =
  let rec store_all n =
    if n = 0 then []
    else store (T.Var (n-1)) @ store_all (n-1)
  in
  let len = List.length args in
  let args = List.(flatten (map (fun e -> expression not_tail box_it env e) args)) in
  args @ store_all len

and compile_cond_instr env lbl = function
  | S.(BinOp (Eq|Le|Lt|Ge|Gt as op, e1, e2)) ->
    let e1' = expression not_tail unbox_it env e1  in
    let e2' = expression not_tail unbox_it env e2  in
    e1' @ e2' @ no_lbl (cmpop_to_javix lbl op)
  | _ as e ->
    let e' = expression not_tail unbox_it env e  in
    e' @ push 1 @ no_lbl (cmpop_to_javix lbl S.Eq)

(** Remarks:
    - When using this compiler from fopix to javix, flap will
    produce some .j files.
    + Compile them to .class via: jasmin Foobar.j
    + Run them with: java -noverify Foobar

    - Final answer:
    your code should contain a final [Ireturn] that should
    return the value of the last DefineValue (supposed to be
    an Integer).

    - Function Call Convention:
    + The n arguments should be in jvm's variables 0,1,...(n-1).
    + At least the variables that are reused after this call
      should have their contents saved in stack before the call
      and restored afterwards.
    + Just before the function call, the return address should
      be placed on the stack (via the encoding as number of this
      return label, see Labels.encode).
    + When the function returns, the result should be on the top
      of the stack.

    - Boxing:
    The stack could contain both unboxed elements (Java int)
    or boxed elements (Java objects such as Integer or java arrays).
    We place into variables or in array cells only boxed values.
    The arithmetical operations (iadd, if_icmpeq, ...) only works
    on unboxed numbers.
    Conversion between int and Integer is possible via the
    Box and Unboxed pseudo-instructions (translated into correct
    calls to some ad-hoc methods we provide). You may try to
    do some obvious optimisations such as removing [Box;Unbox] or
    [Unbox;Box].

    - Tail-recursive calls : if the body of f ends with a call to
    another function g (which may be f itself in case of recursion),
    no need to save any variables, nor to push a new return address:
    just reuse the return address of the current call to f when
    jumping to g !

    - Variable size and stack size
    Your code should determine the number of variables used by the
    produced code. You might also try to compute the maximum
    stack size when the code is non-recursive or 100% tail-recursive.

*)
