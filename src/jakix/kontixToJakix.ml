(** This module implements a compiler from Kontix to Fopix. *)

(** As in any module that implements {!Compilers.Compiler}, the source
    language and the target language must be specified.

    Convention =
    - R0 = continuation
    - R1 = environment

*)

module Source = Kontix
module S = Source.AST
module Target = Jakix
module T = Target.AST

type environment = {
  nextvar          : int;
  variables        : (S.identifier * int) list;
  function_labels  : (S.function_identifier * T.label) list;
  function_formals : (S.function_identifier * S.formals) list;
}

(** [initial_environment ()] create an init environment *)
let initial_environment () = {
  nextvar          = 2;
  variables        = [];
  function_labels  = [];
  function_formals = [];
}

let varsize p =
  let rec local' max = function
    | [] -> let T.Var v = max in v+1
    | ((_, T.Astore n) | (_, T.Aload n))::tl when n > max -> local' n tl
    | _ :: tl -> local' max tl
  in local' (T.Var 0) p

let stacksize p =
  let rec split acc acc' = function
    | [] -> acc'
    | (Some (T.Label str), instr) :: tl when str.[0] <> '_' ->
      split [instr] (acc :: acc') tl
    | (_,instr) :: tl -> split (instr::acc) acc' tl
  in
  let rec count cur max = function
    | [] -> max
    | T.Astore _ :: tl -> count (cur-1) max tl
    | (T.Aload _ | T.Bipush _) :: tl ->
       let cur' = cur+1 in
       let max' = if cur' > max then cur' else max in
       count cur' max' tl
    | T.Binop _ :: tl -> count (cur-1) max tl
    | T.AAstore :: tl -> count (cur-3) max tl
    | T.AAload :: tl -> count (cur-1) max tl
    | T.Tableswitch _ :: tl -> count (cur-1) max tl
    | T.If_icmp _ :: tl -> count (cur-2) max tl
    | _ :: tl -> count cur max tl
  in
  let maxl = List.fold_left (fun a x -> if x > a then x else a) 0 in
  let countl = List.map (count 0 0) in
  let l0 = split [] [] p in
  let l1 = countl l0 in
  maxl l1 + 1
  
(** [basic_program code] create a basic program *)
let basic_program code =
  { T.classname = "Kontix";
    T.code = code;
    T.varsize = varsize code;
    T.stacksize = stacksize code; }

(** [range n m] create a list n..m *)
let rec range n m = if n > m then [] else n :: (range (n+1) m)

(** [fresh_function_label f] returns a fresh label starting with [f]
    that will be used for the function body instructions. *)
let fresh_function_label =
  let r = ref 0 in
  fun f -> incr r; T.Label (f ^ "_body_" ^ string_of_int !r)

(** [lookup_variables f env] returns the variable of [x] in [env]. *)
let lookup_variables x env =
  List.assoc x env.variables

(** [lookup_function_label f env] returns the label of [f] in [env]. *)
let lookup_function_label f env =
  List.assoc f env.function_labels

(** [lookup_function_formals f env] returns the formal arguments of
    [f] in [env]. *)
let lookup_function_formals f env =
  List.assoc f env.function_formals

(** [bind_variable env x] associates Fopix variable x to the next
    available Javix variable, and return this variable and the updated
    environment *)
let bind_variable env x =
  let v = env.nextvar in
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
    the unique label and the environment *)
let bind_function env0 f formals =
  let fl, env1 = bind_function_label env0 f          in
  let env2 = bind_function_formals env1 f formals    in
  let env3 = bind_function_variables env2 formals    in
  fl, env3

(** [bind_all_functions env p] associate all function identifier with
    with an unique label*)
let bind_all_functions env p =
  List.fold_left (fun a x ->
      match x with
      | S.DefFun(f,_,_) ->
        let _,a' = bind_function_label a f in a'
      | S.DefCont(k,_,_,_) ->
        let _,a' = bind_function_label a k in a') env p

(** [rename_first_op lbl code] rename the first label of [code]
    with [lbl] *)
let rename_first_op lbl = function
  | [] -> assert false (* impossible *)
  | (_, instr) :: tl -> (Some lbl,instr) :: tl

(** [rename_first_op2 lbl code] rename the first label of [code]
    with [lbl] *)
let rename_first_op2 lbl = function
  | [] -> assert false (* impossible *)
  | (_, instr) :: tl -> (lbl,instr) :: tl

let is_array = function
  | S.BlockNew _ -> true
  | _ -> false

(** [clear_all_variables env] erase all variable *)
let clear_all_variables env = {env with variables = []; nextvar = 2}

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
    let look x =
      let rec aux = function
        | [] -> -1
        | (n,y) :: tl when x=y -> n
        | _ :: tl -> aux tl
      in
      aux !allcodes                                           
    let encode lab =
      let n = !nextcode in
      incr nextcode;
      allcodes := (n,lab) :: !allcodes;
      n
    let all_encodings () = !allcodes
    let lookup_lbl_addr lbl =
      List.(assoc lbl (map (fun (x,y) -> (y,x)) (all_encodings ())))
  end


(** [clean p] clean a jakix program *)
let rec clean = function
  | []
  | [_] as instr -> instr
  | (lbl,T.Aload n)::(_,T.Astore m)::tl when n = m ->
    tl |> rename_first_op2 lbl |> clean
  | (lbl,T.Box)::(_,T.Unbox)::tl ->
    tl |> rename_first_op2 lbl |> clean
  | h :: tl -> h :: clean tl

let no_lbl instr = [(None,instr)]
let with_lbl lbl i = [(Some lbl, i)]
let with_lbl_s str i = with_lbl (T.Label str) i

let push x     = no_lbl (T.Bipush x)
let load x     = no_lbl T.(Aload (Var x))
let store x    = no_lbl T.(Astore (Var x))
let goto lbl   = no_lbl (T.Goto lbl)

let box        = no_lbl T.Box
let unbox      = no_lbl T.Unbox
let swap       = no_lbl T.Swap
let checkarray = no_lbl T.Checkarray
let continue lbl = with_lbl lbl (T.Comment "continue")
let comment cmt =  no_lbl (T.Comment cmt)

let anew = no_lbl T.Anewarray @ checkarray

let aaload arr i =
  load arr @ checkarray @ push i @ no_lbl T.AAload
let aastoreN arr i x =
  load arr @ checkarray @ push i @ push x @ box @ no_lbl T.AAstore
let aastoreV arr i v =
  load arr @ checkarray @ push i @ load v @ no_lbl T.AAstore

let rec store_all min n =
  if n = min then []
  else store (n-1) @ store_all min (n-1)

let boxed = true
let unboxed = false

let box_it is_boxed instr_l =
  if is_boxed then instr_l
  else instr_l @ box

let unbox_it is_boxed instr_l =
  if is_boxed then instr_l @ unbox
  else instr_l

let as_def _ instr_l = instr_l

let binop_to_javix = FopixToJavix.binop_to_javix
let cmpop_to_javix = FopixToJavix.cmpop_to_javix

let dispatch_lbl = T.Label "dispatch"
let dispatch () =
  let lbl_err = T.Label "error" in
  let codes = List.(Labels.all_encodings () |> map snd |> rev)        in
  let tableinstr = no_lbl (T.Tableswitch (0, codes, lbl_err))  in
  let default = with_lbl_s "error" (T.Bipush (-1)) @ no_lbl T.Ireturn   in
  if codes = [] then []
  else rename_first_op dispatch_lbl tableinstr @ default

let return () =
  let lbl_return = T.Label "body_return"                              in
  let addr = Labels.encode lbl_return                                 in
  addr,
  (rename_first_op lbl_return (load 2)) @ unbox @ no_lbl T.Ireturn

(** [translate p env] translate a kontix program into a jakix program *)
let rec translate (p : S.t) (env0 : environment) : T.t * environment =
  let def0,expr0 = p                                                  in
  let env1 = bind_all_functions env0 def0                             in
  let def1 = List.(map (definition env1) def0 |> flatten)             in
  let addr_ret, ret = return ()                                       in
  let expr1 = tailexpr box_it env1 expr0                              in
  let disp = dispatch ()                                              in
  let pre =
    comment "store cont and env"
    @ push addr_ret @ box @ store 0 @ push (-1) @ box @ store 1
    @ comment "end store cont and env"
  in
  basic_program
    (pre @ expr1 @ ret @ def1 @ disp |> clean), env1

(** [definition env d] translate definition *)
and definition env = function
  | S.DefFun(f, formals, e) ->
    let fl = lookup_function_label f env                             in
    let env = clear_all_variables env                                in
    let env = bind_function_variables env formals                    in
    let fcode = tailexpr as_def env e                                in
    rename_first_op fl fcode

  | S.(DefCont(kid, karg, kenv, tail_e)) ->
    let klbl = lookup_function_label kid env in
    let env = clear_all_variables env in
    let env = bind_function_variables env (karg::kenv) in
    let f_instr = tailexpr as_def env tail_e in
    let aux x =  aaload 1 x @ store (x+1) in
    let ld = List.(map aux (range 2 (length kenv+1)) |> flatten) in
    rename_first_op klbl (ld @ aaload 1 1 @ aaload 1 0
    @ store 0 @ store 1 @ f_instr)

and tailexpr fun_box env = function
  | S.TDef(x,e,tl) ->
    let var, env = bind_variable env x in
    let e = basicexpr box_it env e in
    let first = e @ store var in
    let tl = tailexpr fun_box env tl in
    first @ tl

  | S.TContCall (cont_and_env, e) ->
    let e = basicexpr box_it env e in
    continuation env cont_and_env @
    e @ store 2 @ load 0 @ unbox @ goto dispatch_lbl

  | S.(TFunCall (FunName fid, args, cont_and_env)) ->
    let args_instr = function_arguments env args in
    continuation env cont_and_env @
    args_instr @ goto (lookup_function_label fid env)

  | S.TFunCall (fe, args, cont_and_env) ->
    let f_instr = basicexpr unbox_it env fe in
    let args_instr = function_arguments env args in
    continuation env cont_and_env
    @ args_instr @ f_instr @ goto dispatch_lbl

  | S.TIfThenElse (b, t, f) ->
    let tl = fresh_function_label "_true"                            in
    let ifb = tailexpr fun_box env t |> rename_first_op tl           in
    let elseb = tailexpr fun_box env f                               in
    let cond_instr = compile_cond_instr env tl b                     in
    cond_instr @ elseb @ ifb

and basicexpr fun_box env = function
  | S.Num i ->
    fun_box unboxed (push i)

  | S.Var x ->
    let v= lookup_variables x env in
    fun_box boxed (load v)

  | S.FunName f ->
    let fl = lookup_function_label f env                             in
    let addr = Labels.encode fl in
    fun_box unboxed (push addr)

  | S.( BinOp(Add, Num n1, Num n2)) ->
    basicexpr fun_box env (S.Num (n1+n2))

  | S.( BinOp(Mod|Add|Sub|Mul|Div as op, e1, e2)) ->
    let e1' = basicexpr unbox_it env e1                              in
    let e2' = basicexpr unbox_it env e2                              in
    let op = no_lbl (binop_to_javix op) in
    fun_box unboxed (e1' @ e2' @ op)

  | S.(BinOp(_)) as b -> (** when boolean binop *)
    basicexpr fun_box env S.(IfThenElse(b,Num 1,Num 0))

  | S.Def(x, e1, e2) ->
    let e1' = basicexpr box_it env e1                                in
    let var, env' = bind_variable env x                             in
    let e2' = basicexpr fun_box env' e2 in
    e1' @ store var @ e2'

  | S.IfThenElse(b,t,f) ->
    let tl = fresh_function_label "_true"                            in
    let nl = fresh_function_label "_next"                            in
    let ifb = basicexpr fun_box env t @ goto nl in
    let elseb = basicexpr fun_box env f @ goto nl                in
    let cond_instr = compile_cond_instr env tl b                     in
    cond_instr @ elseb @ (rename_first_op tl ifb) @ continue nl

  | S.BlockNew e ->
    let e' = basicexpr unbox_it env e                                in
    e' @ anew

  | S.BlockGet (e1, e2) ->
    let e1' = basicexpr box_it env e1 @ checkarray                   in
    let e2' = basicexpr unbox_it env e2                              in
    fun_box boxed (e1' @ e2' @ no_lbl T.AAload)

  | S.BlockSet (e1, e2, e3) ->
    let e1' = basicexpr box_it env e1 @ checkarray                   in
    let e2' = basicexpr unbox_it env e2                              in
    let e3' = basicexpr box_it env e3                                in
    fun_box unboxed (e1' @ e2' @ e3' @ no_lbl T.AAstore @ push 0)

and function_arguments env args =
  let len = List.length args in
  let args = List.(flatten (mapi (fun i e ->
      basicexpr box_it env e) args))
  in
  args @ store_all 2 (len+2)

and compile_cond_instr env lbl = function
  | S.(BinOp (Eq|Le|Lt|Ge|Gt as op, e1, e2)) ->
    let e1' = basicexpr unbox_it env e1                              in
    let e2' = basicexpr unbox_it env e2                              in
    e1' @ e2' @ no_lbl (cmpop_to_javix lbl op)
  | _ as e ->
    let e' = basicexpr unbox_it env e                                in
    e'@ push 1 @ no_lbl (cmpop_to_javix lbl S.Eq)

and continuation env = function
  | S.CurCont -> []
  | S.PushCont(kont, cont_and_env, m) ->
    let n = env.nextvar                                              in
    let kont_addr = Labels.encode (lookup_function_label kont env ) in
    let array0 = push (List.length m + 2) @ anew @ store n             in
    let array1 = aastoreV n 0 0                                       in
    let array2 = aastoreV n 1 1                                       in
    let aux v i = aastoreV n i (lookup_variables v env)               in
    let array3 = List.(map2 aux m (range 2 (length m+1)) |> flatten) in
    array0 @ array1 @ array2 @ array3
    @ push kont_addr @ box @ store 0
    @ load n @ store 1
    @ continuation env cont_and_env
