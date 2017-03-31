(** This module implements a compiler from Anfix to Kontix. *)

(** As in any module that implements {!Compilers.Compiler}, the source
    language and the target language must be specified. *)
module Source = Anfix
module S = Source.AST
module Target = Kontix
module T = Target.AST

type environment = T.continuation_and_env

let initial_environment () = T.CurCont

(** [fresh_function_identifier ()] return a new fresh
    function identifier *)
let fresh_function_identifier =
  let r = ref (-1) in
  fun () -> incr r; "kont" ^ string_of_int !r

(** [undress_defval v] get identifier and expression of a defVal *)
let undress_defval = function
  | S.DefVal(x, e) -> x, e
  | _ -> assert false

(** [undress_deffun v] get identifier and expression of a defFun *)
let undress_deffun = function
  | S.DefFun(f,formals,e) -> f,formals,e
  | _ -> assert false

(** [is_simpl e] test if e contains funcall *)
let rec is_simpl : type a. (a,unit) S.expr ->bool =
  function
  | S.Num _ -> true
  | S.FunName _ -> true
  | S.Var _ -> true
  | S.Def (x, e1, e2, _) ->
    let e1' = is_simpl e1                                            in
    let e2' = is_simpl e2                                            in
    e1' && e2'
  | S.IfThenElse(_,t,f) ->
    let t' = is_simpl t                                              in
    let f' = is_simpl f                                              in
    t' && f'
  | S.BinOp(_,e1,e2) ->
    let e1' = is_simpl e1                                            in
    let e2' = is_simpl e2                                            in
    e1' && e2'
  | S.BlockNew _ -> true
  | S.BlockSet _ -> true
  | S.BlockGet _ -> true
  | S.FunCall _ -> false

(** [free_variables set e] get the FV of e *)
let rec free_variables
  : type a.string list->(a,unit)S.expr->string list= fun set->
  function
  | S.Num _ -> []
  | S.FunName _ -> []
  | S.Var v when List.mem v set -> []
  | S.Var v -> [v]
  | S.Def(x,e1, e2,()) ->
    let set'= x :: set                                               in
    let fve1 = free_variables set e1                                 in
    let fve2 = free_variables set' e2                                in
    fve1 @ fve2
  | S.IfThenElse(b,t,f) ->
    let fvt = free_variables set t                                   in
    let fvf = free_variables set f                                   in
    fvt @ fvf
  | S.BinOp(_,e1,e2) ->
    let fve1 = free_variables set e1                                 in
    let fve2 = free_variables set e2                                 in
    fve1 @ fve2
  | S.BlockNew _ -> []
  | S.BlockGet (e,_) -> free_variables set e
  | S.BlockSet (e,_,_) -> free_variables set e
  | S.FunCall(_, le) ->
    List.(map (free_variables set) le |> flatten)

let split_program p =
  let f_part = function S.DefVal _ -> true | _ -> false in
  List.partition f_part p

let join_all_val val_l=
  let f_fold v (S.E next_v) =
    match v with
    |S.DefVal (v,S.E(e)) ->
      S.E(S.Def(v,e,next_v,()))
    | _ -> assert false
  in
  match val_l with
  | S.DefVal (_, e)::all_v ->
    List.fold_right f_fold all_v e
  | _ -> assert false

(** [preprocess p] merge all DefValue and put it at end
    exemple: val x = 1 val y = 2 val z = 3 become
    val res = val x = 1 in val y = 2 in val z = 3 in z end end end *)
let preprocess p =
  let val_l, fun_l = split_program p in
  (List.map undress_deffun fun_l), join_all_val val_l

let rec translate p env =
  let fun_l, S.E(res) = preprocess p in
  let kontv, v = tail_expression env res in
  let dfun = compile_funs fun_l in
  ((dfun @ kontv, v), env)

(** [compile_fun dfuns] compile anfix function *)
and compile_funs dfuns =
  let aux = fun (f,formals, S.E(e)) ->
    let kontL, fe =
      tail_expression (initial_environment ()) e        in
    T.(DefFun(f,formals,fe)) :: kontL
  in
  let dfuns0 = List.(map aux dfuns |> flatten)          in
  let f_part = function T.DefFun _ -> true | _  -> false in
  let fun_l,kont_l = List.partition f_part dfuns0 in
  fun_l @ kont_l

(** [tail_expression env e] translate an anfix expression with funcall
    into a kontix expression with continuation *)
and tail_expression :
  type a.environment -> (a,unit) S.expr->T.definition list * T.tailexpr =
  fun env -> function
    | S.FunCall(f, le) ->
      let f' = basic_expression f                                      in
      let le' = List.map basic_expression le
      in
      [], T.(TFunCall(f', le', env))

    | S.Def(x,e1,e2,_) when is_simpl e1 ->
      let e1' = basic_expression e1                                    in
      let de2, e2' = tail_expression env e2                            in
      de2, T.TDef(x, e1', e2')

    | S.Def(x,e1,e2,()) ->
      let kontN = fresh_function_identifier ()                         in
      let fv = free_variables [x] e2                                   in
      let env'= T.PushCont(kontN,env,fv)                               in
      let de1, e1' = tail_expression env' e1                           in
      let de2, e2' = tail_expression (initial_environment ()) e2       in
      let defKont=T.(DefCont(kontN,x,fv,e2'))                          in
      de2 @ de1 @ [defKont], e1'

    | S.IfThenElse(b, t, f) ->
      let b' = basic_expression b                                      in
      let dt, t' = tail_expression env t                               in
      let df, f' = tail_expression env f                               in
      dt @ df, T.TIfThenElse(b', t', f')

    | _  as e ->
      let e' = basic_expression e                                      in
      [], T.(TContCall(env, e'))

(** [basic_expression e] translate an anfix expression without funcall
    in to a kontix expression without continuation *)
and basic_expression :
  type a. (a,unit)S.expr->T.basicexpr = function
  | S.Num n -> T.Num n
  | S.FunName f -> T.FunName f
  | S.Var x -> T.Var x
  | S.Def (x, e1, e2, _) ->
    let e1' = basic_expression e1                                    in
    let e2' = basic_expression e2                                    in
    T.Def(x, e1', e2')
  | S.IfThenElse(b, t, f) ->
    let b' = basic_expression b                                      in
    let t' = basic_expression t                                      in
    let f' = basic_expression f                                      in
    T.IfThenElse(b',t',f')
  | S.BinOp(b, e1, e2) ->
    let e1' = basic_expression e1                                    in
    let e2' = basic_expression e2                                    in
    T.BinOp(b, e1', e2')
  | S.BlockNew e ->
    let e' = basic_expression e                                      in
    T.BlockNew e'
  | S.BlockSet (e1, e2, e3) ->
    let e1' = basic_expression e1                                    in
    let e2' = basic_expression e2                                    in
    let e3' = basic_expression e3                                    in
    T.BlockSet(e1', e2', e3')
  | S.BlockGet(e1, e2) ->
    let e1' = basic_expression e1                                    in
    let e2' = basic_expression e2                                    in
    T.BlockGet(e1', e2')

  | _ -> assert false (* impossible : no funcall *)
