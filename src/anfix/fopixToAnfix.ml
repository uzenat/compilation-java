(** Conversion from Fopix to Anfix *)

(** For the moment, we only handle Fopix code which is already
    in ANF form (i.e. all arguments of function call and binop
    are simple). This is used for parsing Anfix without defining
    a full parser (but reusing Fopix parser instead) *)

(** TODO: extend this code into a full Fopix to Anfix converter *)

module S=FopixAST
module T=AnfixAST
exception Not_simpl
        
type environment = unit
let initial_environment () = ()

type defs = (T.identifier * T.expression) list

(** [fresh_var ()] return an unique fresh variable *)
let fresh_var =
  let r = ref 0 in
  fun f -> incr r; "_v" ^ string_of_int !r

(** [define e f] return a constructor like Def(x, e, f x) 
    where x is a unique fresh variable *)
let define e f =
  let v = fresh_var () in
  S.(Def (v, e, f (Var v)))

(** [program l] translate a fopix program in a anfix program *)
let rec program l = List.map definition l

and definition = function
  | S.DefVal (i,e) -> T.DefVal (i,expr e)
  | S.DefFun (f,a,e) -> T.DefFun (f,a,expr e)
                      
and simplexpr : S.expression -> (T.simple,unit) T.expr =
  function
  | S.Num n -> T.Num n
  | S.FunName f -> T.FunName f
  | S.Var x -> T.Var x
  | e ->
     failwith ("This expression should be simple:" ^
                 FopixPrettyPrinter.(to_string expression e))

and is_simpl : S.expression -> bool = function
  | S.(Num _ | FunName _ | Var _) -> true 
  | _ -> false
       
and expr : S.expression -> T.expression = function
  | S.Num n -> T.E (T.Num n)
  | S.FunName f -> T.E (T.FunName f)
  | S.Var x -> T.E (T.Var x)
  
  | S.Def (x,S.Def(x',e1',e2'),e2) ->
     expr (S.Def(x', e1', S.Def(x, e2', e2)))
  
  | S.Def (x,e1,e2) ->
     let T.E(e1') = expr e1 in
     let T.E(e2') = expr e2 in
     T.E (T.Def (x, e1', e2', ()))

  (* cas if then else avec e1 simpl *)
  | S.IfThenElse (e1, e2, e3) when is_simpl e1 ->
     let se1 = simplexpr e1 in
     let T.E(e2') = expr e2 in
     let T.E(e3') = expr e3 in
     T.E (T.IfThenElse (se1, e2', e3'))

  (* cas if then else avec e1 pas simpl *)
  | S.IfThenElse (e1, e2, e3) ->
     let f = fun x -> S.IfThenElse(x, e2, e3) in
     expr (define e1 f)

  | S.BinOp (b, S.Def(x, e1', e2'), e2) ->
     expr (S.Def(x, e1', S.BinOp(b, e2', e2)))
  | S.BinOp (b, e1, S.Def(x, e1', e2')) ->
     expr (S.Def(x, e1', S.BinOp(b, e1, e2')))

  (* binop avec e1 et e2 simple *)
  | S.BinOp (b,e1,e2) when is_simpl e1 && is_simpl e2 ->
     let se1 = simplexpr e1 in
     let se2 = simplexpr e2 in
     T.E (T.BinOp (b,se1,se2))

  (* binop avec e1 ou e2 pas simpl *)
  |S.BinOp (b, e1, e2) ->
    if not (is_simpl e1) then 
      let f x = S.BinOp(b, x, e2) in
      expr (define e1 f)
    else
      let f x = S.BinOp(b, e1, x) in 
      expr (define e2 f)
     
  | S.FunCall(e,el) ->
     let rec flatten arg = function
       |[] -> S.FunCall(e, arg)
       | e::el when is_simpl e -> flatten (e::arg) el
       | S.Def(x, e1, e2)::el ->
          S.Def(x, e1, flatten (e2::arg) el)
       | e::el ->
          let f x = flatten (x::arg) el in
          define e f

     in
     (match flatten [] (List.rev el) with
      | S.FunCall (e,el) ->
         let se = simplexpr e in
         let sel = List.((map simplexpr el)) in
         T.E (T.FunCall (se,sel))
      | _ as e -> expr e)

  (* creation de bloc avec e une simple expression *)
  | S.BlockNew e when is_simpl e ->
     let se = simplexpr e in
     T.E (T.BlockNew se)

  (* creation de bloc avec e qui n'est pas une simple expression *)
  | S.BlockNew e ->
     let f x = S.BlockNew x in
     expr (define e f)

  (* accede a un block avec e1 et e2 qui est une simple expression *)
  | S.BlockGet (e1,e2) when is_simpl e1 && is_simpl e2 ->
     let se1 = simplexpr e1 in
     let se2 = simplexpr e2 in
     T.E (T.BlockGet (se1,se2))

  (* accede a un block avec e1 ou e2 pas une simple expression *)
  | S.BlockGet (e1,e2) ->
     if not (is_simpl e1) then
       let f x = S.BlockGet (x, e2) in
       expr (define e1 f)
     else
       let f x = S.BlockGet (e1, x) in
       expr (define e2 f) 

  (* modifie un block avec e1, e2 et e3 une simple expression *)
  | S.BlockSet (e1,e2,e3)
       when is_simpl e1 && is_simpl e2 && is_simpl e3 ->
     let se1 = simplexpr e1 in
     let se2 = simplexpr e2 in
     let se3 = simplexpr e3 in
     T.E (T.BlockSet (se1,se2,se3))

  (* modifie un block avec e1, e2, ou e3 pas une simple expression *)
  | S.BlockSet (e1, e2, e3) ->
     if not (is_simpl e1) then
       let f x = S.BlockSet (x, e2, e3) in
       expr (define e1 f)
     else if not (is_simpl e2) then
       let f x = S.BlockSet (e1, x, e3) in
       expr (define e1 f)
     else
       let f x = S.BlockSet (e1, e2, x) in
       expr (define e1 f)
