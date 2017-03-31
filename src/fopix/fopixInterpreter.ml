open Error
open FopixAST

(** [error pos msg] reports runtime error messages. *)
let error positions msg =
  errorN "execution" positions msg

(** Every expression of fopi evaluates into a [value]. *)
type value =
  | VUnit
  | VInt      of int
  | VBool     of bool
  | VLocation of Memory.location
  | VFun      of function_identifier
               
let print_value = function
  | VInt x      -> string_of_int x
  | VBool true  -> "true"
  | VBool false -> "false"
  | VUnit       -> "()"
  | VLocation l -> Memory.print_location l
  | VFun f      -> f

type 'a coercion = value -> 'a option
let value_as_int      = function VInt x -> Some x | _ -> None
let value_as_bool     = function VBool x -> Some x | _ -> None
let value_as_location = function VLocation x -> Some x | _ -> None
let value_as_unit     = function VUnit -> Some () | _ -> None

type 'a wrapper = 'a -> value
let int_as_value x  = VInt x
let bool_as_value x = VBool x
let location_as_value x = VLocation x
let unit_as_value () = VUnit

(** Binary operators *)

let lift_binop coerce wrap op v1 v2 =
  match coerce v1, coerce v2 with
  | Some li, Some ri -> Some (wrap (op li ri))
  | _, _ -> None

let lift_arith_op op = lift_binop value_as_int int_as_value op
let lift_cmp_op op = lift_binop value_as_int bool_as_value op

let arith_op_of_symbol = function
  | Add -> ( + )
  | Sub -> ( - )
  | Div -> ( / )
  | Mul -> ( * )
  | Mod -> ( mod )
  | _ -> assert false

let cmp_op_of_symbol = function
  | Lt -> ( < )
  | Gt -> ( > )
  | Le -> ( <= )
  | Ge -> ( >= )
  | Eq -> ( = )
  | _ -> assert false

let evaluation_of_binary_symbol = function
  | (Add|Sub|Mul|Div|Mod) as s -> lift_arith_op (arith_op_of_symbol s)
  | (Lt|Gt|Le|Ge|Eq) as s -> lift_cmp_op (cmp_op_of_symbol s)

(** Execution environment *)

module Environment : sig
  type t
  val initial : t
  val bind    : t -> identifier -> value -> t
  exception UnboundIdentifier of identifier
  val lookup  : identifier -> t -> value
  val last    : t -> (identifier * value * t) option
  val print   : t -> string
end = struct
  type t = (identifier * value) list

  let initial = []

  let bind e x v = (x, v) :: e

  exception UnboundIdentifier of identifier

  let lookup x e =
    try
      List.assoc x e
    with Not_found ->
      raise (UnboundIdentifier x)

  let last = function
    | [] -> None
    | (x, v) :: e -> Some (x, v, e)

  let print_binding (x, v) =
    (* Identifiers starting with '_' are reserved for the compiler.
       Their values must not be observable by users. *)
    if x <> "_" && x.[0] = '_' then
      ""
    else
      x ^ " = " ^ print_value v

  let print env =
    String.concat "\n" (
      List.(filter (fun s -> s <> "") (map print_binding env))
    )

end

type runtime = {
    environment : Environment.t;
    functions : (function_identifier * (formals * expression)) list;
  }

type observable = {
  new_environment : Environment.t;
}

let initial_runtime () = {
    environment = Environment.initial;
    functions = [];
}

(** 640k ought to be enough for anybody -- B.G. *)
let memory : value Memory.t = Memory.create (640 * 1024)
                            
let rec evaluate runtime ast =
  let runtime' = List.fold_left declaration runtime ast in
  (runtime', extract_observable runtime runtime')


and declaration runtime = function
  | DefVal (i, e) ->
    let v = expression runtime e in
    { environment = Environment.bind runtime.environment i v ;
      functions = runtime.functions }
  | DefFun (f,xs,e) ->
    { runtime with functions = (f, (xs, e)) :: runtime.functions }

and expression runtime = function
  | Num n -> VInt n

  | FunName f -> VFun f

  | Var x -> Environment.lookup x runtime.environment

  | Def (x, ex, e) ->
     let v = expression runtime ex in
     let runtime =
       { environment = Environment.bind runtime.environment x v ;
       functions = runtime.functions }
     in
     expression runtime e
     
  | IfThenElse (c, t, f) ->
     begin
       match expression runtime c with
       | VBool true -> expression runtime t
       | VBool false -> expression runtime f
       | _ -> assert false (* by typing *)
     end

  | BinOp (op, e1, e2) ->
     binop runtime op e1 e2

  | BlockNew e ->
     let size = eval_int runtime e                           in
     let loc = Memory.allocate memory size VUnit             in
     VLocation loc

  | BlockGet (e1, e2) ->
     let loc = eval_loc runtime e1                           in
     let index = eval_int runtime e2                         in
     let bloc = Memory.dereference memory loc                in
     Memory.read bloc index

  | BlockSet (e1, e2, e3) ->
     let loc = eval_loc runtime e1                           in
     let index = eval_int runtime e2                         in
     let bloc = Memory.dereference memory loc                in
     let _ = Memory.write bloc index (expression runtime e3) in
     VUnit

     
  | FunCall (fexpr, args) ->
     begin
       match expression runtime fexpr with
       | VFun f ->
          let (formals, content)= lookup_fun f runtime
          in
          let args' =
            List.map2 (fun x y -> (x, expression runtime y)) formals args
          in
          let environment' =
            List.fold_left (fun a (x,e) ->
                Environment.bind a x e) runtime.environment args'
          in
          let runtime'= { runtime with environment = environment' } in
          expression runtime' content
       | _ -> assert false (* by typing *)
     end

and binop runtime op e1 e2 =
  let v1 = expression runtime e1 in
  let v2 = expression runtime e2 in
  match evaluation_of_binary_symbol op v1 v2 with
  | Some v -> v
  | None -> error [] "Invalid binary operation."

and eval_loc runtime e =
  match expression runtime e with
  | VLocation loc -> loc
  | _ -> assert false (* by typing *)

and eval_int runtime e =
  match expression runtime e with
  | VInt i -> i
  | _ -> assert false (* by typing *)
    
and lookup_fun x env = List.assoc x env.functions

and extract_observable runtime runtime' =
  let rec substract new_environment env env' =
    if env == env' then new_environment
    else
      match Environment.last env' with
        | None -> assert false (* Absurd. *)
        | Some (x, v, env') ->
          let new_environment = Environment.bind new_environment x v in
          substract new_environment env env'
  in
  {
    new_environment =
      substract Environment.initial runtime.environment runtime'.environment
  }

let print_observable runtime observation =
  Environment.print observation.new_environment
