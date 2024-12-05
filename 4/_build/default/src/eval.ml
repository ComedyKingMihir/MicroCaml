open Types

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v) :: env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0)) :: env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then value := v else update t x v

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning an expression, or throwing an exception on error *)
let rec eval_expr env e =
  match e with
  | Int i -> Int i 
  | Bool b -> Bool b
  | String s -> String s
  | ID id -> lookup env id
  | Fun (param, body) -> Closure (env, param, body)
  | Not expr ->
      let res = eval_expr env expr in
      (match res with
      | Bool b -> Bool (not b)
      | _ -> raise (TypeError "Expected boolean"))
  | Binop (op, e1, e2) ->
      let e1' = eval_expr env e1 in
      let e2' = eval_expr env e2 in
      (match (op, e1', e2') with
      | Or, Bool a, Bool b -> Bool (a || b)
      | And, Bool a, Bool b -> Bool (a && b)
      | Equal, a, b -> Bool (a = b)
      | NotEqual, a, b -> Bool (a <> b)
      | Greater, Int a, Int b -> Bool (a > b)
      | Less, Int a, Int b -> Bool (a < b)
      | GreaterEqual, Int a, Int b -> Bool (a >= b)
      | LessEqual, Int a, Int b -> Bool (a <= b)
      | Add, Int a, Int b -> Int (a + b)
      | Sub, Int a, Int b -> Int (a - b)
      | Mult, Int a, Int b -> Int (a * b)
      | Div, Int a, Int b -> if b = 0 then raise DivByZeroError else Int (a / b)
      | Concat, String a, String b -> String (a ^ b)
      | _ -> raise (TypeError "Invalid binary operation"))
  | If (cond, then_branch, else_branch) ->
      (match eval_expr env cond with
      | Bool true -> eval_expr env then_branch
      | Bool false -> eval_expr env else_branch
      | _ -> raise (TypeError "Condition is not a boolean"))
  | Record fields -> Record (List.map (fun (label, expr) -> (label, eval_expr env expr)) fields)
  | Select (label, record_expr) ->
      let rec find_label fields = match fields with
        | [] -> raise (SelectError "Label not found")
        | (l, v) :: rest -> if l = label then v else find_label rest
      in
      let record_val = eval_expr env record_expr in
      (match record_val with
      | Record fields -> find_label fields
      | _ -> raise (TypeError "Not a record"))
  | Let (name, is_rec, expr1, expr2) ->
      if is_rec then
        let env' = extend_tmp env name in
        let value = eval_expr env' expr1 in
        update env' name value;
        eval_expr env' expr2
      else 
        let value = eval_expr env expr1 in
        eval_expr (extend env name value) expr2
  | App (func, arg) ->
      let arg_val = eval_expr env arg in
      (match eval_expr env func with
      | Closure (env', param, body) -> eval_expr (extend env' param arg_val) body
      | _ -> raise (TypeError "Expected a function"))
  | _ -> raise (TypeError "Invalid expression")

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m =
  match m with
  | Def (v, e) ->
      let env' = extend_tmp env v in
      let value = eval_expr env' e in
      update env' v value;
      (env', Some value)
  | Expr e -> (env, Some (eval_expr env e))
  | NoOp -> (env, None)
