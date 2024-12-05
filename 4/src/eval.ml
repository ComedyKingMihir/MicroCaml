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

(* Evaluates a MicroCaml expression [e] in the provided environment [env],
   returning the resulting expression, or raising an exception on error *)
let rec eval_expr env e =

  (* Helper function to retrieve type name as a string *)
  let type_of expr =
    match expr with
    | Int _ -> "int"
    | Bool _ -> "bool"
    | String _ -> "string"
    | Closure _ -> "function"
    | Record _ -> "record"
    | _ -> "unknown"
  in

  match e with
  | Int i -> e  (* Return integer values directly *)
  | Bool b -> e  (* Return boolean values directly *)
  | String s -> e  (* Return string values directly *)
  | ID id -> lookup env id  (* Retrieve the value of an identifier from the environment *)
  | Fun (param, body) -> Closure (env, param, body)  (* Create a closure for a function definition *)
  
  (* Evaluate 'not' expressions *)
  | Not inner_expr ->
    let res = eval_expr env inner_expr in
    (match res with
    | Bool b -> Bool (not b)  (* Negate boolean values *)
    | _ -> raise (TypeError ("Expected boolean for 'not', got " ^ type_of res)))  (* Error if not boolean *)

  (* Evaluate binary operations *)
  | Binop (op, left_expr, right_expr) ->
    let left_val = eval_expr env left_expr in
    let right_val = eval_expr env right_expr in
    (match (op, left_val, right_val) with
    | Or, Bool a, Bool b -> Bool (a || b)  (* Logical OR operation *)
    | And, Bool a, Bool b -> Bool (a && b)  (* Logical AND operation *)
    | Equal, val1, val2 -> Bool (val1 = val2)  (* Equality check *)
    | NotEqual, val1, val2 -> Bool (val1 <> val2)  (* Inequality check *)
    | Greater, Int a, Int b -> Bool (a > b)  (* Greater-than comparison *)
    | Less, Int a, Int b -> Bool (a < b)  (* Less-than comparison *)
    | GreaterEqual, Int a, Int b -> Bool (a >= b)  (* Greater-than-or-equal comparison *)
    | LessEqual, Int a, Int b -> Bool (a <= b)  (* Less-than-or-equal comparison *)
    | Add, Int a, Int b -> Int (a + b)  (* Integer addition *)
    | Sub, Int a, Int b -> Int (a - b)  (* Integer subtraction *)
    | Mult, Int a, Int b -> Int (a * b)  (* Integer multiplication *)
    | Div, Int a, Int b -> 
        if b = 0 then raise DivByZeroError else Int (a / b)  (* Division with zero check *)
    | Concat, String a, String b -> String (a ^ b)  (* String concatenation *)
    | _ -> raise (TypeError ("Invalid operation: " ^ type_of left_val ^ " and " ^ type_of right_val)))  (* Error for invalid types *)

  (* Evaluate 'if' expressions *)
  | If (cond_expr, then_branch, else_branch) ->
    (match eval_expr env cond_expr with
    | Bool true -> eval_expr env then_branch  (* Evaluate 'then' branch if condition is true *)
    | Bool false -> eval_expr env else_branch  (* Evaluate 'else' branch if condition is false *)
    | other -> raise (TypeError ("Expected boolean in 'if' condition, got " ^ type_of other)))  (* Error if not boolean *)

  (* Evaluate record expressions *)
  | Record fields ->
    Record (List.map (fun (label, field_expr) -> (label, eval_expr env field_expr)) fields)  (* Map and evaluate each field *)

  (* Select a value from a record by label *)
  | Select (label, record_expr) ->
    let rec find_label_in_record fields = match fields with
      | [] -> raise (SelectError "Label not found")  (* Error if label is missing *)
      | (current_label, value) :: remaining_fields -> 
          if current_label = label then value else find_label_in_record remaining_fields
    in
    let record_val = eval_expr env record_expr in
    (match record_val with
    | Record fields -> find_label_in_record fields  (* Retrieve field value by label *)
    | other -> raise (TypeError ("Expected record for 'select', got " ^ type_of other)))  (* Error if not a record *)

  (* Evaluate 'let' expressions, with optional recursion *)
  | Let (name, is_recursive, defining_expr, body_expr) ->
    if is_recursive then
      (* For recursive definitions, create a temporary environment binding for [name] *)
      let temp_env = extend_tmp env name in
      let defining_value = eval_expr temp_env defining_expr in
      update temp_env name defining_value;  (* Update temporary environment with evaluated value *)
      eval_expr temp_env body_expr  (* Evaluate body in the updated environment *)
    else 
      let defining_value = eval_expr env defining_expr in
      eval_expr (extend env name defining_value) body_expr  (* Non-recursive let; extend environment directly *)

  (* Evaluate function applications *)
  | App (func_expr, arg_expr) ->
    let arg_val = eval_expr env arg_expr in
    (match eval_expr env func_expr with
    | Closure (closure_env, parameter, body) -> 
        eval_expr (extend closure_env parameter arg_val) body  (* Evaluate with extended environment *)
    | other -> raise (TypeError ("Expected function for 'apply', got " ^ type_of other)))  (* Error if not a function *)

  (* Handle unexpected expressions by raising a TypeError *)
  | other -> raise (TypeError ("Invalid expression: " ^ type_of other))

(* Part 2: Evaluating mutop directive *)

(* Evaluates a MicroCaml mutop directive [m] in environment [env],
 returning a possibly updated environment paired with a value option; raises an exception on error *)
let eval_mutop env m =
  match m with
  | Def (var_name, expr) ->
    (* For 'def' directives, extend the environment temporarily and evaluate the expression *)
    let temp_env = extend_tmp env var_name in
    let evaluated_value = eval_expr temp_env expr in
    update temp_env var_name evaluated_value;  (* Update environment with the evaluated definition *)
    (temp_env, Some evaluated_value)  (* Return updated environment and the evaluated value *)
  | Expr expr -> 
    (env, Some (eval_expr env expr))  (* For expressions, evaluate and return the result with current environment *)
  | NoOp -> (env, None)  (* For NoOp, return the current environment unchanged, with no value *)