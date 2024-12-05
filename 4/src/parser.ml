open Types
open Utils

(* Provided functions - DO NOT MODIFY *)

let match_token toks tok =
  match toks with
  | [] -> raise (InvalidInputException (string_of_token tok))
  | h :: t when h = tok -> t
  | h :: _ ->
      raise (InvalidInputException
               (Printf.sprintf "Expected %s from input %s, got %s"
                  (string_of_token tok)
                  (string_of_list string_of_token toks)
                  (string_of_token h)))

let match_many toks to_match = List.fold_left match_token toks to_match

let lookahead toks = match toks with [] -> None | h :: t -> Some h

let rec lookahead_many toks n =
  match (toks, n) with
  | h :: _, 0 -> Some h
  | _ :: t, n when n > 0 -> lookahead_many t (n - 1)
  | _ -> None

(* Part 2: Parsing expressions *)

(* Main function to parse an expression *)
let rec parse_expr toks = 
  let (remaining_tokens, parsed_expr) = parse_expr2 toks in
  if remaining_tokens = [] then (remaining_tokens, parsed_expr)
  else raise (InvalidInputException "none")

(* Parses different types of expressions based on the first token *)
and parse_expr2 toks =
  match lookahead toks with 
  | Some Tok_Let -> parse_letExpr toks      (* 'let' expression *)
  | Some Tok_If -> parse_ifExpr toks        (* 'if' expression *)
  | Some Tok_Fun -> parse_functionExpr toks (* 'fun' (function) expression *)
  | _ -> parse_orExpr toks                  (* Otherwise, parse as an 'or' expression *)

(* Parses primary expressions, including literals and identifiers *)
and parse_primaryExpr toks =  
  match lookahead toks with
  | Some (Tok_Int n) -> let remaining_tokens = match_token toks (Tok_Int n) in (remaining_tokens, Int n)
  | Some (Tok_Bool b) -> let remaining_tokens = match_token toks (Tok_Bool b) in (remaining_tokens, Bool b)
  | Some (Tok_String s) -> let remaining_tokens = match_token toks (Tok_String s) in (remaining_tokens, String s)
  | Some (Tok_ID id) -> let remaining_tokens = match_token toks (Tok_ID id) in (remaining_tokens, ID id)
  | Some Tok_LParen ->                     
      let remaining_tokens = match_token toks Tok_LParen in
      let (next_tokens, parsed_expr) = parse_expr2 remaining_tokens in 
      let tokens_after_match = match_token next_tokens Tok_RParen in (tokens_after_match, parsed_expr)
  | _ -> parse_recordExpr toks             

(* Parses record expressions that start with '{' and end with '}' *)
and parse_recordExpr toks = 
  let remaining_tokens = match_token toks Tok_LCurly in
  let (next_tokens, parsed_expr) = parse_recordBodyExpr remaining_tokens in 
  let tokens_after_match = match_token next_tokens Tok_RCurly in (tokens_after_match, parsed_expr)

(* Parses the body of a record expression containing key-value pairs *)
and parse_recordBodyExpr toks = 
  match lookahead toks with
  | Some (Tok_ID id) -> 
      let remaining_tokens = match_token toks (Tok_ID id) in
      let next_tokens = match_token remaining_tokens Tok_Equal in
      let (tokens_after_match, parsed_expr) = parse_expr2 next_tokens in
      (match lookahead tokens_after_match with
       | Some Tok_Semi ->                  
           let new_remaining_tokens = match_token tokens_after_match Tok_Semi in
           let (last_tokens, next_expr) = parse_recordBodyExpr new_remaining_tokens in
           (match next_expr with
            | Record lst -> (last_tokens, Record ((Lab id, parsed_expr) :: lst))
            | _ -> raise (InvalidInputException "failure"))
       | _ -> (tokens_after_match, Record [(Lab id, parsed_expr)]))
  | _ -> (toks, Record [])

(* Parses unary expressions like 'not' *)
and parse_unaryExpr toks = 
  match lookahead toks with
  | Some Tok_Not -> 
      let remaining_tokens = match_token toks Tok_Not in 
      let (next_tokens, parsed_expr) = parse_unaryExpr remaining_tokens in (next_tokens, Not parsed_expr)
  | _ -> parse_appExpr toks                  

(* Parses application expressions where a function is applied to arguments *)
and parse_appExpr toks = 
  let (remaining_tokens, parsed_expr) = parse_selectExpr toks in
  match lookahead remaining_tokens with
  | Some (Tok_Int _ | Tok_Bool _ | Tok_String _ | Tok_ID _ | Tok_LParen | Tok_LCurly) -> 
      let (next_tokens, next_expr) = parse_primaryExpr remaining_tokens in (next_tokens, App (parsed_expr, next_expr))
  | _ -> (remaining_tokens, parsed_expr)

(* Parses selection expressions, used to access fields in records *)
and parse_selectExpr toks =
  let (remaining_tokens, parsed_expr) = parse_primaryExpr toks in
  if lookahead remaining_tokens = Some Tok_Dot then 
    let next_tokens = match_token remaining_tokens Tok_Dot in 
    match lookahead next_tokens with
    | Some (Tok_ID id) -> let tokens_after_match = match_token next_tokens (Tok_ID id) in (tokens_after_match, Select (Lab id, parsed_expr))
    | _ -> raise (InvalidInputException "N/A") 
  else (remaining_tokens, parsed_expr)

(* Parses 'let' expressions, handling optional recursion *)
and parse_letExpr toks =
  match lookahead toks with
  | Some Tok_Let -> 
      let remaining_tokens = match_token toks Tok_Let in
      let (next_tokens, is_recursive) = parse_recursion remaining_tokens in 
      (match lookahead next_tokens with
       | Some (Tok_ID id) -> 
           let mid_tokens = match_token next_tokens (Tok_ID id) in 
           let tokens_after_match = match_token mid_tokens Tok_Equal in 
           let (final_tokens, parsed_expr) = parse_expr2 tokens_after_match in 
           let tokens_after_in = match_token final_tokens Tok_In in
           let (final_remaining_tokens, next_expr) = parse_expr2 tokens_after_in in (final_remaining_tokens, Let (id, is_recursive, parsed_expr, next_expr))
       | _ -> raise (InvalidInputException "ID N/A"))
  | _ -> raise (InvalidInputException "redo")

(* Checks for recursive 'let' expressions *)
and parse_recursion toks =
  match lookahead toks with
  | Some Tok_Rec -> let remaining_tokens = match_token toks Tok_Rec in (remaining_tokens, true)
  | _ -> (toks, false)

(* Parses function expressions that start with 'fun' *)
and parse_functionExpr toks =
  let remaining_tokens = match_token toks Tok_Fun in
  match lookahead remaining_tokens with 
  | Some Tok_ID id ->
      let next_tokens = match_token remaining_tokens (Tok_ID id) in 
      let tokens_after_match = match_token next_tokens Tok_Arrow in
      let (final_tokens, parsed_expr) = parse_expr2 tokens_after_match in (final_tokens, Fun (id, parsed_expr))
  | _ -> raise (InvalidInputException "ID N/A")

(* Parses 'if' expressions with 'then' and 'else' branches *)
and parse_ifExpr toks = 
  let remaining_tokens = match_token toks Tok_If in 
  let (next_tokens, parsed_expr) = parse_expr2 remaining_tokens in
  let mid_tokens = match_token next_tokens Tok_Then in 
  let (final_tokens, next_expr) = parse_expr2 mid_tokens in
  let tokens_after_else = match_token final_tokens Tok_Else in 
  let (final_remaining_tokens, else_expr) = parse_expr2 tokens_after_else in (final_remaining_tokens, If (parsed_expr, next_expr, else_expr))

(* Parses logical 'or' expressions *)
and parse_orExpr toks = 
  let (remaining_tokens, parsed_expr) = parse_andExpr toks in 
  match lookahead remaining_tokens with
  | Some Tok_Or -> 
      let next_tokens = match_token remaining_tokens Tok_Or in
      let (final_tokens, next_expr) = parse_orExpr next_tokens in (final_tokens, Binop (Or, parsed_expr, next_expr))
  | _ -> (remaining_tokens, parsed_expr)

(* Parses logical 'and' expressions *)
and parse_andExpr toks = 
  let (remaining_tokens, parsed_expr) = parse_equalityExpr toks in
  match lookahead remaining_tokens with 
  | Some Tok_And -> 
      let next_tokens = match_token remaining_tokens Tok_And in
      let (final_tokens, next_expr) = parse_andExpr next_tokens in (final_tokens, Binop (And, parsed_expr, next_expr))
  | _ -> (remaining_tokens, parsed_expr)

(* Parses equality expressions (e.g., '==' and '!=') *)
and parse_equalityExpr toks = 
  let (remaining_tokens, parsed_expr) = parse_relationalExpr toks in
  match lookahead remaining_tokens with 
  | Some Tok_Equal | Some Tok_NotEqual as op -> 
      let (next_tokens, eq_op) = parse_equalityOperator remaining_tokens in
      let (final_tokens, next_expr) = parse_equalityExpr next_tokens in (final_tokens, Binop (eq_op, parsed_expr, next_expr)) 
  | _ -> (remaining_tokens, parsed_expr)

(* Parses equality operators for use in equality expressions *)
and parse_equalityOperator toks =
  match lookahead toks with
  | Some Tok_Equal -> let remaining_tokens = match_token toks Tok_Equal in (remaining_tokens, Equal)
  | Some Tok_NotEqual -> let remaining_tokens = match_token toks Tok_NotEqual in (remaining_tokens, NotEqual)
  | _ -> raise (InvalidInputException "")

(* Parses relational expressions (e.g., '<', '>', '<=', '>=') *)
and parse_relationalExpr toks =
  let (remaining_tokens, parsed_expr) = parse_additiveExpr toks in
  match lookahead remaining_tokens with
  | Some Tok_Less | Some Tok_Greater | Some Tok_LessEqual | Some Tok_GreaterEqual as op -> 
      let (next_tokens, rel_op) = parse_relationalOperator remaining_tokens in
      let (final_tokens, next_expr) = parse_relationalExpr next_tokens in (final_tokens, Binop (rel_op, parsed_expr, next_expr))
  | _ -> (remaining_tokens, parsed_expr)

(* Parses relational operators for use in relational expressions *)
and parse_relationalOperator toks =
  match lookahead toks with
  | Some Tok_Less -> let remaining_tokens = match_token toks Tok_Less in (remaining_tokens, Less)
  | Some Tok_LessEqual -> let remaining_tokens = match_token toks Tok_LessEqual in (remaining_tokens, LessEqual)
  | Some Tok_Greater -> let remaining_tokens = match_token toks Tok_Greater in (remaining_tokens, Greater)
  | Some Tok_GreaterEqual -> let remaining_tokens = match_token toks Tok_GreaterEqual in (remaining_tokens, GreaterEqual)
  | _ -> raise (InvalidInputException "N/A")

(* Parses additive expressions (e.g., '+' and '-') *)
and parse_additiveExpr toks = 
  let (remaining_tokens, parsed_expr) = parse_multiplicativeExpr toks in
  match lookahead remaining_tokens with 
  | Some Tok_Add | Some Tok_Sub as op -> 
      let (next_tokens, add_op) = parse_additiveOperator remaining_tokens in 
      let (final_tokens, next_expr) = parse_additiveExpr next_tokens in (final_tokens, Binop (add_op, parsed_expr, next_expr)) 
  | _ -> (remaining_tokens, parsed_expr)

(* Parses addition and subtraction operators *)
and parse_additiveOperator toks =
  match lookahead toks with
  | Some Tok_Add -> let remaining_tokens = match_token toks Tok_Add in (remaining_tokens, Add)
  | Some Tok_Sub -> let remaining_tokens = match_token toks Tok_Sub in (remaining_tokens, Sub)
  | _ -> raise (InvalidInputException "N/A")

(* Parses multiplicative expressions (e.g., '*' and '/') *)
and parse_multiplicativeExpr toks = 
  let (remaining_tokens, parsed_expr) = parse_concatExpr toks in
  match lookahead remaining_tokens with 
  | Some Tok_Mult | Some Tok_Div as op -> 
      let (next_tokens, mult_op) = parse_multiplicativeOperator remaining_tokens in
      let (final_tokens, next_expr) = parse_multiplicativeExpr next_tokens in (final_tokens, Binop (mult_op, parsed_expr, next_expr)) 
  | _ -> (remaining_tokens, parsed_expr)

(* Parses multiplication and division operators *)
and parse_multiplicativeOperator toks =
  match lookahead toks with
  | Some Tok_Mult -> let remaining_tokens = match_token toks Tok_Mult in (remaining_tokens, Mult)
  | Some Tok_Div -> let remaining_tokens = match_token toks Tok_Div in (remaining_tokens, Div)
  | _ -> raise (InvalidInputException "invalid operator")

(* Parses concatenation expressions using the '^' operator *)
and parse_concatExpr toks = 
  let (remaining_tokens, parsed_expr) = parse_unaryExpr toks in
  match lookahead remaining_tokens with
  | Some Tok_Concat -> 
      let next_tokens = match_token remaining_tokens Tok_Concat in 
      let (final_tokens, next_expr) = parse_concatExpr next_tokens in (final_tokens, Binop(Concat, parsed_expr, next_expr))
  | _ -> (remaining_tokens, parsed_expr)

(* Part 3: Parsing mutop *)

(* Parses a mutop (top-level directive), which could be a definition, an expression, or a no-op *)
let rec parse_mutop toks = 
  match lookahead toks with
  | Some Tok_Def -> parse_def_mutop toks
  | Some Tok_DoubleSemi ->              
      let remaining_tokens = match_token toks Tok_DoubleSemi in
      if lookahead remaining_tokens = None then (remaining_tokens, NoOp) 
      else raise (InvalidInputException "")
  | _ -> parse_expr_mutop toks                    

(* Parses a 'def' mutop, which defines a value or function *)
and parse_def_mutop toks =
  let remaining_tokens = match_token toks Tok_Def in
  match lookahead remaining_tokens with
  | Some (Tok_ID id) ->                         
      let next_tokens = match_token remaining_tokens (Tok_ID id) in
      let mid_tokens = match_token next_tokens Tok_Equal in    
      let (tokens_after_def, parsed_expr) = parse_expr2 mid_tokens in     
      let tokens_after_semi = match_token tokens_after_def Tok_DoubleSemi in
      (tokens_after_semi, Def (id, parsed_expr))              
  | _ -> raise (InvalidInputException "")

(* Parses an expression mutop, evaluating it as an expression followed by ';;' *)
and parse_expr_mutop toks =
  let (remaining_tokens, parsed_expr) = parse_expr2 toks in
  let tokens_after_expr = match_token remaining_tokens Tok_DoubleSemi in 
  if lookahead tokens_after_expr = None then (tokens_after_expr, Expr parsed_expr)
  else raise (InvalidInputException "")