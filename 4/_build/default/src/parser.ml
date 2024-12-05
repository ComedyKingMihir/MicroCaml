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

let rec parse_expr toks = 
  let (temp, modO) = parse_expr2 toks in
  if temp = [] then (temp, modO) else raise (InvalidInputException "none")

and parse_expr2 toks =
  match lookahead toks with 
  | Some Tok_Let -> parse_letExpr toks 
  | Some Tok_If -> parse_ifExpr toks
  | Some Tok_Fun -> parse_functionExpr toks
  | _ -> parse_orExpr toks

and parse_primaryExpr toks =  
  match lookahead toks with
  | Some (Tok_Int n) -> let temp = match_token toks (Tok_Int n) in (temp, Int n)
  | Some (Tok_Bool b) -> let temp = match_token toks (Tok_Bool b) in (temp, Bool b)
  | Some (Tok_String s) -> let temp = match_token toks (Tok_String s) in (temp, String s)
  | Some (Tok_ID id) -> let temp = match_token toks (Tok_ID id) in (temp, ID id)
  | Some Tok_LParen -> 
      let temp = match_token toks Tok_LParen in
      let (next, modO) = parse_expr2 temp in 
      let final = match_token next Tok_RParen in (final, modO)
  | _ -> parse_recordExpr toks

and parse_recordExpr toks = 
  let temp = match_token toks Tok_LCurly in
  let (next, modO) = parse_recordBodyExpr temp in 
  let final = match_token next Tok_RCurly in (final, modO)

and parse_recordBodyExpr toks = 
  match lookahead toks with
  | Some (Tok_ID id) -> 
      let temp = match_token toks (Tok_ID id) in
      let next = match_token temp Tok_Equal in
      let (final, modO) = parse_expr2 next in
      (match lookahead final with
       | Some Tok_Semi -> 
           let new_temp = match_token final Tok_Semi in
           let (last, modT) = parse_recordBodyExpr new_temp in
           (match modT with
            | Record lst -> (last, Record ((Lab id, modO) :: lst))
            | _ -> raise (InvalidInputException "failure"))
       | _ -> (final, Record [(Lab id, modO)]))
  | _ -> (toks, Record [])

and parse_unaryExpr toks = 
  match lookahead toks with
  | Some Tok_Not -> 
      let temp = match_token toks Tok_Not in 
      let (next, modO) = parse_unaryExpr temp in (next, Not modO)
  | _ -> parse_appExpr toks

and parse_appExpr toks = 
  let (temp, modO) = parse_selectExpr toks in
  match lookahead temp with
  | Some (Tok_Int _ | Tok_Bool _ | Tok_String _ | Tok_ID _ | Tok_LParen | Tok_LCurly) -> 
      let (next, modT) = parse_primaryExpr temp in (next, App (modO, modT))
  | _ -> (temp, modO) 

and parse_selectExpr toks =
  let (temp, modO) = parse_primaryExpr toks in
  if lookahead temp = Some Tok_Dot then 
    let next = match_token temp Tok_Dot in 
    match lookahead next with
    | Some (Tok_ID id) -> let final = match_token next (Tok_ID id) in (final, Select (Lab id, modO))
    | _ -> raise (InvalidInputException "N/A") 
  else (temp, modO)

and parse_letExpr toks =
  match lookahead toks with
  | Some Tok_Let -> 
      let temp = match_token toks Tok_Let in
      let (next, rec_tok) = parse_recursion temp in 
      (match lookahead next with
       | Some (Tok_ID id) -> 
           let mid = match_token next (Tok_ID id) in 
           let final = match_token mid Tok_Equal in 
           let (tempF, modO) = parse_expr2 final in 
           let after_in = match_token tempF Tok_In in
           let (tempS, modT) = parse_expr2 after_in in (tempS, Let (id, rec_tok, modO, modT))
       | _ -> raise (InvalidInputException "ID N/A"))
  | _ -> raise (InvalidInputException "redo")

and parse_recursion toks =
  match lookahead toks with
  | Some Tok_Rec -> let rest = match_token toks Tok_Rec in (rest, true)
  | _ -> (toks, false)

and parse_functionExpr toks =
  let temp = match_token toks Tok_Fun in
  match lookahead temp with 
  | Some Tok_ID id ->
      let next = match_token temp (Tok_ID id) in 
      let final = match_token next Tok_Arrow in
      let (tempF, modO) = parse_expr2 final in (tempF, Fun (id, modO))
  | _ -> raise (InvalidInputException "ID N/A")

and parse_ifExpr toks = 
  let temp = match_token toks Tok_If in 
  let (next, modO) = parse_expr2 temp in
  let mid = match_token next Tok_Then in 
  let (tempF, modT) = parse_expr2 mid in
  let final = match_token tempF Tok_Else in 
  let (tempS, modTh) = parse_expr2 final in (tempS, If (modO, modT, modTh))

and parse_orExpr toks = 
  let (temp, modO) = parse_andExpr toks in 
  match lookahead temp with
  | Some Tok_Or -> 
      let next = match_token temp Tok_Or in
      let (tempF, modT) = parse_orExpr next in (tempF, Binop (Or, modO, modT))
  | _ -> (temp, modO)

and parse_andExpr toks = 
  let (temp, modO) = parse_equalityExpr toks in
  match lookahead temp with 
  | Some Tok_And -> 
      let next = match_token temp Tok_And in
      let (tempF, modT) = parse_andExpr next in (tempF, Binop (And, modO, modT))
  | _ -> (temp, modO)

and parse_equalityExpr toks = 
  let (temp, modO) = parse_relationalExpr toks in
  match lookahead temp with 
  | Some Tok_Equal | Some Tok_NotEqual as op -> 
      let (next, eq_tok) = parse_equalityOperator temp in
      let (tempF, modT) = parse_equalityExpr next in (tempF, Binop (eq_tok, modO, modT)) 
  | _ -> (temp, modO)

and parse_equalityOperator toks =
  match lookahead toks with
  | Some Tok_Equal -> let temp = match_token toks Tok_Equal in (temp, Equal)
  | Some Tok_NotEqual -> let temp = match_token toks Tok_NotEqual in (temp, NotEqual)
  | _ -> raise (InvalidInputException "")

and parse_relationalExpr toks =
  let (temp, modO) = parse_additiveExpr toks in
  match lookahead temp with
  | Some Tok_Less | Some Tok_Greater | Some Tok_LessEqual | Some Tok_GreaterEqual as op -> 
      let (next, op_tok) = parse_relationalOperator temp in
      let (tempF, modT) = parse_relationalExpr next in (tempF, Binop (op_tok, modO, modT))
  | _ -> (temp, modO)

and parse_relationalOperator toks =
  match lookahead toks with
  | Some Tok_Less -> let temp = match_token toks Tok_Less in (temp, Less)
  | Some Tok_LessEqual -> let temp = match_token toks Tok_LessEqual in (temp, LessEqual)
  | Some Tok_Greater -> let temp = match_token toks Tok_Greater in (temp, Greater)
  | Some Tok_GreaterEqual -> let temp = match_token toks Tok_GreaterEqual in (temp, GreaterEqual)
  | _ -> raise (InvalidInputException "N/A")

and parse_additiveExpr toks = 
  let (temp, modO) = parse_multiplicativeExpr toks in
  match lookahead temp with 
  | Some Tok_Add | Some Tok_Sub as op -> 
      let (next, add_op) = parse_additiveOperator temp in 
      let (tempF, modT) = parse_additiveExpr next in (tempF, Binop (add_op, modO, modT)) 
  | _ -> (temp, modO)

and parse_additiveOperator toks =
  match lookahead toks with
  | Some Tok_Add -> let temp = match_token toks Tok_Add in (temp, Add)
  | Some Tok_Sub -> let temp = match_token toks Tok_Sub in (temp, Sub)
  | _ -> raise (InvalidInputException "N/A")

and parse_multiplicativeExpr toks = 
  let (temp, modO) = parse_concatExpr toks in
  match lookahead temp with 
  | Some Tok_Mult | Some Tok_Div as op -> 
      let (next, mult_op) = parse_multiplicativeOperator temp in
      let (tempF, modT) = parse_multiplicativeExpr next in (tempF, Binop (mult_op, modO, modT)) 
  | _ -> (temp, modO)

and parse_multiplicativeOperator toks =
  match lookahead toks with
  | Some Tok_Mult -> let temp = match_token toks Tok_Mult in (temp, Mult)
  | Some Tok_Div -> let temp = match_token toks Tok_Div in (temp, Div)
  | _ -> raise (InvalidInputException "invalid operator")

and parse_concatExpr toks = 
  let (temp, modO) = parse_unaryExpr toks in
  match lookahead temp with
  | Some Tok_Concat -> 
      let next = match_token temp Tok_Concat in 
      let (tempF, modT) = parse_concatExpr next in (tempF, Binop(Concat, modO, modT))
  | _ -> (temp, modO)

(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  match lookahead toks with
  | Some Tok_Def -> parse_def_mutop toks
  | Some Tok_DoubleSemi -> 
      let temp = match_token toks Tok_DoubleSemi in
      if lookahead temp = None then (temp, NoOp) 
      else raise (InvalidInputException "")
  | _ -> parse_expr_mutop toks

and parse_def_mutop toks =
  let temp = match_token toks Tok_Def in
  match lookahead temp with
  | Some (Tok_ID id) ->
      let next = match_token temp (Tok_ID id) in
      let mid = match_token next Tok_Equal in
      let (final, modO) = parse_expr2 mid in
      let final_next = match_token final Tok_DoubleSemi in
      (final_next, Def (id, modO))
  | _ -> raise (InvalidInputException "")

and parse_expr_mutop toks =
  let (temp, modO) = parse_expr2 toks in
  let final = match_token temp Tok_DoubleSemi in
  if lookahead final = None then (final, Expr modO)
  else raise (InvalidInputException "")
