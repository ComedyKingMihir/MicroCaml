open Types
open Str

(* Part 1: Lexer - Token Definitions *)
(* Regular expressions for each type of token in the language *)

let regex_left_parenthesis = Str.regexp "("
let regex_right_parenthesis = Str.regexp ")"
let regex_open_brace = Str.regexp "{"
let regex_close_brace = Str.regexp "}"
let regex_period = Str.regexp "\\."
let regex_equals = Str.regexp "="
let regex_not_equals = Str.regexp "<>"
let regex_greater_than = Str.regexp ">"
let regex_less_than = Str.regexp "<"
let regex_greater_than_equal = Str.regexp ">="
let regex_less_than_equal = Str.regexp "<="
let regex_logical_or = Str.regexp "||"
let regex_logical_and = Str.regexp "&&"
let regex_logical_not = Str.regexp "not"
let regex_if_keyword = Str.regexp "if"
let regex_then_keyword = Str.regexp "then"
let regex_else_keyword = Str.regexp "else"
let regex_addition = Str.regexp "+"
let regex_subtraction = Str.regexp "-"
let regex_multiplication = Str.regexp "*"
let regex_division = Str.regexp "/"
let regex_concatenation = Str.regexp "\\^"
let regex_let_keyword = Str.regexp "let"
let regex_definition = Str.regexp "def"
let regex_in_keyword = Str.regexp "in"
let regex_recursive = Str.regexp "rec"
let regex_function = Str.regexp "fun"
let regex_arrow = Str.regexp "->"
let regex_double_semicolon = Str.regexp ";;"
let regex_semicolon = Str.regexp ";"
let regex_integer = Str.regexp "[0-9]+"
let regex_negative_integer = Str.regexp "(-[0-9]+)"
let regex_boolean_true = Str.regexp "true"
let regex_boolean_false = Str.regexp "false"
let regex_string_literal = Str.regexp "\"[^\"]*\""
let regex_identifier = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let regex_whitespace = Str.regexp "[ \t\n]"

(* Main function to tokenize an input string into a list of tokens *)
let tokenize input = 
  (* Helper function to iterate through the input string and match tokens *)
  let rec tokenize_helper position input_string =
    (* Base case: If all characters have been processed, return an empty list *)
    if position >= String.length input_string then
      []
    (* Checks and matches each possible token in the input string starting at 'position' *)
    else if Str.string_match regex_arrow input_string position then
      Tok_Arrow :: tokenize_helper (position + 2) input_string
    else if Str.string_match regex_right_parenthesis input_string position then
      Tok_RParen :: tokenize_helper (position + 1) input_string
    else if Str.string_match regex_open_brace input_string position then
      Tok_LCurly :: tokenize_helper (position + 1) input_string
    else if Str.string_match regex_close_brace input_string position then
      Tok_RCurly :: tokenize_helper (position + 1) input_string
    else if Str.string_match regex_period input_string position then
      Tok_Dot :: tokenize_helper (position + 1) input_string
    else if Str.string_match regex_equals input_string position then
      Tok_Equal :: tokenize_helper (position + 1) input_string
    else if Str.string_match regex_not_equals input_string position then
      Tok_NotEqual :: tokenize_helper (position + 2) input_string
    else if Str.string_match regex_greater_than input_string position then
      Tok_Greater :: tokenize_helper (position + 1) input_string
    else if Str.string_match regex_greater_than_equal input_string position then
      Tok_GreaterEqual :: tokenize_helper (position + 2) input_string
    else if Str.string_match regex_less_than_equal input_string position then
      Tok_LessEqual :: tokenize_helper (position + 2) input_string
    else if Str.string_match regex_logical_or input_string position then
      Tok_Or :: tokenize_helper (position + 2) input_string
    else if Str.string_match regex_logical_and input_string position then
      Tok_And :: tokenize_helper (position + 2) input_string
    else if Str.string_match regex_logical_not input_string position then
      Tok_Not :: tokenize_helper (position + 3) input_string
    else if Str.string_match regex_if_keyword input_string position then
      Tok_If :: tokenize_helper (position + 2) input_string
    else if Str.string_match regex_then_keyword input_string position then
      Tok_Then :: tokenize_helper (position + 4) input_string
    else if Str.string_match regex_else_keyword input_string position then
      Tok_Else :: tokenize_helper (position + 4) input_string
    else if Str.string_match regex_negative_integer input_string position then
      (* Extract matched negative integer and convert to integer type *)
      let matched_str = Str.matched_string input_string in
      let token_length = String.length matched_str in
      let number_str = String.sub matched_str 1 (token_length - 2) in
      Tok_Int (int_of_string number_str) :: tokenize_helper (position + token_length) input_string
    else if Str.string_match regex_left_parenthesis input_string position then
      Tok_LParen :: tokenize_helper (position + 1) input_string
    else if Str.string_match regex_addition input_string position then
      Tok_Add :: tokenize_helper (position + 1) input_string
    else if Str.string_match regex_subtraction input_string position then
      Tok_Sub :: tokenize_helper (position + 1) input_string
    else if Str.string_match regex_division input_string position then
      Tok_Div :: tokenize_helper (position + 1) input_string
    else if Str.string_match regex_multiplication input_string position then
      Tok_Mult :: tokenize_helper (position + 1) input_string
    else if Str.string_match regex_concatenation input_string position then
      Tok_Concat :: tokenize_helper (position + 1) input_string
    else if Str.string_match regex_let_keyword input_string position then
      Tok_Let :: tokenize_helper (position + 3) input_string
    else if Str.string_match regex_definition input_string position then
      Tok_Def :: tokenize_helper (position + 3) input_string
    else if Str.string_match regex_in_keyword input_string position then
      Tok_In :: tokenize_helper (position + 2) input_string
    else if Str.string_match regex_recursive input_string position then
      Tok_Rec :: tokenize_helper (position + 3) input_string
    else if Str.string_match regex_function input_string position then
      Tok_Fun :: tokenize_helper (position + 3) input_string
    else if Str.string_match regex_double_semicolon input_string position then
      Tok_DoubleSemi :: tokenize_helper (position + 2) input_string
    else if Str.string_match regex_semicolon input_string position then
      Tok_Semi :: tokenize_helper (position + 1) input_string
    else if Str.string_match regex_integer input_string position then
      (* Extract matched integer and convert to integer type *)
      let matched_str = Str.matched_string input_string in
      let token_length = String.length matched_str in
      Tok_Int (int_of_string matched_str) :: tokenize_helper (position + token_length) input_string
    else if Str.string_match regex_boolean_true input_string position then
      Tok_Bool true :: tokenize_helper (position + 4) input_string
    else if Str.string_match regex_boolean_false input_string position then
      Tok_Bool false :: tokenize_helper (position + 5) input_string
    else if Str.string_match regex_string_literal input_string position then
      (* Extract matched string, removing surrounding quotes *)
      let matched_str = Str.matched_string input_string in
      let token_length = String.length matched_str in
      let string_content = String.sub matched_str 1 (token_length - 2) in
      Tok_String string_content :: tokenize_helper (position + token_length) input_string
    else if Str.string_match regex_identifier input_string position then
      (* Extract matched identifier *)
      let matched_str = Str.matched_string input_string in
      let token_length = String.length matched_str in
      Tok_ID matched_str :: tokenize_helper (position + token_length) input_string
    else if Str.string_match regex_whitespace input_string position then
      (* Skip whitespace and continue *)
      let matched_str = Str.matched_string input_string in
      let token_length = String.length matched_str in
      tokenize_helper (position + token_length) input_string
    else
      (* Raise error if unrecognized input is encountered *)
      raise (InvalidInputException "Unrecognized input")
  in
  (* Start the tokenization process from position 0 *)
  tokenize_helper 0 input
