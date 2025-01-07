module AST = struct
  type program = statement list [@@deriving show]

  and statement =
    | Let of string * expression
    | Return of expression
    | Expression of expression
    | Block of statement list

  and expression =
    | Identifier of string
    | Integer of int64
    | Prefix of string * expression
    | Infix of expression * string * expression
    | Boolean of bool
    | If of expression * statement * statement option
    | Function of expression list * statement
    | Call of expression * expression list

  let to_string (program : program) : string =
    let rec statement_to_string (s : statement) : string =
      match s with
      | Let (ident, expr) -> Printf.sprintf "let %s = %s;" ident (expression_to_string expr)
      | Return expr -> Printf.sprintf "return %s;" (expression_to_string expr)
      | Expression expr -> expression_to_string expr
      | Block stmts -> List.map statement_to_string stmts |> String.concat ""
    and expression_to_string (e : expression) : string =
      match e with
      | Identifier ident -> ident
      | Integer i -> Int64.to_string i
      | Prefix (op, expr) -> Printf.sprintf "(%s%s)" op (expression_to_string expr)
      | Infix (left, op, right) ->
          Printf.sprintf "(%s %s %s)" (expression_to_string left) op (expression_to_string right)
      | Boolean b ->
          if b then
            "true"
          else
            "false"
      | If (cond, cons, alt) ->
          Printf.sprintf "if %s %s%s" (expression_to_string cond) (block_to_string cons)
            (match alt with Some a -> Printf.sprintf " else %s" (block_to_string a) | None -> "")
      | Function (params, body) -> function_to_string params body
      | Call (expr, args) -> Printf.sprintf "%s(%s)" (expression_to_string expr) (args_to_string args)
    and block_to_string (block : statement) : string = statement_to_string block
    and function_to_string (params : expression list) (body : statement) : string =
      Printf.sprintf "fn (%s) %s"
        (String.concat ", " (List.map expression_to_string params))
        (block_to_string body)
    and args_to_string (args : expression list) : string =
      List.map expression_to_string args |> String.concat ", "
    in
    List.map statement_to_string program |> String.concat ""
end