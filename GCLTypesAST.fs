// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module GCLTypesAST


type expr =
  | Num of float
  | ArrayVariable of string * expr
  | Variable of string
  | TimesExpr of (expr * expr)
  | DivExpr of (expr * expr)
  | PlusExpr of (expr * expr)
  | MinusExpr of (expr * expr)  
  | PowExpr of (expr * expr)
  | UPlusExpr of (expr)
  | UMinusExpr of (expr)
  
type cmd =
  | Skip
  | Assign of (string * expr)
  | AssignArray of (string * expr * expr)
  | Cmds of (cmd * cmd)  //semicolon

