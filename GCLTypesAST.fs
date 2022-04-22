// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module GCLTypesAST


type aExpr =
  | Num of float
  | ArrayVariable of (string * aExpr)
  | Variable of string
  | TimesExpr of (aExpr * aExpr)
  | DivExpr of (aExpr * aExpr)
  | PlusExpr of (aExpr * aExpr)
  | MinusExpr of (aExpr * aExpr)  
  | PowExpr of (aExpr * aExpr)
  | UPlusExpr of (aExpr)
  | UMinusExpr of (aExpr)

type bExpr =
  | True
  | False
  | LOr of (bExpr * bExpr)
  | LAnd of (bExpr * bExpr)
  | Or of (bExpr * bExpr)
  | And of (bExpr * bExpr)
  | Not of bExpr
  | Equals of (aExpr * aExpr)
  | NotEquals of (aExpr * aExpr)
  | GrtThan of (aExpr * aExpr)
  | GrtEq of (aExpr * aExpr)
  | LeThan of (aExpr * aExpr)
  | LeEq of (aExpr * aExpr)
  
  
type cmd =
  | Skip
  | BoolCmd of bExpr
  | Assign of (string * aExpr)
  | AssignArray of (string * aExpr * aExpr)
  | Cmds of (cmd * cmd)
  | If of gCmd
  | Do of gCmd
and gCmd =
  | Statement of (bExpr * cmd)
  | GCmds of (gCmd * gCmd)
  

