module graphPrinter

let rec graphPrinter (cmd:cmd) =
    match cmd with
        | Skip                   -> "Skip"
        | Assign(x, a)           -> x + ":=" + (graphPrinterA a)
        | AssignArray(x, a1, a2) -> x+"["+(graphPrinterA a1)+"] :="+(graphPrinterA a2)
        | Cmds(c1, c2)           -> failwith "Should not happen"
        | If(gCmd)               -> "if " + (graphPrinterGc gCmd) + " fi"
        | Do(gCmd)               -> "do " + (graphPrinterGc gCmd) + " od"
        | BoolCmd(b)             -> graphPrinterB b
and graphPrinterA (aexpr : aExpr) =
    match aexpr with
        | Num(n)              -> sprintf "%i" ((int) n)
        | Variable(x)         -> x
        | ArrayVariable(x, e) -> x + "[" + (graphPrinterA e) + "]"
        | TimesExpr(x, y)     -> (graphPrinterA x) + "*" + (graphPrinterA y) 
        | DivExpr(x, y)       -> (graphPrinterA x) + "/" + (graphPrinterA y) 
        | PlusExpr(x, y)      -> (graphPrinterA x) + "+" + (graphPrinterA y) 
        | MinusExpr(x, y)     -> (graphPrinterA x) + "-" + (graphPrinterA y)
        | PowExpr(x, y)       -> (graphPrinterA x) + "^" + (graphPrinterA y)
        | UPlusExpr(x)        -> "+" + (graphPrinterA x)
        | UMinusExpr(x)       -> "-" + (graphPrinterA x)
and graphPrinterB (bexpr : bExpr) =
    match bexpr with
        | True              -> "True"
        | False             -> "False"
        | LOr(b1, b2)       -> "(" + (graphPrinterB b1) + "|" + (graphPrinterB b2) + ")"
        | LAnd(b1, b2)      -> "(" + (graphPrinterB b1) + "&" + (graphPrinterB b2) + ")"
        | Or(b1, b2)        -> "(" + (graphPrinterB b1) + "||" + (graphPrinterB b2) + ")"
        | And(b1, b2)       -> "(" + (graphPrinterB b1) + "&&" + (graphPrinterB b2) + ")"
        | Not(b)            -> "!(" + (graphPrinterB b) + ")"
        | Equals(a1, a2)    -> "(" + (graphPrinterA a1) + "=" + (graphPrinterA a2) + ")"
        | NotEquals(a1, a2) -> "(" + (graphPrinterA a1) + "!=" + (graphPrinterA a2) + ")"
        | GrtThan(a1, a2)   -> "(" + (graphPrinterA a1) + ">" + (graphPrinterA a2) + ")"
        | GrtEq(a1, a2)     -> "(" + (graphPrinterA a1) + ">=" + (graphPrinterA a2) + ")"
        | LeThan(a1, a2)    -> "(" + (graphPrinterA a1) + "<" + (graphPrinterA a2) + ")"
        | LeEq(a1, a2)      -> "(" + (graphPrinterA a1) + "<=" + (graphPrinterA a2) + ")"
and graphPrinterGc (gc : gCmd) =
    match gc with
        | Statement(bExpr, cmd) -> (graphPrinterB bExpr) + "->" + (graphPrinter cmd)
        | GCmds(gc1, gc2)       -> graphPrinterGc gc1 + " [] " + graphPrinterGc gc2;;  