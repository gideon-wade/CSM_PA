module printer

let rec printer (cmd:cmd) =
    match cmd with
        | Skip                   -> "Skip"
        | Assign(x, a)           -> "Assign(" + x + ", " + (printA a) + ")"
        | AssignArray(x, a1, a2) -> "ArrayAssign(" + x+"["+(printA a1)+"], "+(printA a2)+")"
        | Cmds(c1, c2)           -> "Cmds(" + (printer c1) + ", " + (printer c2) + ")"
        | If(gCmd)               -> "IFFI(" + (printGc gCmd) + ")"
        | Do(gCmd)               -> "DOOD(" + (printGc gCmd) + ")"
and printA (aexpr : aExpr) =
    match aexpr with
        | Num(n)              -> sprintf "%i" ((int) n)
        | Variable(x)         -> "VAR("+x+")"
        | ArrayVariable(x, e) -> "ArrayVar(" + x + ", " + (printA e) + ")"
        | TimesExpr(x, y)     -> "Times(" + (printA x) + ", " + (printA y) + ")" 
        | DivExpr(x, y)       -> "Div(" + (printA x) + ", " + (printA y) + ")" 
        | PlusExpr(x, y)      -> "Plus(" + (printA x) + ", " + (printA y) + ")" 
        | MinusExpr(x, y)     -> "Minus(" + (printA x) + ", " + (printA y) + ")" 
        | PowExpr(x, y)       -> "Pow(" + (printA x) + ", " + (printA y) + ")" 
        | UPlusExpr(x)        -> "Pos(" + (printA x) + ")" 
        | UMinusExpr(x)       -> "Neg(" + (printA x) + ")" 
and printB (bexpr : bExpr) =
    match bexpr with
        | True              -> "True"
        | False             -> "False"
        | LOr(b1, b2)       -> "LOr(" + (printB b1) + ", " + (printB b2) + ")"
        | LAnd(b1, b2)      -> "LAnd(" + (printB b1) + ", " + (printB b2) + ")"
        | Or(b1, b2)        -> "Or(" + (printB b1) + ", " + (printB b2) + ")"
        | And(b1, b2)       -> "And(" + (printB b1) + ", " + (printB b2) + ")"
        | Not(b)            -> "Not(" + (printB b) + ")"
        | Equals(a1, a2)    -> "Eq(" + (printA a1) + ", " + (printA a2) + ")"
        | NotEquals(a1, a2) -> "NotEq(" + (printA a1) + ", " + (printA a2) + ")"
        | GrtThan(a1, a2)   -> "Grt(" + (printA a1) + ", " + (printA a2) + ")"
        | GrtEq(a1, a2)     -> "GrtEq(" + (printA a1) + ", " + (printA a2) + ")"
        | LeThan(a1, a2)    -> "Le(" + (printA a1) + ", " + (printA a2) + ")"
        | LeEq(a1, a2)      -> "LeEq(" + (printA a1) + ", " + (printA a2) + ")"
and printGc (gc : gCmd) =
    match gc with
        | Statement(bExpr, cmd) -> "Statement(" + (printB bExpr) + ", " + (printer cmd) + ")"
        | GCmds(gc1, gc2)       -> "GCmds(" + printGc gc1 + ", " + printGc gc2 + ")";;