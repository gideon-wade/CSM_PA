module Interpreter

let rec replace (x : float list) (idx : int) (value : float) (curIdx : int) = 
    match x with
        | []                         -> []
        | e::tail when idx = curIdx  -> value::tail
        | e::tail when idx <> curIdx -> e::(replace tail idx value (curIdx+1))
        | _                          -> [];;
        
let rec evalA (e : aExpr) (varMem : Map<string, float>) (arrMem : Map<string, float list>) =
    match e with
        | Num(x)              -> x
        | ArrayVariable(x, a) -> (arrMem.[x]).[(int) (evalA a varMem arrMem)]
        | Variable(x)         -> varMem.[x]
        | TimesExpr(x,y)      -> (evalA x varMem arrMem) * (evalA y varMem arrMem)
        | DivExpr(x,y)        -> (evalA x varMem arrMem) / (evalA y varMem arrMem)
        | PlusExpr(x,y)       -> (evalA x varMem arrMem) + (evalA y varMem arrMem)
        | MinusExpr(x,y)      -> (evalA x varMem arrMem) - (evalA y varMem arrMem)
        | PowExpr(x,y)        -> (evalA x varMem arrMem) ** (evalA y varMem arrMem)
        | UPlusExpr(x)        -> (evalA x varMem arrMem)
        | UMinusExpr(x)       -> -(evalA x varMem arrMem);;

let rec evalB (bexpr : bExpr) (varMem : Map<string, float>) (arrMem : Map<string, float list>) =
    match bexpr with
        | True              -> true
        | False             -> false
        | LOr(b1, b2)       -> let b1 = (evalB b1 varMem arrMem)
                               let b2 = (evalB b2 varMem arrMem)
                               b1 || b2
        | LAnd(b1, b2)      -> let b1 = (evalB b1 varMem arrMem)
                               let b2 = (evalB b2 varMem arrMem)
                               b1 && b2
        | Or(b1, b2)        -> (evalB b1 varMem arrMem) || (evalB b2 varMem arrMem)
        | And(b1, b2)       -> (evalB b1 varMem arrMem) && (evalB b2 varMem arrMem)
        | Not(b)            -> not (evalB b varMem arrMem)
        | Equals(a1, a2)    -> (evalA a1 varMem arrMem) =  (evalA a2 varMem arrMem)
        | NotEquals(a1, a2) -> (evalA a1 varMem arrMem) <> (evalA a2 varMem arrMem)
        | GrtThan(a1, a2)   -> (evalA a1 varMem arrMem) >  (evalA a2 varMem arrMem)
        | GrtEq(a1, a2)     -> (evalA a1 varMem arrMem) >= (evalA a2 varMem arrMem)
        | LeThan(a1, a2)    -> (evalA a1 varMem arrMem) <  (evalA a2 varMem arrMem)
        | LeEq(a1, a2)      -> (evalA a1 varMem arrMem) <= (evalA a2 varMem arrMem);;  

let rec evalCmd (cmd:cmd) (varMem : Map<string, float>) (arrMem : Map<string, float list>) =
    match cmd with
        | Skip                   -> (varMem, arrMem)
        | Assign(x, a)           -> (varMem.Add(x, (evalA a varMem arrMem)), arrMem)
        | AssignArray(x, a1, a2) -> (varMem, arrMem.Add(x, replace (arrMem.[x]) ((int) (evalA a1 varMem arrMem)) (evalA a2 varMem arrMem) 0))
        | Cmds(c1, c2)           -> let (varMem, arrMem) = (evalCmd c1 varMem arrMem)
                                    evalCmd c2 varMem arrMem
        | BoolCmd(_)             -> (varMem, arrMem)
        | _                      -> failwith ("Wrong parse" + (sprintf "%A" cmd))
and evalGCmd (gCmd:gCmd) (varMem : Map<string, float>) (arrMem : Map<string, float list>) =
    match gCmd with
        | Statement(bExpr, cmd) -> if (evalB bExpr varMem arrMem) then
                                        (evalCmd cmd varMem arrMem)
                                   else
                                        (varMem, arrMem)
        | GCmds(gc1, gc2) -> match gc1 with
                                | Statement(bExpr, c1) -> 
                                        if (evalB bExpr varMem arrMem) then
                                            evalCmd c1 varMem arrMem
                                        else
                                            (evalGCmd gc2 varMem arrMem)
                                | GCmds(gc1, gc2) -> failwith ("Wrong parse");;
//O(n)
let rec getValid (all : Edge list) (varMem :  Map<string, float>) (arrMem : Map<string, float list>) = 
    match all with
        | []                                                            -> []
        | (q1, BoolCmd(b), q2)::tail when evalB b varMem arrMem = false -> getValid tail varMem arrMem
        | e::tail                                                       -> e::(getValid tail varMem arrMem);;
//O(n)
let rec findAll (node : Node) (programGraph : Edge list) = 
    match programGraph with
        | []                                -> []
        | (q1, e, q2)::tail when q1 <> node -> findAll node tail
        | (q1, e, q2)::tail when q1 = node  -> (q1, e, q2)::(findAll node tail)
        | _                                 -> [];;

//O(n^n^...^n)
let rec evalGraph (curNode : Node ) (programGraph : Edge list) (varMem : Map<string, float>) (arrMem : Map<string, float list>) =
    match curNode with
        | "q◀" -> (curNode, "Terminated", varMem, arrMem)
        | _    -> let all = findAll curNode programGraph
                  let valid = getValid all varMem arrMem
                  if List.length valid = 0 then
                    (curNode, "Stuck(No valid node)", varMem, arrMem)
                  else
                    let r = System.Random()
                    let i = r.Next(0, List.length valid)
                    let (_, edge, endNode) = valid.[i]
                    let (varMem, arrMem) = (evalCmd edge varMem arrMem)
                    try
                        evalGraph endNode programGraph varMem arrMem
                    with e ->
                        (curNode, "Stuck(Crash)" + (sprintf "%A" e), varMem, arrMem);;