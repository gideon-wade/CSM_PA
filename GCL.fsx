// This script implements our interactive calculator

// We need to import a couple of modules, including the generated lexer and parser
open System.IO;;
#load "import.fsx"
#load "GCLTypesAST.fs"
open GCLTypesAST
#load "graphPrinter.fsx"
open graphPrinter

#load "Compiler.fsx"
open Compiler

//#load "printer.fsx"
//open printer
//#load "eval.fsx"
//open eval
open FSharp.Text.Lexing
open System
#load "GCLParser.fs"
open GCLParser
#load "GCLLexer.fs"
open GCLLexer

let rec replace (x : float list) (idx : int) (value : float) (curIdx : int) = 
    match x with
        | []                         -> []
        | e::tail when idx = curIdx  -> value::tail
        | e::tail when idx <> curIdx -> e::(replace tail idx value (curIdx+1));;
        
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
        | UMinusExpr(x)       -> - (evalA x varMem arrMem);;

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
        | _                      -> (varMem, arrMem)
        //| If(gCmd)               -> evalGCmd gCmd varMem arrMem
        //| Do(gCmd)               -> evalGCmd gCmd varMem arrMem;;
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
                                            //let newMem = evalCmd c1 varMem arrMem
                                            //evalCmd c1 newMem
                                        else
                                            (evalGCmd gc2 varMem arrMem)
                                | GCmds(gc1, gc2) -> failwith ("Wrong parse");;

let rec findValid (all : Edge list) (varMem :  Map<string, float>) (arrMem : Map<string, float list>) = 
    match all with
        | []                                                            -> []
        | (q1, BoolCmd(b), q2)::tail when evalB b varMem arrMem = false -> findValid tail varMem arrMem
        | e::tail                                                       -> e::(findValid tail varMem arrMem);;

let rec findAll (node : Node) (programGraph : Edge list) = 
    match programGraph with
        | []                                -> []
        | (q1, e, q2)::tail when q1 <> node -> findAll node tail
        | (q1, e, q2)::tail when q1 = node  -> (q1, e, q2)::(findAll node tail)

let rec evalGraph (curNode : Node ) (programGraph : Edge list) (varMem : Map<string, float>) (arrMem : Map<string, float list>) =
    match curNode with
        | "q◀" -> (curNode, "Terminated", varMem, arrMem)
        | _    -> let all = findAll curNode programGraph
                  let valid = findValid all varMem arrMem
                  if List.length valid = 0 then
                    (curNode, "Stuck(No valid node)", varMem, arrMem)
                  else
                    let r = System.Random()
                    let i = r.Next(0, List.length valid - 1)
                    let (_, edge, endNode) = valid.[i]
                    //printfn "EDGE: %A" edge
                    let (varMem, arrMem) = (evalCmd edge varMem arrMem)
                    try
                        evalGraph endNode programGraph varMem arrMem
                    with e ->
                        (curNode, "Stuck(Crash)", varMem, arrMem);;
        
// We He
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = GCLParser.start GCLLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr") 
    res

// We implement here the function that interacts with the user
let rec compute n =
    if n = 0 then
        printfn "Bye bye ♿"
    else
        printfn "Enter an expression: "
        try
        let mutable prog = ""
        // We parse the input string
        let mutable read = Console.ReadLine()
        while (read <> "") do
            prog <- prog + " " + read
            read <- Console.ReadLine()
        let e = parse (prog)
        
        //printfn "AST ☢ SHEEEEEEEEEEEEEEEEEEEEESH: %A" (e)
        // and print the result of evaluating it 
        //printfn "Result: %A" (evalCmd e Map.empty)
        //printfn "Result: %A" (printer e)
        let graph = (edges "q▷" "q◀" e)
        //printfn "Result:\n%A" e
        //printfn "Graph:\n\n"
        File.WriteAllText("graph.dot", (makeEdge (bubbleDown (bubbleSort graph (List.length graph)))))
        //let varMem = Map.empty.Add("x",4.0)
        //let arrMem = Map.empty.Add("X",[4.;4.;4.;4.0])
        let varMem = Map [("n",10.)]
        let arrMem = Map [("A",[6.;3.;2.;-4.;3.;7.;99.;69.;-69.;55555.])]
        let (node, status, newVarMem, newArrMem) = (evalGraph "q▷" graph varMem arrMem)
        printfn "Status: %s\nNode: %s\nMemory:\n%A\n%A" status node newVarMem newArrMem//(prettyPrintVar newVarMem) (prettyPrintArr newArrMem) 
        compute n
        with e -> compute (n-1)

// Start interacting with the user
compute 20;;
