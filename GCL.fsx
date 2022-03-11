// This script implements our interactive calculator

// We need to import a couple of modules, including the generated lexer and parser
#load "import.fsx"
//#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
open FSharp.Text.Lexing
open System
#load "GCLTypesAST.fs"
open GCLTypesAST
#load "GCLParser.fs"
open GCLParser
#load "GCLLexer.fs"
open GCLLexer

// We define the evaluation function recursively, by induction on the structure
// of arithmetic expressions (AST of type expr)
let rec eval (e : aExpr) (mem : Map<string, float>) =
  match e with
    | Num(x) -> x
    | ArrayVariable(x, a) -> mem.[x+"["+(sprintf "%i" ((int) (eval a mem)))+"]"]
    | Variable(x) -> mem.[x]
    | TimesExpr(x,y) -> (eval x mem) * (eval y mem)
    | DivExpr(x,y) -> (eval x mem) / (eval y mem)
    | PlusExpr(x,y) -> (eval x mem) + (eval y mem)
    | MinusExpr(x,y) -> (eval x mem) - (eval y mem)
    | PowExpr(x,y) -> (eval x mem) ** (eval y mem)
    | UPlusExpr(x) -> (eval x mem)
    | UMinusExpr(x) -> - (eval x mem)

let rec bEval (bexpr : bExpr) (mem : Map<string, float>) =
    match bexpr with
        | True -> true
        | False -> false
        | LOr(b1, b2) -> let b1 = (bEval b1 mem)
                         let b2 = (bEval b2 mem)
                         b1 || b2
        | LAnd(b1, b2) -> let b1 = (bEval b1 mem)
                          let b2 = (bEval b2 mem)
                          b1 && b2
        | Or(b1, b2) -> (bEval b1 mem) || (bEval b2 mem)
        | And(b1, b2) -> (bEval b1 mem) && (bEval b2 mem)
        | Not(b) -> not (bEval b mem)
        | Equals(a1, a2) -> (eval a1 mem) = (eval a2 mem)
        | NotEquals(a1, a2) -> (eval a1 mem) <> (eval a2 mem)
        | GrtThan(a1, a2) -> (eval a1 mem) > (eval a2 mem)
        | GrtEq(a1, a2) -> (eval a1 mem) >= (eval a2 mem)
        | LeThan(a1, a2) -> (eval a1 mem) < (eval a2 mem)
        | LeEq(a1, a2) -> (eval a1 mem) <= (eval a2 mem);;  

let rec evalCmd (cmd:cmd) (mem : Map<string, float>) =
    match cmd with  
        | Skip -> mem
        | Assign(x, a) -> mem.Add(x, (eval a mem))
        | AssignArray(x, a1, a2) -> mem.Add(x+"["+(sprintf "%i" ((int) (eval a1 mem)))+"]", (eval a2 mem))
        | Cmds(c1, c2) -> evalCmd c2 (evalCmd c1 mem)
        | If(gCmd) -> evalGCmd gCmd mem
        | Do(gCmd) -> evalGCmd gCmd mem
                    //match gCmd with
                    //   | Statement(bExpr, c1) -> 
                    //           if (bEval bExpr mem) then
                    //               let newMem = evalCmd c1 mem
                    //               (evalCmd cmd newMem)
                    //           else
                    //               mem
                    //   | GCmds(gc1, gc2) ->
                    //       match gc1 with
                    //           | Statement(bExpr, c1) -> 
                    //               if (bEval bExpr mem) then
                    //                   let newMem = evalCmd c1 mem
                    //                   (evalCmd c1 newMem)
                    //               else
                    //                   (evalGCmd gc2 mem)
and evalGCmd (gCmd:gCmd) (mem : Map<string, float>) =
    match gCmd with
        | Statement(bExpr, cmd) -> if (bEval bExpr mem) then
                                        (evalCmd cmd mem)
                                   else
                                        mem
        | GCmds(gc1, gc2) -> match gc1 with
                                | Statement(bExpr, c1) -> 
                                        if (bEval bExpr mem) then
                                            let newMem = evalCmd c1 mem
                                            (evalCmd c1 newMem)
                                        else
                                            (evalGCmd gc2 mem)
                                | GCmds(gc1, gc2) -> failwith ("Wrong parse");;

let rec printer (cmd:cmd) =
    0;;
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
        printfn "Bye bye"
    else
        printfn "Enter an arithmetic expression: "
        try
        let mutable prog = ""
        // We parse the input string
        let mutable read = Console.ReadLine()
        while (read <> "") do
            prog <- prog + read
            read <- Console.ReadLine()
        let e = parse (prog)
        printfn "AST ☢: %A" (e)
        // and print the result of evaluating it 
        //printfn "Result: %A" (evalCmd e Map.empty)
        printfn "Result: %A" (printer e)
        compute n
        with e -> compute (n-1)

// Start interacting with the user
compute 20;;
