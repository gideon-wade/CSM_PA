module Verificator

//Define abstract syntax trees for predicates
type Expr =
    | X of string
    | LogicalX of string
    | TimesExpr of (Expr * Expr)
    | DivExpr of (Expr * Expr)
    | PlusExpr of (Expr * Expr)
    | MinusExpr of (Expr * Expr)  
    | PowExpr of (Expr * Expr)
    | UMinusExpr of (Expr);;

type Predicate =
    | True
    | And of (Predicate * Predicate)
    | Or  of (Predicate * Predicate)
    | Not of Predicate
    | Imp of (Predicate * Predicate)
    | Eq  of (Expr * Expr)
    | GT  of (Expr * Expr)
    | GTE of (Expr * Expr)
    | LT  of (Expr * Expr)
    | LTE of (Expr * Expr);;

//Determine a partial predicate assigment that covers C's PG
let rec coverNodes (startNode:Node) (endNode:Node) (edge:cmd) : List<Node> =
    match edge with
        | Cmds(c1, c2)           -> let q = sprintf "q%i" num
                                    num <- num + 1
                                    let e1 = coverNodes startNode q c1
                                    let e2 = coverNodes q endNode c2
                                    e1 @ e2
        | Do(gCmd)               -> let e = coverNodesGC startNode startNode gCmd
                                    [startNode] @ e  
        | _                      -> []    
and coverNodesGC (startNode:Node) (endNode:Node) (edge:gCmd) =
    match edge with
        | Statement(b, cmd)      -> let q = sprintf "q%i" num 
                                    num <- num + 1;
                                    coverNodes q endNode cmd
        | GCmds(gc1, gc2)        -> let e1 = coverNodesGC startNode endNode gc1
                                    let e2 = coverNodesGC startNode endNode gc2 
                                    e1 @ e2;;

//Compute all shortest path fragments 
let rec build (qStart : Node) (curQ : Node) (w : string) (edges : Edge list) (nodes : Node list) (allEdges : Edge list) =
    match edges with
        | []                               -> []
        | (q1, c, q2)::tail when q1 = curQ -> if List.contains q2 nodes then
                                                  [(qStart, [w; " "; graphPrinter c] |> String.Concat, q2)] @ (build qStart curQ w tail nodes allEdges)
                                              else
                                                  (build qStart q2 ([w; " "; graphPrinter c] |> String.Concat) allEdges nodes allEdges) @ (build qStart curQ w tail nodes allEdges)
        | (q1, c, q2)::tail                -> //printfn "2: %s %s %s %s %A" qStart curQ q1 q2 (List.length tail)
                                              build qStart curQ w tail nodes allEdges;;

let rec SPF (edges : Edge List) (nodes : Node list) (allNodes : Node list) =
    match nodes with
    | []                     -> []
    | q::tail when q <> "q◀" -> (build q q "" edges allNodes edges)::(SPF edges tail allNodes)
    | q::tail                -> [];;
    
let rec printSPFS (spfs) (conditions) =
    match spfs with
        | [] -> ""    
        | spf::tail -> (printSPF spf conditions) + (printSPFS tail conditions)
and printSPF (spf) (conditions : Map<string, string>) =
    match spf with
        | []              -> ""    
        | (a, b, c)::tail -> conditions.[a] + "  => " + b + "  =>  " + conditions.[c] + "\n" + (printSPF tail conditions);;

let rec getConditions (nodes : Node list) = 
    match nodes with
        | []         -> []
        | node::tail -> printf "%s: " node
                        (node, Console.ReadLine())::getConditions tail;;
//Construct a verification condition A => B for every SPF




