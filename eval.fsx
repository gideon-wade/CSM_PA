module eval

// We define the evaluation function recursively, by induction on the structure
// of arithmetic expressions (AST of type expr)

                    //match gCmd with
                    //   | Statement(bExpr, c1) -> 
                    //           if (bEval bExpr varMem arrMem) then
                    //               let newMem = evalCmd c1 mem
                    //               (evalCmd cmd newvarMem arrMem)
                    //           else
                    //               mem
                    //   | GCmds(gc1, gc2) ->
                    //       match gc1 with
                    //           | Statement(bExpr, c1) -> 
                    //               if (bEval bExpr varMem arrMem) then
                    //                   let newMem = evalCmd c1 mem
                    //                   (evalCmd c1 newvarMem arrMem)
                    //               else
                    //                   (evalGCmd gc2 varMem arrMem)
