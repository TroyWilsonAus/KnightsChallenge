module Util

let delayFunc = fun _ ->
    System.Threading.Thread.CurrentThread.Join 100 |> ignore

let logFun text arg =
    let retval = printfn text arg
    
    System.Threading.Thread.CurrentThread.Join 1000 |> ignore
    retval |> ignore

