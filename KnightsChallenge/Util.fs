module Util

let logFun text arg =
    let retval = printfn text arg
    
    System.Threading.Thread.CurrentThread.Join 1000 |> ignore
    retval |> ignore