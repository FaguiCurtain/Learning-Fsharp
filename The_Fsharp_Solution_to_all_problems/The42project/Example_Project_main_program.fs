// order of files is important
// in the left pane "solution", we should see Example_Project_File1.fs ABOVE
//                                            Example_Project_main_program.fs

open File1.MyModule1


[<EntryPoint>]
let main args =
    printfn "Arguments passed to function : %A" args |> ignore
    myfun1 3
    0

