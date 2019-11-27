open Common.VisitorMonad
open PassContext

let empty_context: context = { typeCheck = 0 }

let passes = [
  TypeCheck.process
]

let check p = 
  let run = List.fold_left (fun acc elem -> acc >>= fun newP -> elem newP) (success p) passes 
  in run empty_context