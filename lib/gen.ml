open Common.VisitorMonad
open Common.Meta

let passes = []

let generate p =
  let run = List.fold_left (fun acc elem -> acc >>= fun newP -> elem newP) (success p) passes
  in run empty_context