open Common.VisitorMonad
open Common.Context

let passes = [
  Semantic.ScopePass.process;
  Semantic.MiscCheckPass.process;
  (* Semantic.Print.process; *)
  Semantic.TypeCheckPass.process;
  Semantic.LoweringPass.process;
  Semantic.TypeInferPass.process;
]

let check p =
  let run = List.fold_left (fun acc elem -> acc >>= fun newP -> elem newP) (success p) passes
  in run S.empty