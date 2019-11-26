
let passes = [
  TypeCheck.process
]

let check p = List.fold_left (fun t pass -> pass t) p passes