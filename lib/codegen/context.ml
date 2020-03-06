type context = { other: int }

let empty_context = { other = 0 }

type pass_error = 
  | Message of string