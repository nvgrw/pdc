open Data

module type CONTEXT = sig
  type t 
  val scopes: t -> 'a StringMap.t list
end