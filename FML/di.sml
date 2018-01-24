(* #1 *) 
fun inList (x, []) = false 
  | inList (x, y :: ?) = 
        if x = y then true 
        else inList (x, ?)
  
val result = Bool.toString(inList(1, []))
fun main() = print result
(* #2 *)
