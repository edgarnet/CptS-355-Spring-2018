(* Edgar Perez - CptS 355 *)
(* Help from Elliott unless otherwise noted *)
(* #1 *) 
fun inList (x, []) = false 
  | inList (x, y :: ?) = 
        if x = y then true 
        else inList (x, ?)

(* #2 *)
(*Help from qaphla *)
fun removeDuplicates [] = []
  | removeDuplicates (x::rest) =
  if inList(x, rest) = true
  then removeDuplicates(rest)
  else x::(removeDuplicates(rest))

(* #3 *)
fun listIntersect (L1, []) = []
  | listIntersect ([], L2) = []
  | listIntersect (x::rest, L2) = if inList(x, L2) = true then
   removeDuplicates( x::(listIntersect(rest, L2))) else listIntersect(rest, L2)  

(* #4 *)
fun range min step max = if min = max then []
                           else if min > max andalso step > 0 then []
                           else if min < max andalso step > 0 orelse min > max
                           andalso step < 0 then min::(range(min + step) step
                           max) else range(min + step) step max

(* #5 *)
(* Help from Samra *)
fun numbersToSum sum [] = []
  | numbersToSum sum (x::rest) = if sum > x then x::(numbersToSum (sum - x) rest)
                               else numbersToSum (sum - x) rest

(* #6 *)
(* Help from Alexis *)
fun replace n v (x::rest) = if n = 0 then v::rest
                          else replace (n - 1) v rest

(* #7 *)

fun reverse [] = []
  | reverse (x::rest) = reverse (rest)@[x]

fun groupLeft v [] buf = [buf]
  | groupLeft v (x::rest) buf = if (length buf) = v 
                                then buf::(groupLeft v rest [x]) 
                                else groupLeft v rest ([x]@buf)
                                
fun groupNLeft v [] = [[]]
  | groupNLeft v x = reverse(groupLeft v (reverse x) [])



fun groupRight v [] buf = [buf]
  | groupRight v (x::rest) buf = if length buf = v
                                 then buf::(groupRight v rest [x])
                                 else groupRight v rest (buf@[x])

fun groupNRight v [] = [[]]
  | groupNRight v x = groupRight v x []
