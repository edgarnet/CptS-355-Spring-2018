(* Edgar Perez - CptS 355 *)
(* Help from Elliott unless otherwise noted *)
(* #1 *) 
fun inList (x, []) = false 
  | inList (x, y :: ?) = 
        if x = y then true 
        else inList (x, ?)

fun myTest_inList (x, y) output =
  if inList (x, y) = output then true else false

(* #2 *)
(*Help from qaphla *)
fun removeDuplicates [] = []
  | removeDuplicates (x :: ?) =
  if inList(x, ?) = true
  then removeDuplicates(?)
  else x::(removeDuplicates(?))

fun myTest_removeDuplicates x output =
  if removeDuplicates x = output then true else false

(* #3 *)
fun listIntersect (L1, []) = []
  | listIntersect ([], L2) = []
  | listIntersect (x :: ?, L2) = if inList(x, L2) = true then
   removeDuplicates( x::(listIntersect(?, L2))) else listIntersect(?, L2)

fun myTest_listIntersect (x, y) output =
  if listIntersect (x, y) = output then true else false

(* #4 *)
fun range min step max = if min = max then []
                           else if min > max andalso step > 0 then []
                           else if min < max andalso step < 0 then []
                           else if min < max andalso step > 0 orelse min > max
                           andalso step < 0 then min::(range(min + step) step max)
                           else range(min + step) step max

fun myTest_range min step max output =
  if range min step max = output then true else false

(* #5 *)
(* Help from Samra *)
fun numbersToSum sum [] = []
  | numbersToSum sum (x :: ?) = if sum > x then x::(numbersToSum (sum - x) ?)
                                 else numbersToSum (sum - x) ?

fun myTest_numbersToSum sum x output =
  if numbersToSum sum x = output then true else false

(* #6 *)
(* Help from Sanskar *)
fun replace x y [] = []
  | replace x y (z :: ?) = if x = 0 then y::(replace (x - 1) y ?)
                           else z::(replace (x - 1) y ?)

fun myTest_replace x y z output =
  if replace x y z = output then true else false

(* #7 *)

fun reverse [] = []
  | reverse (x :: ?) = reverse (?)@[x]

fun groupLeft v [] buf = [buf]
  | groupLeft v (x :: ?) buf = if (length buf) = v 
                                then buf::(groupLeft v ? [x]) 
                                else groupLeft v ? ([x]@buf)
                                
fun groupNLeft v [] = [[]]
  | groupNLeft v x = reverse(groupLeft v (reverse x) [])

fun myTest_groupNLeft v x output =
  if groupNLeft v x = output then true else false

fun groupRight v [] buf = [buf]
  | groupRight v (x :: ?) buf = if length buf = v
                                 then buf::(groupRight v ? [x])
                                 else groupRight v ? (buf@[x])

fun groupNRight v [] = [[]]
  | groupNRight v x = groupRight v x []

fun myTest_groupNRight v x output = 
  if groupNRight v x = output then true else false

val test1a_inList = myTest_inList (1, []) false
val test2a_inList = myTest_inList (1,[1,2,3]) true
val test3a_inList = myTest_inList ("c",["b","c","z"]) true
val test1b_removeDuplicates = myTest_removeDuplicates [1, 5, 1, 3, 4, 3, 5] [1,4,3,5]
val test2b_removeDuplicates = myTest_removeDuplicates ["a", "e", "c", "a", "a", "b", "c", "d"] ["e","a","b","c","d"]
val test3b_removeDuplicates = myTest_removeDuplicates [] []
val test1c_listIntersect = myTest_listIntersect ([1],[1]) [1]
val test2c_listIntersect = myTest_listIntersect ([1,2,3],[1,1,2]) [1,2]
val test3c_listIntersect = myTest_listIntersect ([[2,3],[1,2],[2,3]],[[1],[2,3]]) [[2,3]]
val test1d_range = myTest_range 0 5 30 [0,5,10,15,20,25]
val test2d_range = myTest_range 10 1 10 []
val test3d_range = myTest_range 1 ~1 10 []
val test1e_numToSum = myTest_numbersToSum 100 [10, 20, 30, 40] [10, 20, 30]
val test2e_numToSum = myTest_numbersToSum 30 [5, 4, 6, 10, 4, 2, 1, 5] [5, 4, 6, 10, 4]
val test3e_numToSum = myTest_numbersToSum 1 [2] []
val test1f_replace = myTest_replace 3 40 [1, 2, 3, 4, 5, 6] [1,2,3,40,5,6]
val test2f_replace = myTest_replace 0 "X" ["a", "b", "c", "d"] ["X","b","c","d"]
val test3f_replace = myTest_replace 4 false [true, false, true, true, true] [true,false,true,true,false]
val test1g_groupNLeft = myTest_groupNLeft 2 [1, 2, 3, 4, 5] [[1], [2, 3], [4, 5]]
val test2g_groupNLeft = myTest_groupNLeft 3 [1, 2, 3, 4, 5] [[1, 2], [3, 4, 5]]
val test3g_groupNLeft = myTest_groupNLeft 3 [] [[]]
val test1h_groupNRight = myTest_groupNRight 2 [1, 2, 3, 4, 5] [[1, 2], [3, 4], [5]]
val test2h_groupNRight = myTest_groupNRight 3 [1, 2, 3, 4, 5] [[1, 2, 3], [4,5]]
val test3h_groupNRight = myTest_groupNRight 3 [] [[]]
