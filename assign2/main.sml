structure Main =
struct
local
  open Csc330
  open Set           
in

infix 1 IDENTICAL
infix 3 EXCEPT
infix 4 IS_SUBSET_OF
infix 5 INTERSECT
infix 6 UNION
infix 7 IN
infix 8 CONTAINS        
infix 9 ++
infix 9 --

fun comp_list_int(a, b) =
    comp_list_any(a, b, Int.compare)

fun comp_list_string(a, b) =
    comp_list_any(a, b, String.compare)

fun comp_pair_any ( (a1,b1), (a2,b2), fcomp) =
    comp_list_any ([a1,b1], [a2,b2], fcomp)

fun comp_pair_int (a,b) =
    comp_pair_any(a,b, Int.compare)

fun str_pair_int(a,b) =
    "(" ^ Int.toString(a) ^ ":" ^ Int.toString(b) ^ ")"

fun str_list_any (lst,fstr) =
    "[" ^ (String.concatWith "," (List.map fstr lst)) ^ "]"

fun str_list_int (lst) =
    str_list_any(lst, Int.toString)

fun ident x = x

fun str_list_string (lst) =
    str_list_any(lst, ident)

fun test (exp, errMsg) =
    if exp then
      ()
    else
      print("test failed: " ^ errMsg ^ "\n")

fun assert (exp, errMsg) =
    if exp then
      ()
    else
      (TextIO.output (TextIO.stdErr, errMsg ^ "\n"); OS.Process.exit(OS.Process.failure))

fun test_empty() =
    test(size_set(EmptySet Int.compare) = 0, "Test empty set")

fun test_set_int() =
    let
      val se = EmptySet Int.compare
      val sa = se ++ 1 ++ 2 ++ 3 ++ 5 ++ 3 ++ 2
      val sb = se ++ 9 ++ 3 ++ 2
      val _ = test(sb IDENTICAL list_to_set ([2,3,9], Int.compare), "testing simple insertion")
      val _ = test(sa IDENTICAL list_to_set ([1,2,3,5], Int.compare), "testing duplicate insertion")
    in
      ()
    end

fun test_set_int_2() =
    let
      val se = EmptySet Int.compare
      val sa = se ++ 1 ++ 1 ++ 1 ++ 1 -- 2
      val sb = se ++ 9 ++ 3 ++ 2 -- 4
      val _ = test(sa IDENTICAL list_to_set ([1], Int.compare), "testing more duplicate insertions")
      val _ = test(sb IDENTICAL list_to_set ([2,3,9], Int.compare), "testing remove non-exist element")
    in
      ()
    end
      
fun test_set_ops() =
    let
      val se = EmptySet Int.compare
      val sa = se ++ 1 ++ 2 ++ 3 ++ 5 ++ 3 ++ 2
      val sb = se ++ 9 ++ 3 ++ 2 --2
      val _ = test(is_empty_set se, "testing is_empty_set")
      val _ = test(sb UNION sb IDENTICAL sb, "testing union")
      val _ = test(sb INTERSECT se IDENTICAL se, "testing intersect")
      val _ = test(sb EXCEPT sb IDENTICAL se, "testing intersect")
      val _ = test(not(2 IN sb), "testing IN")
      val _ = test(se IS_SUBSET_OF sb, "testing IS_SUBSET_OF")
    in
      ()
    end


fun test_set_ops_2() =
    let
      val e1 = EmptySet Int.compare
      val e2 = EmptySet Int.compare
      val _ = test(e1 IS_SUBSET_OF e2, "compare two empty sets")
      val s1 = e1 ++ 1 ++ 2 ++ 3
      val _ = test(e1 IS_SUBSET_OF s1, "empty set is a subset of any set")
      val _ = test(not (s1 IS_SUBSET_OF e1), "non-empty set is not a subset of an empty set")
      val s2 = e2 ++ 1 ++ 2 ++ 5 ++ 6 -- 6 -- 5
      val _ = test(s2 IS_SUBSET_OF s1, "[1,2] is a subset of [1,2,3]")
      val _ = test(not(s1 IS_SUBSET_OF s2), "[1,2,3] is not a subset of [1,2]")
      val s3 = e2 ++ 1 ++ 2 ++ 3
      val _ = test(s1 IS_SUBSET_OF s3, "[1,2,3] is a subset of [1,2,3]")

      val s4 = e1 ++ 1 ++ 2
      val s5 = e1 ++ 1 ++ 3 ++ 4
      val s6 = e1 ++ 1 ++ 2 ++ 3 ++ 4
      val _ = test(s4 UNION s5 IDENTICAL s6, "[1,2] union [1,3,4] = [1,2,3,4]")
      val _ = test(s6 UNION s6 IDENTICAL s6, "[1,2,3,4] union [1,2,3,4] = [1,2,3,4]")
      val _ = test(s6 UNION e1 IDENTICAL s6, "[1,2,3,4] union [] = [1,2,3,4]")

      val s7 = e1 ++ 1 ++ 2 ++ 3 ++ 4 ++ 5
      val s8 = e1 ++ 2 ++ 5 ++ 8
      val s9 = e1 ++ 2 ++ 5
      val _ = test(s7 INTERSECT s8 IDENTICAL s9, "[1,2,3,4,5] intersect [2,5,8] = [2,5]")
      val _ = test(s7 INTERSECT e1 IDENTICAL e1, "[1,2,3,4,5] intersect [] = []")

      val s10 = e1 ++ 1 ++ 3 ++ 4
      val _ = test(s7 EXCEPT s9 IDENTICAL s10, "[1,2,3,4,5] except [2,5] = [1,3,4]")
      val _ = test(s7 EXCEPT e1 IDENTICAL s7, "[1,2,3,4,5] except [] = [1,2,3,4,5]")
      val _ = test(e1 EXCEPT s7 IDENTICAL e1, "[] except [1,2,3,4,5] = []")

      val _ = test(not(2 IN s10), "2 is not in [1,3,4]")
      val _ = test(not(2 IN e1), "2 is not in []")
    in
      ()
    end

fun test_conversions() =
    let
      val se = EmptySet Int.compare
      val sb = se ++ 9 ++ 3 ++ 2
      val _ = test(str_set(se, Int.toString) = "{}", "testing set_str")
      val _ = test(set_to_list(sb) = [2,3,9], "testing set_to_list")
    in
      ()
    end

fun test_conversions_2() =
    let
      val se = EmptySet String.compare
      val sb = se ++ "zbb" ++ "c" ++ "d" ++ "b" ++ "z"
      val _ = test(str_set(sb, String.toString) = "{b:c:d:z:zbb}", "set is b,c,d,z,zbb")
      val sc = se ++ "abb" ++ "c" ++ "d" ++ "b" ++ "z"
      val _ = test(str_set(sc, String.toString) = "{abb:b:c:d:z}", "set is abb,b,c,d,z")
    in
      ()
    end

fun test_others() =
    let
      val se = EmptySet String.compare
      val sa = se ++ "" ++ "a" ++ "ab" ++ "abc" ++ "ab" ++ "ac" ++ " a" 
      val _ = test(max_in_set(sa) = "ac", "testing max_in_set")
      val _ = test(min_in_set(sa) = "", "testing min_in_set")
    in
      ()
    end

fun test_pairs() =
    let
      val se = EmptySet comp_pair_int
      val sa = se ++ (1,3) ++ (1,2)
      val _   = test(str_set(sa, str_pair_int) = "{(1:2):(1:3)}",  "testing a pair")
    in
      ()
    end

fun test_lists() =
    let
      val se = EmptySet comp_list_int
      val sa = se ++ [1,3] ++ [1,2] ++ [] ++ [2,3,9]
      val _   = test(str_set(sa, str_list_int) = "{[]:[1,2]:[1,3]:[2,3,9]}",  "testing a list")
    in
      ()
    end

fun test_lists_strings() =
    let
      val se = EmptySet comp_list_string
      val sa = se ++ ["","abc"] ++ ["daniel","robert"] ++ [] ++ ["Canada","Japan","Brazil"] ++ ["","abc"]
      val _   = test(str_set(sa, str_list_string) = "{[]:[,abc]:[Canada,Japan,Brazil]:[daniel,robert]}", 
                      "testing a list of strings")
    in
      ()
    end


(* Kush *)
fun test_lists_2() = 
  let
    val s1 = EmptySet comp_list_int
    val s2 = EmptySet comp_list_string
    val s3 = s1 ++ [1,2] ++ [4,5] ++ [5,6]
    val s4 = s1 ++ [4,5] ++ [7,7,7]
    val s5 = ["Hi", "this", "is", "a", "string list"]
    val s6 = ["and", "another", "string list"]
    val s7 = ["another", "list"]
    val s8 = ["hi", "and", ""]
    val s9 = s2 ++ s8 ++ s7
    val s10 = s2 ++ s5 ++ s6
    val s11 = EmptySet Int.compare
    val s12 = s11 ++ 1 ++ 2 ++ 44 ++ 99 ++ 3
    val s13 = EmptySet String.compare
    val s14 = s13 ++ "1" ++ "2" ++ "44" ++ "99" ++ "3"
    val _ = test(s9 EXCEPT s10 IDENTICAL s9, "Testing String_List Sets: [Except, Identical]")
    val _ = test(s6 IN s10, "Testing String_List Sets: [Subset]")
    val _ = test(str_set(s3, str_list_int) = "{[1,2]:[4,5]:[5,6]}", "Testing Int_List Sets: [String-Set]")
    val _ = test(Set([str_set(map_set(s12, String.compare, Int.toString), ident)], String.compare) IDENTICAL Set([str_set(s14, ident)], String.compare), "Testing Int Set: [Map, Str]")
  in
    ()
  end

(* Greay *)
fun greay_tests() =
    let
        fun addFive(a) = a + 5
        val s0 = EmptySet Int.compare
        val l0 = EmptySet comp_list_int
        val s1 = s0 ++ 2 ++ 5 ++ 4 ++ 1 ++ 4
        val s2 = s0 ++ 0 ++ ~1 ++ 1 ++ 5 ++ ~5 ++ 5 ++ ~2
        val l1 = l0 ++ [1,2,3] ++ [2,3,4] ++ [] ++ [1,2] ++ [5] -- []
        val l2 = l0 ++ [1,2,3] ++ [2,3,4] ++ [] ++ [1,2] ++ [5] ++ []
        val _ = test(map_set(s1, Int.compare, addFive) IDENTICAL list_to_set ([6,7,9,10], Int.compare), "testing mapped set")
        val _ = test(map_set(s1, Int.compare, addFive) IDENTICAL list_to_set ([6,7,9,10], Int.compare), "testing insert list & remove list with emptylist")
        val _ = test(str_set(l1, str_list_int) = "{[1,2]:[1,2,3]:[2,3,4]:[5]}",  "testing remove list with emptylist")
        val _ = test(str_set(l2, str_list_int) = "{[]:[1,2]:[1,2,3]:[2,3,4]:[5]}",  "testing insert list with with duplicate emptylist")
        val _ = test(str_set(s2, Int.toString) = "{~5:~2:~1:0:1:5}",  "testing insert with negative ints")
    in ()
    end


fun main (prog_name, args) =
    let
      val _ = test_empty()
      val _ = test_set_int()
      val _ = test_set_ops()
      val _ = test_conversions()
      val _ = test_others()
      val _ = test_pairs()
      val _ = test_lists()
      val _ = test_lists_strings()
      (* extra *)
      val _ = test_set_int_2()
      val _ = test_set_ops_2()
      val _ = test_conversions_2()
      val _ = test_lists_2()
      val _ = greay_tests()
    in
      print("Finished testing\n"); exit()
    end

                                
end
end    
