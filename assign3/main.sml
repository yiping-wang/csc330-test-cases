structure Main =
struct
local
  open Patterns
  open Testing
in

(*  some code to make it easier to test *)


infix 9 ++
infix 9 --
infix 7 ==

val tt = emptyTree ++ 4 ++ 2 ++ 1 ++ 5 ++ 1 ++ 10
val tt_yw_unique = emptyTree ++ 7 ++ 5 ++ 6 ++ 3 ++ 4 ++ 2 ++ 8 ++ 10
val tt_yw_duplicate = emptyTree ++ 8 ++ 4 ++ 6 ++ 7 ++ 3 ++ 3 ++ 2 ++ 2 ++ 4 ++ 4 ++ 5 ++ 5 ++ 10 ++ 10 ++ 10 ++ 11 ++ 12 ++ 12
(*Greay trees*)
val tt_complete = emptyTree ++ 15 ++ 19 ++ 10 ++ 17 ++ 18 ++ 16 ++ 22 ++ 20 ++ 25 ++ 12 ++ 11 ++ 13 ++ 8 ++ 1 ++ 9
val tt_dup_right = emptyTree ++ 15 ++ 1 ++ 20 ++ 20 ++ 20 ++ 19 ++ 25 ++ 21
val tt_dup_left = emptyTree ++ 15 ++ 25 ++ 9 ++ 11 ++ 12 ++ 13 ++ 9 ++ 10
val tt_one = emptyTree ++ 15
val tt_empty = emptyTree

(*
each of the following pairs is  a set of tests. One per item in the assignment.
the #1 of the pair is the name of the test
    #2 is a list of tests.
    Each element of this list is a boolean expression that tests certain
    functionality. All boolean expressions should evaluate to true
add tests to test you code

*)

val test_tree_insert =
    ("T1. test tree insert", [
      (tree_root emptyTree) = NONE,
      (tree_root tt) = SOME 4,
      (tree_root tt_yw_unique) = SOME 7,
      (tree_root tt_yw_duplicate) = SOME 8,
      (*Greay tests*)
      (tree_root tt_complete) = SOME 15,
      (tree_root tt_dup_right) = SOME 15,
      (tree_root tt_dup_left) = SOME 15,
      (tree_root tt_one) = SOME 15,
      (tree_root tt_empty) = NONE

    ])

val test_tree_delete =
    ("T2. test tree delete", [
      tt--4 = (emptyTree ++ 2 ++ 1 ++ 5 ++ 1 ++ 10),
      tt_yw_unique--10 = (emptyTree ++ 7 ++ 5 ++ 6 ++ 3 ++ 4 ++ 2 ++ 8),
      tt_yw_unique--8 = (emptyTree ++ 7 ++ 5 ++ 6 ++ 3 ++ 4 ++ 2 ++ 10),
      tt_yw_unique--5 = (emptyTree ++ 7 ++ 4 ++ 6 ++ 3 ++ 2 ++ 8 ++ 10),
      tt_yw_unique--5--7--4 = (emptyTree ++ 6 ++ 3 ++ 2 ++ 8 ++ 10),
      (tt_yw_unique--100 handle NotFound => emptyTree) = (emptyTree),
      tt_yw_duplicate--4--4--4 = (emptyTree ++ 8 ++ 3 ++ 3 ++ 2 ++ 2 ++ 6 ++ 7 ++ 5 ++ 5 ++ 10 ++ 10 ++ 10 ++ 11 ++ 12 ++ 12),
      (*Greay tests*)
      tt_complete--25--15--16 = (emptyTree ++ 13 ++ 10 ++ 19 ++ 17 ++ 18 ++ 22 ++ 20 ++ 12 ++ 11 ++ 8 ++ 1 ++ 9),
      tt_complete--25--15--16--12--8--10--22--19--11--13--17--9--18 = (emptyTree ++ 1 ++ 20 ),
      (tt_complete--25--15--16--12--8--10--22--19--19 handle NotFound => tt_complete--25--15--16--12--8--10--22--19) = (emptyTree ++ 13 ++ 18 ++ 9 ++ 20 ++ 17 ++ 11 ++ 1 ),
      tt_dup_right--20--15--1 = (emptyTree ++ 20 ++ 20 ++ 19 ++ 25 ++ 21),
      tt_dup_left--25--9--10 = (emptyTree ++ 15 ++ 9 ++ 11 ++ 12 ++ 13),
      tt_one--15 = (emptyTree),
      (tt_empty--15 handle NotFound => emptyTree) = (emptyTree)
     ])

val test_tree_height =
    ("T3. test tree height", [
      (tree_height tt) = 4,
      (tree_height (tt_yw_unique ++ 100 ++ 1000 ++ 1000)) = 6,
      (tree_height (tt_yw_duplicate)) = 6,
      (*Greay tests*)
      (tree_height tt_complete) = 4,
      (tree_height tt_dup_right) = 5,
      (tree_height tt_dup_left) = 5,
      (tree_height tt_one) = 1,
      (tree_height tt_empty) = 0
    ])

val test_tree_fold =
    ("T4. test tree_fold_pre_order", [
      (tree_fold_pre_order (op +) 0 tt) = 23,
      (tree_fold_pre_order (fn (v, acc) => acc + 1) 0 tt_yw_unique) = 8,
      (tree_fold_pre_order (fn (v, acc) => acc + 1) 0 tt_yw_duplicate) = 18,
      (tree_fold_pre_order (op +) 0 tt_yw_duplicate) = 118,
      (*Greay tests*)
      (tree_fold_pre_order (op +) 0 tt_complete) = 216,
      (tree_fold_pre_order (op +) 0 tt_dup_right) = 141,
      (tree_fold_pre_order (op +) 0 tt_dup_left) = 104,
      (tree_fold_pre_order (op +) 0 tt_one) = 15,
      (tree_fold_pre_order (op +) 0 tt_empty) = 0
    ])

val test_tree_max =
    ("T5. test tree_max", [
      (tree_max tt) = SOME 10,
      (tree_max tt_yw_unique) = SOME 10,
      (tree_max tt_yw_duplicate) = SOME 12,
      (*Greay tests*)
      (tree_max tt_complete) = SOME 25,
      (tree_max (tt_dup_right--25--21)) = SOME 20,
      (tree_max (tt_dup_left--25--15)) = SOME 13,
      (tree_max tt_one) = SOME 15,
      (tree_max tt_empty) = NONE
    ])

val test_tree_to_list =
    ("T6. test tree_to_list", [
      (tree_to_list tt) = [4,2,1,1,5,10],
      (tree_to_list tt_yw_unique) = [7,5,3,2,4,6,8,10],
      (tree_to_list tt_yw_duplicate) = [8,4,3,3,2,2,4,4,6,5,5,7,10,10,10,11,12,12],
      (*Greay tests*)
      (tree_to_list tt_complete) = [15,10,8,1,9,12,11,13,19,17,16,18,22,20,25],
      (tree_to_list tt_dup_right) = [15,1,20,20,20,19,25,21],
      (tree_to_list tt_dup_left) = [15,9,9,11,10,12,13,25],
      (tree_to_list tt_one) = [15],
      (tree_to_list tt_empty) = []
    ])

val test_tree_filter =
    ("T7 test tree_filter", [
      (tree_filter (fn x => x> 4) tt) == (emptyTree ++ 5 ++10),
      (tree_filter (fn x => x mod 2 = 0) tt_yw_unique) == (emptyTree ++ 6 ++ 2 ++ 4 ++ 8 ++ 10),
      (*Greay tests*)
      (tree_filter (fn x => x mod 2 = 0) tt_complete) = (emptyTree ++ 12 ++ 10 ++ 8 ++ 18 ++ 16 ++ 22 ++ 20),
      (tree_filter (fn x => x mod 2 = 0) tt_dup_right) = (emptyTree ++ 20 ++ 20 ++ 20),
      (tree_filter (fn x => x mod 2 = 0) tt_dup_left) = (emptyTree ++ 12 ++ 10),
      (tree_filter (fn x => x mod 2 = 0) tt_one) = (emptyTree),
      (tree_filter (fn x => x mod 2 = 0) tt_empty) = (emptyTree)

    ])

val test_tree_sum_even =
    ("T8 test tree_sum_even", [
      (tree_sum_even tt) = 16,
      (tree_sum_even tt_yw_unique) = 30,
      (tree_sum_even tt_yw_duplicate) = 84,
      (*Greay tests*)
      (tree_sum_even tt_complete) = 106,
      (tree_sum_even tt_dup_right) = 60,
      (tree_sum_even tt_dup_left) = 22,
      (tree_sum_even tt_one) = 0,
      (tree_sum_even tt_empty) = 0
    ])

(* test pattern matching *)

val test_first_answer =
    ("P1. test first_answer", [
      first_answer (fn x => if String.size(x) = 3 then SOME x else NONE) ["this", "is", "the", "end", "of", "the", "world"] = "the",
      (* Greay tests*)
      first_answer (fn x => if (x < 0) then SOME x else NONE) [1,2,~1,2,1,2] = ~1,
      (first_answer (fn x => if (x < 0) then SOME x else NONE) [1,2,1,2,1,2] handle NoAnswer => 0) = 0,
      (first_answer (fn x => if (x < 0) then SOME x else NONE) [] handle NoAnswer => 0) = 0
    ]);



val test_all_answers =
    ("P2. all_answers",
     [
       all_answers (fn x => if (x = 1) then SOME [x] else NONE) [1,2,1,2,1,2] = NONE,
       all_answers (fn x => if (x < 0) then NONE else SOME ([1])) [] = SOME [],
       (* Greay tests*)
       all_answers (fn x => if (x < 0) then NONE else SOME ([x])) [1,2,1,2,1,2] = SOME [1,2,1,2,1,2],
       all_answers (fn x => if (x < 0) then NONE else SOME ([x])) [1,2,1,2,1,~2] = NONE
    ]);

val test_check_pattern =
    ("3. check_pattern", [
      check_pattern (TupleP [Wildcard,Variable "cat",
                       Variable "pp",TupleP[Variable "tt"],
                       Wildcard,ConstP 3,
                       ConstructorP("cony",Variable "pp")]) = false,
      (* Greay tests*)
      check_pattern (TupleP [Wildcard,Variable "cat",
                        Variable "pp",TupleP[Variable "tt"],
                        Wildcard,ConstP 3,
                        ConstructorP("cony",Variable "gg")]) = true,
      check_pattern (TupleP [Wildcard,TupleP[Wildcard],
                        Wildcard,ConstP 3,
                        ConstructorP("cony",UnitP)]) = true,
      check_pattern (TupleP [Wildcard,TupleP[ConstP 3],
                        Wildcard,ConstP 3,
                        ConstructorP("cony",ConstP 5)]) = true,
      check_pattern (TupleP [Wildcard,TupleP[Variable "cat"],
                        Wildcard,ConstP 3,
                        ConstructorP("cony",ConstructorP ("frank", TupleP [Variable "cat"]))]) = false,
     (* Matthew P tests*)
     check_pattern (ConstructorP("hello", TupleP[Variable "tt", TupleP[Variable "tt"]])) = false,
     check_pattern (ConstructorP("hello", TupleP[ConstP 1, TupleP[Wildcard]])) = true,
     check_pattern (Variable "helloWorld") = true,
     check_pattern (Wildcard) = true,
     check_pattern (ConstP 1) = true
    ]);


val test_match =
    ("test_match",
         [match(Unit, UnitP) = SOME [],
          match(Unit, Variable "cat") = SOME [("cat", Unit)],
          match(Tuple [Unit, Const 8], TupleP [Variable "cat", Variable "dog"]) = SOME [("cat", Unit),("dog", Const 8)],
          match(Tuple [Unit, Tuple [Unit, Unit]],
                        TupleP [Variable "cat", TupleP [Variable "dog", Variable "rat"]]) = SOME [("cat", Unit), ("dog", Unit),  ("rat", Unit)],
          match(Tuple[Const 7, Const 6, Unit, Const 7],
                        TupleP[
                          Variable "a",
                          Variable "ab",
                          Variable "abc",
                          Variable "abcd"]) = SOME [("a",Const 7), ("ab",Const 6), ("abc",Unit), ("abcd",Const 7)],
          (*Greay tests*)
          match(Const 1, ConstP 1) = SOME [],
          match(Const 1, ConstP 2) = NONE,
          match(Constructor ("test", Unit), ConstructorP ("test", UnitP)) = SOME [],
          match(Constructor ("test1", Unit), ConstructorP ("test2", UnitP)) = NONE,
          match(Tuple [Const 1, Const 1, Unit], TupleP [Variable "test", Variable "test", Variable "test"]) = SOME [("test", Const 1), ("test", Const 1), ("test", Unit)],
          match(Tuple [Unit, Const 1, Constructor ("test", Unit)], TupleP [ConstP 1, UnitP, ConstructorP ("test", UnitP)]) = NONE
    ]);


val test_first_match =
    ("test_first_match",
         [
           first_match Unit [UnitP] = SOME [],
           first_match Unit [Variable "cat"] = SOME [("cat", Unit)],
           first_match Unit [ConstP 3] = NONE,
           first_match (Tuple []) [TupleP [Wildcard]] = NONE,
           first_match (Tuple [Unit]) [TupleP []] = NONE,
           first_match (Tuple [Unit]) [TupleP [Variable "cat"]] = SOME [("cat", Unit)],
           first_match (Const 7) [Wildcard ] = SOME [],
           first_match (Tuple[Const 7, Const 6, Unit, Const 8])
                               [TupleP[ConstP 7, Variable "cat",Wildcard, ConstP 8]] = SOME [("cat",Const 6)],
           first_match (Tuple[Const 7, Const 6, Unit, Const 7])
                               [TupleP[Variable "a", Variable "ab",
                                       Variable "abc", Variable "abcd"]] = SOME [
              ("a",Const 7),
              ("ab",Const 6),
              ("abc",Unit),
              ("abcd",Const 7)
           ],
           (*Greay tests*)
           first_match Unit [] = NONE
    ]);

fun main (prog_name, args) =
    let
      val tree_tests = [
        test_tree_insert,
        test_tree_delete,
        test_tree_height,
        test_tree_fold,
        test_tree_max,
        test_tree_to_list,
        test_tree_filter,
        test_tree_sum_even
      ]

      val pattern_tests = [
        test_first_answer,
        test_all_answers,
        test_check_pattern,
        test_match,
        test_first_match]

      val all_tests = tree_tests @ pattern_tests
      val tests_done = map do_test_set all_tests
      val passed = (foldr (fn (x, acc) => if x then acc + 1 else acc) 0 tests_done)
      val total = length(tests_done)
    in
      print("Passed: " ^ Int.toString(passed) ^
            " Total: " ^ Int.toString(total) ^ "\nFinished testing\n");
      total - passed
    end


end
end
