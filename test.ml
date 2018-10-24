open OUnit2
open Hogwarts
open Command
open State

(**Loading json files in [spells_json] *)
let spells_json = from_json (Yojson.Basic.from_file "spells.json")

(** [assert_equal_test input expected_output] constructs an OUnit
    test named [name] that asserts the equality of 
    [expected_output] with [input]. *)
let assert_equal_test name input expected_output : test =
  name >:: (fun _ -> assert_equal input expected_output)

(** [assert_raises_test input_exn expected_output] constructs an OUnit
    test named [name] that asserts the raising of exception [input_exn] with
    unit [expected_output].*)
let assert_raises_test name input_exn expected_output : test =
  name >:: (fun _ -> assert_raises input_exn  expected_output)

(* let spell : spell_info = {
   name = "Confringo";
   damage = 5;
   target = "enemy";
   description = "Blasting Charm; causes items the charm comes in contact with to burst into flames."
   } *)

let hogwarts_tests =
  [

    (* search *)
    (* assert_equal_test "search test 1" () *)

    (* description *)
    assert_equal_test "description test 1"
      ("Blasting Charm; causes items the charm comes in contact with to burst "^
       "into flames.") 
      ((spell_description spells_json "confringo")); 

    assert_equal_test "description test 2" 
      ("You are on Ho Plaza. Cornell Health is to the southwest. "^
       "The chimes are playing a concert in the clock tower. "^
       "Someone tries to hand you a quartercard, but you avoid them.") 
      ((spell_description spells_json "episky"));

  ]

let command_tests =
  [

  ]

let state_tests =
  [


  ] 

let suite =
  "test suite for A6"  >::: List.flatten [
    hogwarts_tests;
    command_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite