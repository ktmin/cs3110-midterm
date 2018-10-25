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


(** Used to test search *)
(* type spell = Hogwarts.spell_info

   let (spell1 : spell) = {
   name = "Confringo";
   damage = 5;
   target = "enemy";
   description = "Blasting Charm; causes items the charm comes in contact with to burst into flames."
   } *)

let hogwarts_tests =
  [

    (* search *)
    (* assert_equal_test "search test 1" (spell1) search spells_json "confringo"  *)

    (* description *)
    assert_equal_test "description test 1"
      ("Blasting Charm; causes items the charm comes in contact with to burst "^
       "into flames.") 
      ((spell_description spells_json "confringo")); 

    assert_equal_test "description test 2" 
      ("Used to heal minor injuries.") 
      ((spell_description spells_json "episky"));

  ]

let command_tests =
  [
    "test 1 " >:: (fun _ -> 
        assert_equal (parse " draw ") (Draw));

    "test 2 " >:: (fun _ -> 
        assert_equal (parse " CAst spell  ") (Cast ["spell"]));

    "test 3" >:: (fun _ -> 
        let f = fun () -> parse("  DRaw this ") in
        assert_raises (Invalidcommand) f);

    "test 4" >:: (fun _ -> 
        let f = fun () -> parse("  ") in
        assert_raises (Invalidcommand) f)
  ]

let init_state = (init_player spells_json "Clarkson")
let state_after_draw =
  draw init_state

let spell =
  search spells_json "confringo"

let state_after_cast =
  cast spell init_state state_after_draw

let state_tests =
  [
    "test 1 " >:: (fun _ ->
        assert_equal (init_state |> get_hp) (100));

    "test 2 " >:: (fun _ ->
        assert_equal (state_after_draw |> get_hp) (100));

    (* "test 3 " >:: (fun _ ->
        assert_equal (state_after_cast |> get_hp)
          (95)); *)

  ] 

let suite =
  "test suite for A6"  >::: List.flatten [
    hogwarts_tests;
    command_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite
