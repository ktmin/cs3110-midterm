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


let spell1 =
  search spells_json "Avada Kedavra"

let spell2 = 
  search spells_json "Confringo" 

let spell3 = 
  search spells_json "Confundo"

let spell4 = 
  search spells_json "Episky"   

let spell5 = 
  search spells_json "Crucio"  

let spell6 = 
  search spells_json "Expelliarmus"

let spell7 = 
  search spells_json "Ferula"


let init_pl =
  init_player spells_json "Bryan"

let init_en = 
  init_enemy spells_json "Oscar"

let dazed_2 = 
  update spell3 init_pl init_en 

let dazed_1 = 
  update spell1 dazed_2 init_pl 

let dazed_0 = 
  update spell1 dazed_1 init_pl

let draw1 = 
  draw init_pl

let draw2 = 
  draw draw1

let draw3 = 
  draw draw2

let state_tests =
  [
    "test 1 " >:: (fun _ ->
        assert_equal (update spell1 init_pl init_en 
                      |> get_hp) (0));

    "test 2 " >:: (fun _ ->
        assert_equal (update spell2 init_pl init_en 
                      |> get_hp) (95));

    "test 3 " >:: (fun _ ->
        assert_equal (update spell3 init_pl init_en 
                      |> get_hp) (95));

    "test 4 " >:: (fun _ ->
        assert_equal (update spell3 init_pl init_en 
                      |> get_dazed) (2));

    "test 5 " >:: (fun _ ->
        assert_equal (update spell1 dazed_2 init_pl 
                      |> get_dazed) (1));

    "test 6 " >:: (fun _ ->
        assert_equal (update spell1 dazed_1 init_pl 
                      |> get_dazed) (0));

    "test 7 " >:: (fun _ ->
        assert_equal (update spell1 dazed_0 init_pl 
                      |> get_hp) (0));

    "test 8 " >:: (fun _ ->
        assert_equal (update spell4 init_pl init_en 
                      |> get_hp) (110));

    "test 9 " >:: (fun _ ->
        assert_equal (update spell5 init_pl init_en 
                      |> get_hp) (85));

    "test 10 " >:: (fun _ ->
        assert_equal (update spell6 init_pl draw2 
                      |> get_hand |> List.length) 
          (1));

    "test 11 " >:: (fun _ ->
        assert_equal (update spell7 init_pl init_en 
                      |> get_hp) 
          (110));

  ] 

let suite =
  "test suite for A6"  >::: List.flatten [
    hogwarts_tests;
    command_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite
