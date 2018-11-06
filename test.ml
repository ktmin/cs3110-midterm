open OUnit2
open Hogwarts
open Command
open State
open Yojson.Basic

(**Loading json files in [spells_json] *)
let spells_characters_json = from_json (from_file "spells.json") 
    (from_file "characters.json")

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

let hogwarts_tests =
  [

    (* description *)
    assert_equal_test "description test 1"
      ("Blasting Charm; causes items the charm comes in contact with to burst "^
       "into flames.") 
      ((spell_description spells_characters_json "confringo")); 

    assert_equal_test "description test 2" 
      ("Used to heal minor injuries.") 
      ((spell_description spells_characters_json "episky"));

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


let avada =

  search_spells spells_characters_json "avada kedavra"

let confingo = 
  search_spells spells_characters_json "confringo" 

let confundo = 
  search_spells spells_characters_json "confundo"

let episky = 
  search_spells spells_characters_json "episky"   

let crucio = 
  search_spells spells_characters_json "crucio"  

let expelliarmus = 
  search_spells spells_characters_json "expelliarmus"

let ferula = 
  search_spells spells_characters_json "ferula"

let incarcerous = 
  search_spells spells_characters_json "incarcerous"

let sectumsempra = 
  search_spells spells_characters_json "sectumsempra"

let stupefy = 
  search_spells spells_characters_json "stupefy"

let obliviate = 
  search_spells spells_characters_json "obliviate"


let init_pl  =
  init_player spells_characters_json "Bryan"

let init_en = 
  init_enemy spells_characters_json "Cho Chang"


let state_tests =
  [             
    "draw test 1 " >:: (fun _ ->
        assert_equal (draw init_pl |> get_hand |> 
                      List.length) (1));

    "draw test 2 " >:: (fun _ ->
        let draw1 = draw init_pl in 
        assert_equal (draw draw1 |> get_hand |> 
                      List.length) (2));

    "draw test 3 " >:: (fun _ ->
        let draw1 = draw init_pl in 
        let draw2 = draw draw1 in 
        assert_equal (draw draw2 |> get_hand |> 
                      List.length) (3));

    "dazed test 1 " >:: (fun _ ->
        assert_equal ((cast confundo init_pl init_en)
                      |> snd |> get_dazed) 2);

    "dazed test 2 " >:: (fun _ ->
        assert_equal (
          let dazed_enemy = cast confundo init_pl init_en
                            |> snd in 
          cast avada dazed_enemy init_en |> snd            
        ) (init_en) );

    "dazed test 3 " >:: (fun _ ->
        assert_equal (
          let dazed_enemy = cast confundo init_pl init_en
                            |> snd in 
          cast avada dazed_enemy init_en |> fst |> get_dazed            
        ) (1) );

    "dazed test 4 " >:: (fun _ ->
        assert_equal (
          let dazed_enemy = cast confundo init_pl init_en
                            |> snd in
          let dazed1 = cast avada dazed_enemy init_en |>
                       fst in 
          let dazed2 = cast avada dazed1 init_en |>
                       fst in 
          cast avada dazed2 init_en |> fst |> get_dazed            
        ) (0) );


    "hand after cast" >:: (fun _ -> 
        assert_equal (
          let drew1 = draw init_pl in  
          (hand_after_cast (List.hd (State.get_deck init_pl)) drew1) 
          |> get_hand |> 
          List.length)         
          0)  ; 


    "hand after draw" >:: (fun _ -> 
        assert_equal (
          let drew1 = draw init_pl in  
          (drew1) 
          |> get_hand |> 
          List.length)         
          1)  ;  

    "prolong effect 1 " >:: (fun _ ->
        assert_equal ( 
          cast crucio init_pl init_pl |> snd |> get_hp)
          (100 ) );

    "prolong effect 2 " >:: (fun _ ->
        assert_equal ( 
          let prolong1 =  cast crucio init_pl init_pl |> snd in
          let prolong2 = cast confingo init_pl prolong1 |> snd in
          prolong2 |> get_hp)
          (80) );

    "prolong effect 3 " >:: (fun _ ->
        assert_equal ( 
          let prolong1 =  cast crucio init_pl init_pl |> snd in
          let prolong2 = cast confingo init_pl prolong1 |> snd in
          let prolong3 = cast confingo init_pl prolong2 |> snd in 
          prolong3 |> get_hp)
          (60) );


    "remove test 1 " >:: (fun _  ->
        assert_equal ( (let drew = draw init_pl in 
                        cast expelliarmus init_pl drew ) |> snd 
                       |> get_hand |> List.length) 0 );  

    "remove test 2 " >:: (fun _  ->
        assert_equal ( (let drew = draw init_pl in 
                        let drew2 = draw drew in 
                        cast expelliarmus init_pl drew2 ) |> snd 
                       |> get_hand |> List.length) 1 );     

  ] 

let suite =
  "test suite for A6"  >::: List.flatten [
    hogwarts_tests;
    command_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite