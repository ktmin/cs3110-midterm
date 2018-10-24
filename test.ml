open OUnit2
open Hogwarts 
open State
open Command 


let command_tests = [
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








