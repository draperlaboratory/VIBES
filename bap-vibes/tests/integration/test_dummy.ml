open OUnit2

let test_dummy (_ : test_ctxt) : unit =
  assert_equal true true        

let suite = [
  "A dummy test" >:: test_dummy;
]
