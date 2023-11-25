open OUnit2

let sample_test _ =
  let x = 2 in
  assert_equal x 2

let tests =
  "day 1" >::: [
    "sample test" >:: sample_test
  ]
