
open Util
open UtilM
open Tests

let even x = x mod 2 = 0

let is_whitespace c = match c with
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let some_tests =
  [ check_test (fun () ->
        split_by even [1;2;3;5;6;7;8] = [ [1]; [3; 5]; [7] ])
       "split_by even [1;2;3;5;6;7;8] = [ [1]; [3; 5]; [7] ]";

    check_test (fun () ->
        split_by even [2;3;5;6;7;9;11] = [ [3; 5]; [7; 9; 11] ])
       "split_by even [2;3;5;6;7;9;11] = [ [3; 5]; [7; 9; 11] ]";

    check_test (fun () ->
        split_by is_whitespace (explode "Hi, there\n everyone. ") =
          [['H'; 'i'; ',']; ['t'; 'h'; 'e'; 'r'; 'e'];
           ['e'; 'v'; 'e'; 'r'; 'y'; 'o'; 'n'; 'e'; '.']])
       "split_by is_whitespace (explode \"Hi, there\n everyone. \") =
          [['H'; 'i'; ',']; ['t'; 'h'; 'e'; 'r'; 'e'];
           ['e'; 'v'; 'e'; 'r'; 'y'; 'o'; 'n'; 'e'; '.']])";

  ]


let run_tests tests = List.iter (fun f -> f ()) tests

let _ = run_tests some_tests
