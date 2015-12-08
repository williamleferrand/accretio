(*
 * /ys/lib is an shared library for accretio
 * Copyright (C) 2014 William Le Ferrand
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open OUnit2
open Language

(* the tests ******************************************************************)

let test_analyse_function_call _ =
  let lexbuf = Lexing.from_string "run turn_all_senders_into_members" in
  let result = Parser.main Lexer.token lexbuf in
  assert_equal result (Run ("turn_all_senders_into_members", []))

let test_analyse_function_call_with_text_around _ =
  let lexbuf = Lexing.from_string "hi r00t, please run turn_all_senders_into_members" in
  let result = Parser.main Lexer.token lexbuf in
  assert_equal result (Run ("turn_all_senders_into_members", []))

let test_analyse_api_create_context _ =
  let lexbuf = Lexing.from_string "hi r00t, please run create_context playbook toto, thanks!" in
  let result = Parser.main Lexer.token lexbuf in
  match result with
  | Closure _ -> assert_bool "" true
  | _ -> assert_bool "" false


let test_reply_parser _ =
  let emails = Ys_reply_parser.grab_emails "<warnegia@gmail.com> or william.le-ferrand@polytechnique.edu" in
  Printf.printf "number of emails found: %d\n" (List.length emails) ;
  List.iter (fun email -> Printf.printf "%s\n" email) emails ;
  assert_equal emails [ "william.le-ferrand@polytechnique.edu" ; "warnegia@gmail.com" ]

(* the suite ******************************************************************)

let suite =
  "Mu language" >::: [ "test_analyse_function_call" >:: test_analyse_function_call ;
                       "test_reply_parser" >:: test_reply_parser ;
                       "test_analyse_function_call_with_text_around" >:: test_analyse_function_call_with_text_around ]


 let _ =
   run_test_tt_main suite
