(*
 * Accretio is an API, a sandbox and a runtime for social playbooks
 *
 * Copyright (C) 2015 William Le Ferrand
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


open Lwt

let script = "scripts/email_reply_parser.rb"

(* extract the last reply using the github result *****************************)

let parse_reply string =
  lwt reply =
    Lwt_stream.to_list
      (Lwt_process.pmap_lines
         ("ruby", [| "ruby" ; script |])
         (Lwt_stream.of_array [| string |])) in

  return (String.concat "\n" reply)


(* extract all the emails from the string that are under <coucou@biib.com> ****)

let regex = Str.regexp "[-+_\.a-zA-Z]+@[- _ a-z A-Z 0-9]+\.[a-z A-Z 0-9]+"

let grab_emails string =
  let splitted = Str.full_split regex string in
  List.fold_left
    (fun acc -> function Str.Delim delim -> delim :: acc | _ -> acc)
    []
    splitted
