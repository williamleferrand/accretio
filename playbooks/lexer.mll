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

{

open Parser        (* The type token is defined in parser.mli *)
exception Eof

}


rule breadmaking_leader_message = parse
  [' ' '\t' '\n' ]                { breadmaking_leader_message lexbuf }     (* skip blanks *)
  | "not"       { NOT }
  | "available" { AVAILABLE }
  | "run" { RUN }
  | "summarize" { SUMMARIZE }
  | "pause" { PAUSE }
  | "invite" { INVITE }
  | "activate" { ACTIVATE }
| "yes"                           { YES }
| "no"                            { NO }
  | [^' ' '\t' '\n' ]+ as lxm       { Printf.printf "found lxm: %s\n" lxm ; flush stdout ; IDENT(lxm) }
  | eof                             { EOF }

and breadmaking_member_message = parse
  [' ' '\t' '\n' ]                { breadmaking_member_message lexbuf }     (* skip blanks *)
| "yes"                           { YES }
| "no"                            { NO }
| [^' ' '\t' '\n' ]+ as lxm       { IDENT(lxm) }
| eof                             { EOF }

and r00t_leader_response = parse
  [' ' '\t' '\n' ]                { r00t_leader_response lexbuf }     (* skip blanks *)
| "run"                           { RUN }
| [ '0'-'9' ]+ as lxm                  { INT (int_of_string lxm) }
| "create_context"                { CREATE_CONTEXT }
| "create_and_assign_context"     { CREATE_AND_ASSIGN_CONTEXT }
| "summarize"                     { SUMMARIZE }
| "activate"                      { ACTIVATE }
| "pause"                         { PAUSE }
| "yes"                           { YES }
| "no"                            { NO }
| [^' ' '\t' '\n' ]+ as lxm       { IDENT(lxm) }
| eof                             { EOF }


and library_message_yes_no = parse
| "yes"                           { YES }
| "Yes"                           { YES }
| "no"                            { NO }
| "No"                            { NO }
| eof                             { EOF }
| _                               { library_message_yes_no lexbuf }
