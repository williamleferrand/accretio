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

let simple_yes_no message =
  lwt content = $message(message)->content in
  let lexbuf = Lexing.from_string content in
  match Parser.library_message_yes_no Lexer.library_message_yes_no lexbuf with
  | `Yes -> return (Some (`Yes message))
  | `No -> return (Some (`No message))
  | `Unknown -> return_none

let forward message =
  return (Some (`Message message))

let content message =
  Lwt_log.ign_info_f "getting content from %d" message ;
  lwt content = $message(message)->content in
  Lwt_log.ign_info_f "content found" ;
  return (Some (`Content content))
