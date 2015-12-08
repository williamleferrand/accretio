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


{shared{

open Lwt
open Ys_uid
open Ys_types

type t = {
  uid : uid ;
  content : Object_image.content
}

}}

{server{

let to_view uid =
  lwt content =
    match_lwt $image(uid)->content with
      Object_image.File file ->
      let file = Filename.basename file in
      return (Object_image.File ("/" ^ file))
    | _ as content -> return content
  in

  return { uid ; content }

}}


{client{

open Eliom_content

}}
