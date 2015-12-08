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

type content = Text of string | Image of View_image.t

type message = {
  author : View_member.t ;
  timestamp : timestamp ;
  content : content ;
}

let message_to_string message =
  match message.content with
    Text t -> t
  | Image _ -> "image"

type t = {
  uid : uid ;
  subject : string ;
  owner : View_member.t ;
  created_on : timestamp ;
  number_of_messages : int ;
  mutable messages : message list ;
}

}}


{server{

let convert_message message =
  lwt author = View_member.to_view message.Object_thread.author in
  lwt content =
    match message.Object_thread.content with
    | Object_thread.Text text -> return (Text text)
    | Object_thread.Image uid ->
      lwt view = View_image.to_view uid in
      return (Image view)
  in
  return {
    author;
    timestamp = message.Object_thread.timestamp ;
    content
  }

let to_view uid =
  lwt owner, subject, created_on, number_of_messages, messages = $thread(uid)->(owner, subject, created_on, number_of_messages, messages) in
  lwt owner = View_member.to_view owner in
  lwt messages = Lwt_list.map_p convert_message messages in
  return { uid ; subject; owner ; created_on; number_of_messages ; messages }

}}
