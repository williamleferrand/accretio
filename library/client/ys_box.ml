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


open React

open Eliom_content.Html5
open Eliom_content.Html5.D

let make ?(closable=false) ?(onclose=(fun () -> ())) ?onclick ?(cls=[]) content =
  match closable with
  | false ->
    div
      ~a:(match onclick with
            None -> [ a_class ("box" :: cls) ]
          | Some onclick -> [ a_onclick onclick; a_class ("box" :: cls) ])
      content
  | true ->
    begin
      let is_visible, update_value = S.create true in
      let close =
        div ~a:[  a_class [ "close" ]] [
          button
            ~button_type:`Button
            ~a:[ a_onclick (fun _ -> onclose () ; update_value false) ] [
            pcdata "Got it"
          ]
        ]
      in
      R.node
        (S.map
           (function
             | true ->
               div
                 ~a:(match onclick with
                       None -> [ a_class ("box" :: cls) ]
                     | Some onclick -> [ a_onclick onclick; a_class ("box" :: cls) ])
                 (content@[close])
             | false -> pcdata "")
           is_visible)
    end



let box cl content =
  div ~a:[ a_class [ cl ; "box" ]] content

let title content =
  h2 [ pcdata content ]

let subtitle content =
  h3 [ pcdata content ]

let section content =
  div ~a:[ a_class [ "box-section" ]] content

let action content =
  div ~a:[ a_class [ "box-action" ]] content
