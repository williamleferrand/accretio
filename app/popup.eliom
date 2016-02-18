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


{client{

open Lwt
open React

open Eliom_content.Html5
open Eliom_content.Html5.D

let anchor, update_anchor : Html5_types.div_content_fun elt option React.signal * ( ?step:React.step -> Html5_types.div_content_fun elt option -> unit) = S.create None

let show ?(cls=[]) ~label content =
  Ys_mixpanel.track
    "popup-show"
    ~params:[ "label", label ]
    () ;
  Dom_html.window##scroll(0, 1) ;
  update_anchor
    (Some (div ~a:[ a_class ("popup" :: cls) ] content))

let close _ =
  (match S.value anchor with
     Some _ -> Ys_mixpanel.track "popup-close" ()
   | None -> ()) ;
  update_anchor None

let close_and_yield _ =
  close () ;
  Lwt_js.yield ()

(* predefined popups *)

let ask question_old callback =
  let button_yes =
    button
      ~a:[ a_onclick (fun _ -> update_anchor None ;
                       Ys_mixpanel.track "popup-ask-yes" ~params:[ "question_old", question_old ] () ;
                       callback `Yes) ]
      [ pcdata "Yes" ]
  and button_no =
    button
      ~a:[ a_onclick (fun _ -> update_anchor None ;
                       Ys_mixpanel.track "popup-ask-no" ~params:[ "question_old", question_old ] () ;
                       callback `No ) ]
      [ pcdata "No" ] in
  show ~cls:[ "box" ] ~label:("question_old:"^question_old) [
    div ~a:[ a_class [ "content" ]] [ pcdata question_old ] ;
    div ~a:[ a_class [ "box-action" ]] [
      button_no ; button_yes
    ]
  ]

}}
