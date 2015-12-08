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
open Sessions
open Ys_uid
open Vault
open View_member

}}

{server{
(*

let love (cohort, uid) =
  protected_connected
    (fun session ->
       Lwt_log.ign_info_f "member %d sends love to member %d for cohort %d"
         session.member_uid
         uid
         cohort ;
       lwt _ = $member(uid)<-reviews += (`ThanksForCohort cohort, session.member_uid) in
       return (Some ()))

let love = server_function ~name:"member-love" Json.t<int * int> love

*)

}}

{client{

open React
open Ys_react
open Eliom_content.Html5
open Eliom_content.Html5.D

let format_transition view =
  div ~a:[ a_class [ "left" ]] [
    pcdata view.name
  ]


let format_thumbnail view =
  div ~a:[ a_class [ "cohort-leader" ] ] [
    div ~a:[ a_class [ "leader-picture";] ; a_onclick (fun _ -> Service.goto (Service.Member view.uid)) ] [
      (match view.profile_picture with
         None -> span ~a:[ a_class [ "icon-user" ]] []
       | Some image ->
         match image.View_image.content with
         | Object_image.File url -> img ~alt:"" ~src:(uri_of_string (fun () -> url)) ()
         | Object_image.URIData data -> img ~alt:"" ~src:(uri_of_string (fun () -> data)) ()) ;
    ]
  ]



(*
let format_story_owner story view =
  let is_loved, update_is_loved = S.create view.is_loved in
  let love_count, update_love_count = S.create view.love in
  let love _ =
    if view.is_loved then
      ()
    else
      Authentication.if_connected ~mixpanel:("member-love-story", [ "member-uid", Ys_uid.to_string view.uid ;
                                                                     "story-uid", Ys_uid.to_string story ])
        (fun session ->
           rpc
           %love
                 (story, view.uid)
                 (fun () ->
                    view.love <- view.love + 1 ;
                    view.is_loved <- true ;
                    update_is_loved true ;
                    update_love_count view.love ))
  in

  div ~a:[ a_class [ "leader-picture" ] ; a_onclick (fun _ -> Service.goto (Service.Profile view.uid)) ] [
    (match view.profile_picture with
       None -> span ~a:[ a_class [ "icon-user" ]] []
     | Some image ->
       match image.View_image.content with
       | Object_image.File url -> img ~alt:"" ~src:(uri_of_string (fun () -> url)) ()
       | Object_image.URIData data -> img ~alt:"" ~src:(uri_of_string (fun () -> data)) ()) ;
  ]


*)

}}
