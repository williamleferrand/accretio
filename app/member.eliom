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

}}

{server{

let get_details_ uid =
  lwt view = View_member.to_view uid in
  lwt statement = $member(uid)->statement in
  return (view, statement)

let get_details = server_function ~name:"member-get-details" Json.t<int> get_details_

let update_name name =
  Vault.protected_connected
    (fun session ->
       lwt _ = $member(session.member_uid)<-name %% (fun _ -> name) in
       lwt details = get_details_ session.member_uid in
       return (Some details))

let update_name = server_function ~name:"member-update-name" Json.t<string> update_name

let update_statement statement =
  Vault.protected_connected
    (fun session ->
       $member(session.member_uid)<-statement = statement ;
       lwt details = get_details_ session.member_uid in
       return (Some details))

let update_statement = server_function ~name:"member-update-statement" Json.t<string> update_statement

let update_profile_picture data =
  Vault.protected_connected
    (fun session ->
       match_lwt Ys_imagemagick.convert_from_b64 data with
         None -> return_none
       | Some file ->
         match_lwt Object_image.Store.create
                     ~owner:session.member_uid
                     ~content:(Object_image.File file)
                     () with
         | `Object_already_exists _ -> return_none
         | `Object_created image ->
           lwt _ =
             match_lwt $member(session.member_uid)->profile_picture with
             | None -> return_unit
             | Some uid -> lwt _ = $member(session.member_uid)<-album +=! (`ProfilePicture, uid) in return_unit
           in
           $member(session.member_uid)<-profile_picture = Some image.Object_image.uid ;
           lwt details = get_details_ session.member_uid in
           return (Some details))

let update_profile_picture = server_function ~name:"member-update-profile-picture" Json.t<string> update_profile_picture

let send_message (uid, text) =
  Vault.protected_connected
    (fun session ->
       lwt _ = Notify.send_direct_message session.member_uid uid text in
       return (Some ()))

let send_message = server_function ~name:"member-send-message" Json.t<int * string> send_message

}}

{client{

open React
open Eliom_content
open Eliom_content.Html5
open Eliom_content.Html5.D
open Ys_dummy
open Ys_react
open Service
open Leader

open View_member

let dom uid =
  let details, update_details = S.create None in

  ignore
    (lwt details = %get_details uid in
     update_details (Some details) ;
     return_unit) ;

  (* upload image box *)

  S.l2
    (fun session details ->
       match details with
       | None -> None
       | Some (member, statement) ->

         let profile_picture =

           let image, update_image = S.create member.profile_picture in
           let love =
             div ~a:[ a_class [ "member-love"  ; "left" ; "clearfix" ]] [
               div ~a:[ a_class [ "love-heart" ; "left" ]] [
                 span ~a:[ a_class [ "icon-heart" ]] []
               ] ;
               div ~a:[ a_class [ "love-count" ; "left" ;]] [ pcdata (string_of_int member.love) ]
             ] in
           let image =
             R.node
               (S.map
                  (function
                      None ->
                      div ~a:[ a_class [ "box-section" ; "profile-picture" ; "clearfix" ]] [
                        div ~a:[ a_class [ "profile-picture-image" ; "left" ]] [
                          span ~a:[ a_class [ "icon-user" ]] [] ;
                        ] ;
                        love ;
                      ]
                    | Some image ->
                   div ~a:[ a_class [ "box-section" ; "profile-picture"; "clearfix" ]] [
                     div ~a:[ a_class [ "profile-picture-image" ; "left" ]] [
                       match image.View_image.content with
                       | Object_image.File url -> img ~alt:"" ~src:(uri_of_string (fun () -> url)) ()
                       | Object_image.URIData data -> img ~alt:"" ~src:(uri_of_string (fun () -> data)) ()
                     ] ;
                     love
                     ])
               image)
           in

           match session with
           | Connected session when session.member_uid = uid ->
             let upload_box =
               Ys_darkroom.upload_box
                 (fun data finalizer ->
                    Authentication.if_connected
                      (fun session ->
                         rpc %update_profile_picture
                             data
                             (fun ((member, _) as details) ->
                                update_image member.profile_picture ;
                                update_details (Some details) ;
                                finalizer ())))
             in
             div ~a:[ a_class [ "box" ; "member-profile-picture" ]] [
               image ;
               upload_box ;
             ]

           | _ ->
             div ~a:[ a_class [ "box" ; "member-profile-picture" ]] [
               image
             ]
         in

         let name =
           match session with
           | Connected session when session.member_uid = uid ->
             let state, update_state = S.create `View in
             R.node
               (S.map
                  (function
                    | `View -> div ~a:[ a_class [ "box" ; "member-name" ]] [
                        div ~a:[ a_class [ "box-section" ]] [ pcdata member.name ] ;
                        div ~a:[ a_class [ "box-action" ]] [
                          button
                            ~button_type:`Button
                            ~a:[ a_onclick (fun _ -> update_state `Edit) ]
                            [ pcdata "Edit" ]
                        ]
                      ]
                    | `Edit ->
                      let input_name = input ~a:[ a_placeholder "Name" ] ~input_type:`Text ~value:member.name () in
                      let save _ =
                        match Ys_dom.get_value input_name with
                          "" -> Help.warning "Please enter a name"
                        | _ as name ->
                          Authentication.if_connected
                            (fun _ ->
                               rpc %update_name name
                                   (fun details ->
                                      update_details (Some details) ;
                                      update_state `View ;
                                      Ys_mixpanel.track "member-update-name" ~params:[ "uid", Ys_uid.to_string member.uid ;
                                                                                       "name", name ] () ;
                                      Ys_mixpanel.set_people [ "$name", name ]))
                      in
                      Ys_dom.register_on_return input_name save ;
                      let cancel = button ~button_type:`Button ~a:[ a_onclick (fun _ -> update_state `View) ] [ pcdata "Cancel" ] in
                      let save = button ~button_type:`Button ~a:[ a_onclick save ] [ pcdata "Save" ] in


                      div ~a:[ a_class [ "box" ; "member-name" ]] [

                        div ~a:[ a_class [ "box-section" ]] [
                          input_name
                        ] ;
                        div ~a:[ a_class [ "box-action" ]] [
                          cancel ; save
                        ]
                      ])
                  state)
           | _ ->
             div ~a:[ a_class [ "box"; "member-name" ]] [ div ~a:[ a_class [ "box-section" ]] [ pcdata member.name ]]
         in

         let statement =
           match statement, session with
           | _, Connected session when session.member_uid = uid ->
             let state, update_state = S.create (if statement = "" then `Edit else `View) in
             R.node
               (S.map
                  (function
                    | `Edit ->
                      let textarea = Raw.textarea ~a:[ a_placeholder "Describe yourself" ] (pcdata statement) in

                      let save _ =
                        match Ys_dom.get_value_textarea textarea with
                          "" -> Help.warning "Please describe yourself"
                        | _ as statement ->
                          Authentication.if_connected
                            (fun _ ->
                               rpc
                                   %update_statement
                                   statement
                                   (fun details ->
                                      update_details (Some details) ;
                                      update_state `View ;
                                      Ys_mixpanel.track "member-update-statement" ~params:[ "uid", Ys_uid.to_string member.uid ;
                                                                                            "statement", statement ] ()))
                      in

                      Ys_dom.register_on_return textarea save ;

                      let cancel = button ~button_type:`Button ~a:[ a_onclick (fun _ -> update_state `View) ] [ pcdata "Cancel" ] in
                      let save = button ~button_type:`Button ~a:[ a_onclick save ] [ pcdata "Save" ] in

                      div ~a:[ a_class [ "box" ; "member-statement" ]] [
                        div ~a:[ a_class [ "box-section" ]] [
                          textarea
                        ];
                        div ~a:[ a_class [ "box-action" ]]
                          (match statement with "" -> [ save ] | _ -> [ cancel ; save ])
                      ]
                    | `View ->
                      div ~a:[ a_class [ "box" ; "member-statement" ]] [
                        div ~a:[ a_class [ "box-section" ]] [ pcdata statement ] ;
                        div ~a:[ a_class [ "box-action" ]] [
                          button
                            ~button_type:`Button
                            ~a:[ a_onclick (fun _ -> update_state `Edit) ]
                            [ pcdata "Edit" ]
                        ]
                      ])
                  state)
           | "", _ -> pcdata ""
           | _ as statement, _ ->
             div ~a:[ a_class [ "box" ; "member-statement" ]] [ pcdata statement ]
         in

         let send_message =
           match session with
           | Connected session when session.member_uid = uid ->
             pcdata ""
           | _ ->
             let textarea = Raw.textarea (pcdata "") in
             let send _ =
               match Ys_dom.get_value_textarea textarea with
                 "" -> Help.warning "Please write a message"
               | _ as text ->
                 Authentication.if_connected ~mixpanel:("member-send-message", [ "member-uid", Ys_uid.to_string member.uid ;
                                                                                 "message", text ])
                   (fun session ->
                      rpc %send_message
                          (member.uid, text)
                          (fun () -> Ys_dom.set_value_textarea textarea "" ; Help.warning "Message sent"))
             in

             let send = button ~button_type:`Button ~a:[ a_onclick send ] [ pcdata "Send" ] in
             div ~a:[ a_class [ "box"; "member-send-message" ]] [
               h2 [ pcdata "Send a direct message" ] ;
               div ~a:[ a_class [ "box-section" ]] [
                 textarea
               ] ;
               div ~a:[ a_class [ "box-action" ]] [ send ] ;
               div ~a:[ a_class [ "box-section" ; "disclaimer" ]] [
                 pcdata "Sending a direct message will disclose your email address to the recipient"
               ]

             ]
         in

         let container = pcdata "" in


         Some
           (div ~a:[ a_class [ "member" ]] [
               profile_picture ;
               name ;
               statement ;
               send_message ;
               container ;
             ])
    )
    Sessions.session
    details


}}

(* new member page *)

{server{

let retrieve uid =
  lwt view = View_member.to_view uid in
  return (Some view)

let retrieve = server_function ~name:"member-retrieve" Json.t<int> retrieve

}}

{client{

let builder details =
  div ~a:[ a_class [ "member" ]] [


  ]

let dom =
  Template.apply
    %retrieve
    builder

}}
