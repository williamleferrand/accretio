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

(* this is not the usual society creation flow, but for the purpose of the
   experiment we can bend the rules a little bit *)

let name = "first-schoolbus-sf2"

let retrieve_or_create_society () =
  Lwt_log.ign_info_f "pulling the schoolbus society" ;
  match_lwt Object_society.Store.search_name name with
    uid :: _ -> return (Some uid)
  | [] ->
    Lwt_log.ign_info_f "the society doesn't exist yet, creating it" ;
    match_lwt Ys_shortlink.create () with
      None ->
      Lwt_log.ign_error_f "couldn't create shortlink" ;
      return_none
    | Some shortlink ->
      match_lwt Object_member.Store.find_by_email "william@accret.io" with
        None ->
        Lwt_log.ign_info_f "root doesn't exist" ;
        return_none
      | Some leader ->
        match_lwt Object_playbook.Store.search_name Children_schoolbus.name with
          [] ->
          Lwt_log.ign_info_f "schoolbus playbook isn't registered" ;
          return_none
        | playbook :: _ ->
          match_lwt Object_society.Store.create
                      ~shortlink
                      ~leader
                      ~name
                      ~description:"The first schoolbus experiment"
                      ~playbook
                      ~mode:Object_society.Public
                      ~data:[]
                    () with
          | `Object_already_exists (_, uid) -> return (Some uid)
          | `Object_created obj ->
            lwt _ = $member(leader)<-societies += (`Society, obj.Object_society.uid) in
            return (Some obj.Object_society.uid)

let request_more_info (email, notes) =
  match_lwt retrieve_or_create_society () with
    Some society ->
    Society_public.request_to_join_ (society, email, notes)
  | None ->
    Lwt_log.ign_error_f "panic: someone wanted to join the schoolbus activity with email %s, but we couldn't locate the society" email ;
    return_none

let request_more_info = server_function ~name:"schoolbus-request-more-info" Json.t<string * string> request_more_info

}}

{client{

open React
open Ys_react
open Eliom_content.Html5
open Eliom_content.Html5.D


(* the block that show the profiles *******************************************)



(* the /schoolbus page ********************************************************)

let builder () =

  let form =

    let email = input ~a:[ a_input_type `Text ; a_placeholder "What is your email?" ] () in
    let notes = Raw.textarea ~a:[ a_placeholder "(Optional) How old is your child (or are your children)? In which neighborhood do you live?" ] (pcdata "") in
    let send _ =
      let email_ = Ys_dom.get_value email in
      match Ys_email.is_valid email_ with
        false -> Help.warning "Please enter a valid email address"
      | true ->
        detach_rpc %request_more_info (email_, Ys_dom.get_value_textarea notes) (fun _ ->
            Ys_dom.set_value email "" ;
            Ys_dom.set_value_textarea notes "" ;
            Help.warning "Thanks, we will be in touch!")
    in
    let send =
      button
        ~a:[ a_button_type `Button ;
             a_onclick send ]
        [ pcdata "Keep me posted" ]
    in
    div ~a:[ a_class [ "form" ]] [
      div ~a:[ a_class [ "box" ]] [
        h3 [ pcdata "We are experimenting. Do you want to learn more?" ] ;
        div ~a:[ a_class [ "box-section" ]] [ email ] ;
        div ~a:[ a_class [ "box-section" ]] [ notes ] ;
        div ~a:[ a_class [ "box-action" ]] [ send ] ;
      ]
    ]
  in

  div ~a:[ a_class [ "schoolbus" ]] [
    h1 [ pcdata "The preschool on wheels" ] ;
    div ~a:[ a_class [ "pitch" ; "clearfix" ]] [
      div ~a:[ a_class [ "bus" ; "right" ]] [
        Raw.img ~src:(uri_of_string (fun () -> "img/school_bus.png")) ~alt:"" ()
      ] ;
      ol ~a:[ a_class [ "left" ]] [
        li [ pcdata "We pick you and your child up at your doorstep" ] ;
        li [ pcdata "We head out to exciting activities (zoo, music classes, ...)" ] ;
        li [ pcdata "We drive you back after a rich day of learning and playing" ] ;
      ] ;
    ] ;

    form ;

    div ~a:[ a_class [ "faq" ]] [
      h2 [ pcdata "F.A.Q" ] ;
      div [
        h3 [ pcdata "What kind of activities do you organize?" ] ;
        pcdata "Activities will include field trips to the SF Zoo and to the Discovery Museum in Sausalito. Depending on people's interests, we will also schedule music classes and art workshops with public and private providers, as well as laid-back playtimes at our beautiful public playgrounds."
      ] ;
      (* div [
        h3 [ pcdata "How much does it cost?" ] ;
        pcdata "The exact cost depends on the activities and the number of participants, but we initially aim at less than $80 for a whole day of activity for one children and one adult, lunch included."
      ] ; *)
      div [
        h3 [ pcdata "Who operates the bus?" ] ;
        pcdata "We are contracting with a well-known, fully insured charter bus company."
      ] ;
      div [
        h3 [ pcdata "Do I have to accompany my child?" ] ;
        pcdata "Yes, we are experimenting with the service and we would like to share these first steps with you. We are planning to involve professional caregivers once we validate the concept for those of us who would like to let our children ride alone."
      ] ;
    ] ;
  ]

let dom () =
  S.const (Some (builder ()))

}}
