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

(* Ideally code here should only rely on the API signature as this is intended
   to be part of the playbook .. *)

{shared{

open Lwt
open Sessions
open Ys_uid
open Vault

}}

{server{

let retrieve_activities_ uid =
  lwt activities = $society(uid)->activities in
  let activities = List.map snd activities in
  lwt activities =
    Lwt_list.filter_s
      (fun uid ->
         match_lwt $activity(uid)->state with
          | Object_activity.Drafting | Object_activity.Proposing -> return_true
          | _ -> return_false)
      activities
  in
  lwt activities = Lwt_list.map_s View_activity.to_view activities in
  return activities

let retrieve_activities = server_function ~name:"fieldtrip-curriculum-retrieve-activities" Json.t<int> retrieve_activities_

let suggest society member suggestion =
  let stage = Fieldtrips.Stages.handle_suggestion_inbox in
  match_lwt
    Object_message.Store.create
      ~origin:(Object_message.Member member)
      ~destination:(Object_message.Society (society, stage))
      ~content:suggestion
      ~subject:"Field trip suggestion"
      ~reference:(Object_message.create_reference suggestion)
      () with
  | `Object_already_exists _ -> return None
  | `Object_created message ->
    lwt _ = $member(member)<-messages += (`Email, message.Object_message.uid) in
    lwt _ = $society(society)<-inbox += ((`Message Object_society.({ received_on = Ys_time.now () ; read = false })), message.Object_message.uid) in

    lwt playbook = $society(society)->playbook in
    let module Playbook = (val (Registry.get playbook) : Api.PLAYBOOK) in

    lwt _ =
      match_lwt Playbook.dispatch_message_automatically message.Object_message.uid stage with
        None -> return_unit
      | Some call ->
        lwt _ = $message(message.Object_message.uid)<-action = Object_message.RoutedToStage call.Ys_executor.stage in
        lwt _ = $society(society)<-stack %% (fun stack -> call :: stack) in
        Lwt_log.ign_info_f "messaged %d was stacked on society %d" message.Object_message.uid society ;
        return_unit
    in

    return (Some ())

let suggest_by_email (society, email, suggestion) =
  lwt member =
    match_lwt Object_member.Store.find_by_email email with
    | Some uid -> return uid
    | None ->
      match_lwt Object_member.Store.create
                  ~preferred_email:email
                  ~emails:[ email ]
                         () with
      | `Object_already_exists (_, uid) -> return uid
      | `Object_created member -> return member.Object_member.uid
  in
  suggest society member suggestion

let suggest_direct (society, suggestion) =
  protected_connected
    (fun session -> suggest society session.member_uid suggestion)

let suggest_by_email = server_function ~name:"fieldtrip-curriculum-suggest-by-email" Json.t<int * string * string> suggest_by_email

let suggest_direct = server_function ~name:"fieldtrip-curriculum-suggest-direct" Json.t<int * string> suggest_direct

let update_activity (society, activity, title, description) =
  protected_connected
    (fun session ->
       (* todo: add ACL + logging here *)
       Lwt_log.ign_info_f "member %d updated activity %d with title %s, description %s" session.member_uid activity title description ;
       lwt _ = $activity(activity)<-title = title in
       lwt _ = $activity(activity)<-description = description in
       lwt activities = retrieve_activities_ society in
       return (Some activities))

let propose_activity (society, activity, title, description) =
  protected_connected
    (fun session ->
       (* todo: add ACL + logging here *)
       Lwt_log.ign_info_f "member %d proposed activity %d with title %s, description %s" session.member_uid activity title description ;
       lwt _ = $activity(activity)<-title = title in
       lwt _ = $activity(activity)<-description = description in
       lwt _ = $activity(activity)<-state = Object_activity.Proposing in

       (* we trigger the proposing state for this society *)
       lwt _ = Executor.stack_int society Fieldtrips.Stages.propose_activity activity in

       lwt activities = retrieve_activities_ society in
       return (Some activities))

let cancel_activity (society, activity, title, description) =
  protected_connected
    (fun session ->
       (* todo: add ACL + logging here *)
       Lwt_log.ign_info_f "member %d cancelled activity %d with title %s, description %s" session.member_uid activity title description ;
       lwt _ = $activity(activity)<-title = title in
       lwt _ = $activity(activity)<-description = description in
       lwt _ = $activity(activity)<-state = Object_activity.Cancelled in
       lwt activities = retrieve_activities_ society in
       return (Some activities))

let draft_activity (society, activity, title, description) =
  protected_connected
    (fun session ->
       (* todo: add ACL + logging here *)
       Lwt_log.ign_info_f "member %d drafted activity %d with title %s, description %s" session.member_uid activity title description ;
       lwt _ = $activity(activity)<-title = title in
       lwt _ = $activity(activity)<-description = description in
       lwt _ = $activity(activity)<-state = Object_activity.Drafting in
       lwt activities = retrieve_activities_ society in
       return (Some activities))

let update_activity = server_function ~name:"fieldtrip-curriculum-update-activity" Json.t<int * int * string * string> update_activity
let propose_activity = server_function ~name:"fieldtrip-curriculum-propose-activity" Json.t<int * int * string * string> propose_activity
let cancel_activity = server_function ~name:"fieldtrip-curriculum-cancel-activity" Json.t<int * int * string * string> cancel_activity
let draft_activity = server_function ~name:"fieldtrip-curriculum-draft-activity" Json.t<int * int * string * string> draft_activity

}}

{client{

open React
open Ys_react
open Eliom_content.Html5
open Eliom_content.Html5.D

open View_activity

(* the dom ******************************************************************************)

let builder uid activities =

  let activities = RList.init activities in

  let suggest =

    let email = input ~a:[ a_input_type `Text ; a_placeholder "Your email" ] () in

    let suggestion =
      Raw.textarea
        ~a:[ a_placeholder "What is your suggestion? Give as many details as possible, dates, cost estimates, group sizes" ]
        (pcdata "")
    in

    let your_email =
      S.map
        (function
          | Anonymous ->
            div ~a:[ a_class [ "box-section" ]] [
              email
            ]
          | _ -> pcdata "")
        Sessions.session
    in

    let suggest _ =
      let finalize _ =
        Ys_dom.set_value_textarea suggestion "" ;
        Help.warning "Thanks!"
      in

      match S.value Sessions.session with
        Anonymous ->
        begin
          match Ys_dom.get_value email with
            "" -> Help.error "Please enter your email"
          | _ as email ->
            match Ys_dom.get_value_textarea suggestion with
              "" -> Help.error "Please enter a suggestion"
            | _ as suggestion ->
              detach_rpc %suggest_by_email (uid, email, suggestion) finalize
        end
      | Connected sesssion ->
        match Ys_dom.get_value_textarea suggestion with
          "" -> Help.error "Please enter a suggestion"
        | _ as suggestion ->
          detach_rpc %suggest_direct (uid, suggestion) finalize
    in

    let suggest =
      button
        ~a:[ a_button_type `Button ;
             a_onclick suggest ]
        [ pcdata "Suggest" ]
    in

    div ~a:[ a_class [ "box" ; "suggest" ]] [
      h3 [ pcdata "Make a suggestion" ] ;
      div ~a:[ a_class [ "box-section" ]] [ suggestion ] ;
      R.node your_email ;
      div ~a:[ a_class [ "box-action" ]] [ suggest ] ;
    ]
  in


  let format activity =

    match activity.state with
      Drafting ->

      let title = input ~a:[ a_input_type `Text ; a_value activity.title ] () in
      let description = Raw.textarea (pcdata activity.description) in

      let update _ =
        detach_rpc %update_activity (uid, activity.uid, Ys_dom.get_value title, Ys_dom.get_value_textarea description) (RList.update activities)
      in

      let propose _ =
        detach_rpc %propose_activity (uid, activity.uid, Ys_dom.get_value title, Ys_dom.get_value_textarea description) (RList.update activities)
      in

      let cancel _ =
        detach_rpc %cancel_activity (uid, activity.uid, Ys_dom.get_value title, Ys_dom.get_value_textarea description) (RList.update activities)
      in

      let cancel =
        button
          ~a:[ a_onclick cancel ]
          [ pcdata "Cancel" ]
      in

      let propose =
        button
          ~a:[ a_onclick propose ]
          [ pcdata "Propose" ]
      in

      let update =
        button
          ~a:[ a_onclick update ]
          [ pcdata "Update" ]
      in

      div ~a:[ a_class [ "box" ; "activity" ]] [
        div ~a:[ a_class [ "box" ;]] [
          h3 [ pcdata "Drafting" ] ;
          div ~a:[ a_class [ "box-section" ]] [
            title ;
          ] ;
          div ~a:[ a_class [ "box-section"] ; a_style "white-space:pre-line;" ] [
            description ;
          ] ;
          div ~a:[ a_class [ "box-action" ]] [
            update ; cancel ; propose
          ] ;
        ] ;
        Lambda_thread.format activity.thread
      ]
    | Proposing ->

      let cancel _ =
        detach_rpc %cancel_activity (uid, activity.uid, activity.title, activity.description) (RList.update activities)
      in

      let propose _ =
        detach_rpc %propose_activity (uid, activity.uid, activity.title, activity.description) (RList.update activities)
      in

      let draft _ =
        detach_rpc %draft_activity (uid, activity.uid, activity.title, activity.description) (RList.update activities)
      in

      let cancel =
        button
          ~a:[ a_onclick cancel ]
          [ pcdata "Cancel" ]
      in

      let draft =
        button
          ~a:[ a_onclick draft ]
          [ pcdata "Draft" ]
      in

      let propose =
        button
          ~a:[ a_onclick propose ]
          [ pcdata "Propose again" ]
      in

      div ~a:[ a_class [ "box" ; "activity" ]] [
        div ~a:[ a_class [ "box" ;]] [
          h3 [ pcdata "Proposing" ] ;
          div ~a:[ a_class [ "box-section" ]] [
            pcdata activity.title
          ] ;
          div ~a:[ a_class [ "box-section" ] ; a_style "white-space:pre-line;" ] [
            pcdata activity.description
          ] ;
          div ~a:[ a_class [ "box-action" ]] [
            draft ; cancel ; propose
          ] ;
        ] ;
        Lambda_thread.format activity.thread
      ]

    | _ -> pcdata "other"
  in
  let activities =
    RList.map_in_div ~a:[ a_class [ "activities" ]] format activities
  in

  div ~a:[ a_class [ "fieldtrip-curriculum" ]] [
    suggest ;
    activities
  ]

let dom uid = Template.apply %retrieve_activities (builder uid) uid

}}
