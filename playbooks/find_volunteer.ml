(*
 * find a volunteer in a group
 *
 * william@accret.io
 *
 *)

open Lwt

open Printf
open CalendarLib

open Api

open Eliom_content.Html5
open Eliom_content.Html5.D

open Message_parsers


let key_run_id = "find-volunteer-run-id"
let key_tagline = "find-volunteer-tagline"

let tag_refused run_id = sprintf "refused%Ld" run_id
let tag_timer run_id member = sprintf "waiting%Ld%d" run_id member

let alert_supervisor context () =
  context.log_info "alerting supervisor, manual action is needed" ;
  lwt _ =
    context.message_supervisor
      ~subject:"Manual action is needed"
      ~content:[
        pcdata "Hi," ; br () ;
        br () ;
        pcdata "Please connect to your dashboard and check the society. You can also use this direct link:" ; br () ;
        br () ;
        Raw.a ~a:[ a_href (uri_of_string  (fun () -> context.direct_link)) ] [ pcdata context.direct_link ] ; br ();
        br () ;
      ]
  in
  return `None

let find_volunteer_with_tagline context tagline =
  let run_id = Ys_time.now () in
  lwt _ = context.set ~key:key_run_id ~value:(Int64.to_string run_id) in
  lwt _ = context.set ~key:key_tagline ~value:tagline in
  return `FindCandidate

let look_for_candidate context () =
  match_lwt context.get ~key:key_run_id with
    None -> return `AlertSupervisor
  | Some run_id ->
    let run_id = Int64.of_string run_id in
    match_lwt context.search_members ~query:("active -" ^ (tag_refused run_id)) () with
      [] -> return `NoVolunteer
    | _ as participants ->
      let member = List.nth participants (Random.int (List.length participants)) in
      lwt tagline =
        match_lwt context.get ~key:key_tagline with
          None -> return ""
        | Some tagline -> return tagline
      in
      context.log_info "asking member %d to volunteer" member ;
      lwt _ =
        context.message_member
          ~subject:"Would you like to organize the next dinner?"
          ~member
          ~content:[
            pcdata "Hi," ; br () ;
            br () ;
            span [ pcdata tagline ] ; br () ;
          ]
      in
      lwt _ =
        context.set_timer
          ~label:(tag_timer run_id member)
          ~duration:(Calendar.Period.lmake ~hour:24 ())
          (`CandidateDidntReplied member)
      in
      return `None

let candidate_didnt_replied context member =
  context.log_info "candidate %d didn't replied" member ;
  match_lwt context.get ~key:"find-volunteer-run-id" with
    None -> return `AlertSupervisor
  | Some run_id ->
    let run_id = Int64.of_string run_id in
    lwt _ = context.tag_member ~member ~tags:[ tag_refused run_id ] in
    return `FindCandidate

let candidate_declined context message =
  match_lwt context.get ~key:key_run_id with
    None -> return `AlertSupervisor
  | Some run_id ->
    let run_id = Int64.of_string run_id in
    lwt member = context.get_message_sender ~message in
    context.log_info "candidate %d declined" member ;
    lwt _ = context.tag_member ~member ~tags:[ tag_refused run_id ] in
    lwt _ = context.cancel_timers ~query:(tag_timer run_id member) in
    return `FindCandidate

let return_volunteer context email =
  lwt member = context.get_message_sender email in
  lwt _ =
    match_lwt context.get ~key:key_run_id with
      None -> return_unit
    | Some run_id ->
      let run_id = Int64.of_string run_id in
      context.cancel_timers ~query:(tag_timer run_id member)
  in
  context.log_info "volunteer found, returning %d" member ;
  return (`Volunteer member)

let candidate_with_message context message =
  lwt member = context.get_message_sender ~message in
  lwt _ =
    match_lwt context.get ~key:key_run_id with
      None -> return_unit
    | Some run_id ->
      let run_id = Int64.of_string run_id in
      context.cancel_timers ~query:(tag_timer run_id member)
   in
   return (`Message message)


COMPONENT

                                                                                       candidate_declined ~> `AlertSupervisor ~> alert_supervisor
find_volunteer_with_tagline ~> `FindCandidate ~> look_for_candidate ~> `No of email ~> candidate_declined ~> `FindCandidate ~> look_for_candidate
                                                 look_for_candidate ~> `AlertSupervisor ~> alert_supervisor
                                                 look_for_candidate ~> `CandidateDidntReplied of int ~> candidate_didnt_replied ~> `FindCandidate ~> look_for_candidate
                                                                                                        candidate_didnt_replied ~> `AlertSupervisor ~> alert_supervisor
                                                 look_for_candidate ~> `Yes of email ~> return_volunteer
                                                                                        return_volunteer ~> `AlertSupervisor ~> alert_supervisor
                                                 look_for_candidate ~> `Message of email ~> candidate_with_message
