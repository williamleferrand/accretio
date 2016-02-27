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
open Toolbox

let key_run_id = "find-volunteer-run-id"
let key_tagline = "find-volunteer-tagline"
let key_volunteer = sprintf "find-volunteer-volunteer-%Ld"
let tag_refused run_id = sprintf "refused%Ld" run_id
let tag_timer run_id member = sprintf "waiting%Ld%d" run_id member

let alert_supervisor context () =
  context.log_info "alerting supervisor, manual action is needed" ;
  lwt _ =
    context.message_supervisor
      ~subject:"Manual action is needed"
      ~content:[
        pcdata "Greetings," ; br () ;
        br () ;
        pcdata "Please connect to your dashboard and check the society. You can also use this direct link:" ; br () ;
        br () ;
        Raw.a ~a:[ a_href (uri_of_string  (fun () -> context.direct_link)) ] [ pcdata context.direct_link ] ; br ();
        br () ;
      ]
      ()
  in
  return `None

let find_volunteer_with_tagline context tagline =
  let run_id = Ys_time.now () in
  lwt _ = context.set ~key:key_run_id ~value:(Int64.to_string run_id) in
  lwt _ = context.set ~key:key_tagline ~value:tagline in
  return `FindCandidate

let find_volunteer_with_tagline_and_hint context (tagline, member) =
  let run_id = Ys_time.now () in
  lwt _ = context.set ~key:key_run_id ~value:(Int64.to_string run_id) in
  lwt _ = context.set ~key:key_tagline ~value:tagline in
  context.log_info "asking member %d to volunteer" member ;
  lwt _ =
    context.message_member
      ~subject:"Would you like to organize the next dinner?"
      ~data:[ key_run_id, Int64.to_string run_id ]
      ~member
      ~content:[
        pcdata "Greetings," ; br () ;
        br () ;
        span [ pcdata tagline ] ; br () ;
      ]
      ()
  in
  lwt _ =
    context.set_timer
      ~label:(tag_timer run_id member)
      ~duration:(Calendar.Period.lmake ~hour:24 ())
      (`CandidateDidntReplied (member, run_id))
  in
  return `None

let look_for_candidate context () =
  match_lwt context.get ~key:key_run_id with
    None -> return `AlertSupervisor
  | Some run_id ->
    let run_id = Int64.of_string run_id in
    match_lwt context.search_members ~query:("hasparticipated -" ^ (tag_refused run_id)) () with
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
          ~data:[ key_run_id, Int64.to_string run_id ]
          ~member
          ~content:[
            pcdata "Greetings," ; br () ;
            br () ;
            span [ pcdata tagline ] ; br () ;
          ]
          ()
      in
      lwt _ =
        context.set_timer
          ~label:(tag_timer run_id member)
          ~duration:(Calendar.Period.lmake ~hour:24 ())
          (`CandidateDidntReplied (member, run_id))
      in
      return `None

let candidate_didnt_replied context (member, run_id) =
  context.log_info "candidate %d didn't replied in run %Ld" member run_id ;
  lwt _ = context.tag_member ~member ~tags:[ tag_refused run_id ] in
  (* here we need to notify the candidate that it isn't his turn anymore *)
  lwt salutations = salutations member in
  lwt tagline =
    match_lwt context.get ~key:key_tagline with
      None -> return ""
    | Some tagline -> return tagline
  in
  lwt _ =
    context.message_member
      ~member
      ~subject:"Next time?"
      ~data:[ key_run_id, Int64.to_string run_id ]
      ~content:[
        salutations ; br () ;
        br () ;
        pcdata "I didn't heard back from you regarding my message below but no worries I will ask someone else!" ; br () ;
        br () ;
        pcdata "I you want to leave the group, let me know. Otherwise, you will keep receiving updates." ; br () ;
        br () ;
        pcdata "--- " ; br () ;
        br () ;
        span [ pcdata tagline ] ; br () ;
      ]
      ()
  in
  return `FindCandidate

let remove_candidate context message =
  lwt member = context.get_message_sender ~message in
  lwt _ = context.remove_member ~member in
  lwt _ =
    context.reply_to
      ~message
      ~content:[
        pcdata "I'm sorry to see you go!" ; br () ;
      ]
      ()
  in
  return `None

let candidate_declined context message =
  match_lwt context.get_message_data ~message ~key:key_run_id with
    None -> return `AlertSupervisor
  | Some run_id ->
    let run_id = Int64.of_string run_id in
    lwt member = context.get_message_sender ~message in
    context.log_info "candidate %d declined run is %Ld" member run_id ;
    lwt _ = context.tag_member ~member ~tags:[ tag_refused run_id ] in
    lwt _ = context.cancel_timers ~query:(tag_timer run_id member) in
    return `FindCandidate

let return_volunteer context message =
  lwt member = context.get_message_sender ~message in
  lwt _ =
    match_lwt context.get_message_data ~message ~key:key_run_id with
      None -> return_unit
    | Some run_id ->
      let run_id = Int64.of_string run_id in
      lwt _ = context.set ~key:(key_volunteer run_id) ~value:(string_of_int member) in
      context.cancel_timers ~query:(tag_timer run_id member)
  in
  context.log_info "volunteer found, returning %d" member ;
  return (`Volunteer member)

let candidate_with_message context message =
  lwt member = context.get_message_sender ~message in
  lwt _ =
    match_lwt context.get_message_data ~message ~key:key_run_id with
      None -> return_unit
    | Some run_id ->
      let run_id = Int64.of_string run_id in
      context.cancel_timers ~query:(tag_timer run_id member)
   in
   return (`Message message)

let key_message = "message"

let ask_supervisor_for_delay context message =
  lwt member = context.get_message_sender ~message in
  lwt _ =
    match_lwt context.get_message_data ~message ~key:key_run_id with
      None -> return_unit
    | Some run_id ->
      let run_id = Int64.of_string run_id in
      lwt _ = context.cancel_timers ~query:(tag_timer run_id member) in
      lwt _ =
        context.forward_to_supervisor
          ~message
          ~subject:"Volunteer would like a delay"
          ~data:[ key_run_id, Int64.to_string run_id ; key_message, string_of_int message ]
          ~content:[
            pcdata "How many days should I wait for?" ; br () ;
          ]
          ()
      in
      return_unit
  in
  return `None

let set_reminder context message =
  lwt content = context.get_message_content ~message in
  let days = ref None in
  Scanf.sscanf content "%d" (fun f -> days := Some f) ;
  match !days with
    None ->
    lwt _ =
      context.reply_to
        ~message
        ~content:[ pcdata "Couldn't extract number of days" ; br () ]
        ()
    in
    return `None
  | Some days ->
    match_lwt context.get_message_data ~message ~key:key_run_id with
      None -> return `None
    | Some run_id ->
      let run_id = Int64.of_string run_id in
      match_lwt context.get_message_data ~message ~key:key_message with
        None -> return `None
      | Some message ->
        let message = int_of_string message in
        lwt member = context.get_message_sender ~message in
        lwt _ =
          context.reply_to
            ~message
            ~data:[ key_run_id, Int64.to_string run_id ]
            ~content:[ pcdata "No problem, I'll get back in touch in "; pcdata (string_of_int days) ; pcdata " days." ; br () ]
            ()
        in
        lwt _ =
          context.set_timer
            ~label:(tag_timer run_id member)
            ~duration:(Calendar.Period.lmake ~day:days ())
            (`Remind (member, message, run_id))
        in
        return `None

let remind_volunteer_after_delay context (member, message, run_id) =
  lwt _ = context.cancel_timers ~query:(tag_timer run_id member) in
  lwt salutations = salutations member in
  lwt _ =
    context.reply_to
      ~message
      ~data:[ key_run_id, Int64.to_string run_id ]
      ~content:[
        salutations ; br () ;
        br () ;
        pcdata "The dinner is getting closer, can you still suggest a place?" ; br () ;
      ]
      ()
  in
  lwt _ =
    context.set_timer
      ~label:(tag_timer run_id member)
      ~duration:(Calendar.Period.lmake ~hour:24 ())
      (`CandidateDidntReplied (member, run_id))
  in
  return `None

COMPONENT

find_volunteer_with_tagline_and_hint ~> `No of email ~> candidate_declined
find_volunteer_with_tagline_and_hint ~> `Yes of email ~> return_volunteer
find_volunteer_with_tagline_and_hint ~> `YesButDelay of email ~> ask_supervisor_for_delay
find_volunteer_with_tagline_and_hint ~> `CandidateDidntReplied of (int * int64) ~> candidate_didnt_replied ~> `RemoveMember of email ~> remove_candidate

                                                                                       candidate_declined ~> `AlertSupervisor ~> alert_supervisor
find_volunteer_with_tagline ~> `FindCandidate ~> look_for_candidate ~> `No of email ~> candidate_declined ~> `FindCandidate ~> look_for_candidate
                                                 look_for_candidate ~> `AlertSupervisor ~> alert_supervisor
                                                 look_for_candidate ~> `CandidateDidntReplied of (int * int64) ~> candidate_didnt_replied ~> `FindCandidate ~> look_for_candidate
                                                                                                                candidate_didnt_replied ~> `AlertSupervisor ~> alert_supervisor
                                                 look_for_candidate ~> `Yes of email ~> return_volunteer
                                                                                        return_volunteer ~> `AlertSupervisor ~> alert_supervisor
                                                 look_for_candidate ~> `Message of email ~> candidate_with_message
                                                 look_for_candidate ~> `YesButDelay of email ~> ask_supervisor_for_delay<forward> ~> `Message of email ~> set_reminder ~> `Remind of int * int * int64 ~> remind_volunteer_after_delay

     remind_volunteer_after_delay ~> `CandidateDidntReplied of int * int64 ~> candidate_didnt_replied
     remind_volunteer_after_delay ~> `No of email ~> candidate_declined
     remind_volunteer_after_delay ~> `Yes of email ~> return_volunteer
