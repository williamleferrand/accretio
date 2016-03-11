(*
 * monthly bbq
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

let author = "william@accret.io"
let name = "Monthly BBQ"
let description = "This playbook organizes a monthly BBQ."

let version = 0
let tags = ""

let iso_date = "%FT%H:%M:%S"

(* some tags ******************************************************************)

let key_current_run = "current-run"
let key_crontab = "crontab"
let key_date = "date"
let key_event_message = sprintf "event-message-%f"
let key_count = sprintf "count-participants-%f-%d"
let tag_already_invited = sprintf "alreadynotified%f"
let tag_attending = sprintf "attending%f"
let tag_not_attending = sprintf "notattending%f"

let timer_reminder = sprintf "reminder%d%f"

(* stages *********************************************************************)

let ask_for_new_crontab context () =
  lwt _ =
    context.message_supervisor
      ~subject:"Update crontab"
      ~content:[
        pcdata "Greetings," ; br () ;
        br () ;
        pcdata "What crontab do you want to use moving forward?"; br ();
      ]
      ()
  in
  return `None

let update_crontab context message =
  lwt crontab = context.get_message_content ~message in
  try
    let crontab = Cron.crontab_of_string crontab in
    let crontab = Cron.crontab_to_string crontab in
    context.log_info "found crontab %s" crontab ;
    lwt _ = context.set ~key:key_crontab ~value:crontab in
    return (`CrontabUpdated (message, crontab))
  with _ ->
    lwt _ =
      context.reply_to
        ~message
        ~content:[
          pcdata "Oops, I can't parse this crontab. Send me another one?"
        ]
        ()
    in
    return `None

let crontab_updated context (message, crontab) =
  lwt _ =
    context.reply_to
      ~message
      ~content:[
        pcdata "Great, stored! Want to schedule the next one now? (please reply yes/no)"
      ]
      ()
  in
  return `None

let do_nothing _ _ =
  return `None

let tick_schedule_bbq _ _ =
  return `Tick

let tick_ask_for_date _ _ =
  return `Tick

let schedule_bbq context () =
  (* if we have a crontab set, we can make a time proposition right away *)
  match_lwt context.get ~key:key_crontab with
    None -> return `AskForDate
  | Some crontab ->
    try
      let crontab = Cron.crontab_of_string crontab in
      match Cron.find_next_execution_date crontab with
        None -> return `AskForDate
      | Some date ->
        let date = Calendar.to_unixfloat date in
        return (`AskForCustomMessageOrAnotherDate date)
    with _ -> return `AskForDate

let ask_for_date context () =
  lwt _ =
    context.message_supervisor
      ~subject:"Next date?"
      ~content:[
        pcdata "Greetings," ; br () ;
        br () ;
        pcdata "When do you want to hold the next event?" ; pcdata " Please send me a date using the ISO 8601 format, such as in 2013-05-15T08:30:00" ; br () ;
      ]
      ()
  in
  return `None

let parse_date context message =
  lwt content = context.get_message_content ~message in
  try
    let date = CalendarLib.Printer.Calendar.from_fstring iso_date content in
    let date = Calendar.to_unixfloat date in
    return (`AskForCustomMessageOrAnotherDate date)
  with _ ->
    lwt _ =
      context.reply_to
        ~message
        ~content:[
          pcdata "Oops, I can't parse this date. Would you mind using the ISO 8601 format, such as in 2013-05-15T08:30:00"
        ]
        ()
    in
    return `None

let ask_for_custom_message_or_another_date context date =
  let date = Calendar.from_unixfloat date in
  lwt _ =
    context.message_supervisor
      ~subject:(CalendarLib.Printer.Calendar.sprint "Custom message for the even on %B %d" date)
      ~data:[ key_date, string_of_float (Calendar.to_unixfloat date) ]
      ~content:[
        pcdata "Greetings," ; br () ;
        br () ;
        pcdata "The next event will take place on " ;
        pcdata (CalendarLib.Printer.Calendar.sprint "%B %d (it's a %A), at %I:%M %p." date) ; br () ;
        br () ;
        pcdata "Please reply to this email with a custom message describing the event and I will send the invitations out." ; br () ;
        br () ;
        pcdata "Alternatively if you want to alter the date, reply 'alterdate'"; br () ;
        br () ;
      ]
      ()
  in
  return `None

let forward_or_ask_date message =
  lwt content = $message(message)->content in
  try
    ignore (Str.search_forward (Str.regexp_string_case_fold "alterdate") content 0) ;
    return (Some (`AskForDate message))
  with Not_found -> return (Some (`Message message))

let send_invitations context message =
  lwt content = context.get_message_content ~message in
  match_lwt context.get_message_data ~message ~key:key_date with
    None ->
    context.log_error "can't find a date in message %d" message ;
    lwt _ =
      context.forward_to_supervisor
        ~message
        ~subject:"You're needed"
        ~content:[
          pcdata "Can't find a date attached to this message, so invitations won't be sent"
        ]
        ()
    in
    return `None
  | Some date ->
    lwt _ = context.set ~key:key_current_run ~value:date in
    let date = Calendar.from_unixfloat (float_of_string date) in
    lwt _ = context.set ~key:(key_event_message (Calendar.to_unixfloat date)) ~value:content in
    lwt participants = context.search_members ~query:(sprintf "active -%s" (tag_already_invited (Calendar.to_unixfloat date))) () in
    lwt count =
      Lwt_list.fold_left_s
        (fun count member ->
           match_lwt context.check_tag_member ~member ~tag:(tag_already_invited (Calendar.to_unixfloat date)) with
             true -> return count
           | false ->
             lwt name = $member(member)->name in
             lwt _ =
               context.message_member
                 ~data:[ key_date, string_of_float (Calendar.to_unixfloat date)]
                 ~member
                 ~subject:(Printf.sprintf "%s / %s" context.society_name (CalendarLib.Printer.Calendar.sprint "%B %d" date))
                 ~content:[
                   (match name with "" -> pcdata "Dear member," | _ as name -> pcdata (sprintf "Dear %s," name)) ; br () ;
                   br () ;
                   pcdata content ;
                   br () ;
                   pcdata "The event will take place on "; pcdata (CalendarLib.Printer.Calendar.sprint "%B %d (it's a %A), at %I:%M %p." date) ; br () ;
                   br () ;
                   pcdata "Will you join us? Please reply a quick yes or no so that we can efficiently plan for this gathering!" ;
                   br () ;
                   pcdata "Thanks"
                 ]
                 ()
             in
             lwt _ = context.tag_member ~member ~tags:[ tag_already_invited (Calendar.to_unixfloat date) ] in
             lwt _ = context.set_timer
                       ~label:(timer_reminder member (Calendar.to_unixfloat date))
                       ~duration:(Calendar.Period.lmake ~day:3 ())
                       (`RemindMember (member, Calendar.to_unixfloat date))
             in
             return (count + 1))
       0
       participants
     in
     lwt _ =
       context.reply_to
         ~message
         ~content:[
           pcdata "Ok, I just messaged " ; pcdata (string_of_int count) ; pcdata " out of " ; pcdata (string_of_int (List.length participants)) ;
         ]
         ()
     in
     return `None

let mark_attending context message =
  lwt message = context.get_original_message ~message in
  lwt content = context.get_message_content ~message in
  lwt member = context.get_message_sender ~message in
  match_lwt context.get_message_data ~message ~key:key_date with
    None ->
    lwt _ =
      context.forward_to_supervisor
        ~message
        ~subject:"Can't figure out which event this message is about"
        ~content:[
          pcdata "I can't figure out which date is attached to this event. You'd need to go to your dashboard to fix that, sorry.. (or just ignore the message)"
        ]
        ()
    in
    return `None
  | Some date ->

    (* let's try to grab the number of participants *)
    let regexp = Str.regexp "[0-9]+" in
    let number_of_participants =
      try
        let _ = Str.search_forward regexp content in
        int_of_string (Str.matched_string content)
      with Not_found -> 1
    in

    let date = Calendar.from_unixfloat (float_of_string date) in
    lwt _ = context.tag_member ~member ~tags:[ tag_attending (Calendar.to_unixfloat date) ] in
    lwt _ = context.untag_member ~member ~tags:[tag_not_attending (Calendar.to_unixfloat date) ] in
    lwt _ = context.cancel_timers ~query:(timer_reminder member (Calendar.to_unixfloat date)) in
    lwt _ = context.set ~key:(key_count (Calendar.to_unixfloat date) member) ~value:(string_of_int number_of_participants) in
    lwt _ =
      context.forward_to_supervisor
        ~message
        ~subject:(sprintf "You got a positive response for the event on %s" (CalendarLib.Printer.Calendar.sprint "%B %d" date))
        ~content:[
          pcdata "I marked this response as positive, with " ; pcdata (string_of_int number_of_participants) ; pcdata " participants. Reply yes/no and a number to change that if needed."
        ]
        ()
    in
    return `None

let mark_not_attending context message =
  lwt message = context.get_original_message ~message in
  lwt member = context.get_message_sender ~message in
  match_lwt context.get_message_data ~message ~key:key_date with
    None ->
    lwt _ =
      context.forward_to_supervisor
        ~message
        ~subject:"Can't figure out which event this message is about"
        ~content:[
          pcdata "I can't figure out which date is attached to this event. You'd need to go to your dashboard to fix that, sorry.. (or just ignore the message)"
        ]
        ()
    in
    return `None
  | Some date ->
    let date = Calendar.from_unixfloat (float_of_string date) in
    lwt _ = context.tag_member ~member ~tags:[ tag_not_attending (Calendar.to_unixfloat date) ] in
    lwt _ = context.untag_member ~member ~tags:[ tag_attending (Calendar.to_unixfloat date) ] in
    lwt _ = context.cancel_timers ~query:(timer_reminder member (Calendar.to_unixfloat date)) in
    lwt _ =
      context.forward_to_supervisor
        ~message
        ~subject:(sprintf "You got a negative response for the event on %s" (CalendarLib.Printer.Calendar.sprint "%B %d" date))
        ~content:[
          pcdata "I marked this response as negative. Reply yes/no to change that if needed"
        ]
        ()
    in
    return `None

let create_dashboard_current_run context () =
  match_lwt context.get ~key:key_current_run with
    None -> return `None
  | Some date -> return (`CreateDashboard (float_of_string date))

let create_dashboard context date =
  let date = Calendar.from_unixfloat date in
  lwt participants = context.search_members ~query:(tag_attending (Calendar.to_unixfloat date)) () in
  lwt count =
    Lwt_list.fold_left_s
      (fun acc member ->
         match_lwt context.get ~key:(key_count (Calendar.to_unixfloat date) member) with
           None -> return (acc + 1)
         | Some count -> return (acc + int_of_string count))
      0
      participants
  in

  lwt _ =
    context.message_supervisor
      ~subject:(Printf.sprintf "Headcount for the event on %s" (CalendarLib.Printer.Calendar.sprint "%B %d" date))
      ~content:[
        pcdata "Greetings," ; br () ;
        br () ;
        pcdata "So far, there are " ; pcdata (string_of_int count) ; pcdata " confirmed participants to the event (brought by " ;
        pcdata (string_of_int (List.length participants)) ; pcdata " participants)" ; br () ;
      ]
      ()
  in
  return `None

let remind_member context (member, date) =
  context.log_info "reminding member %d about event %f" member date ;
  let date = Calendar.from_unixfloat date in
  match_lwt context.get ~key:(key_event_message (Calendar.to_unixfloat date)) with
    None -> return `None
  | Some message ->
    lwt name = $member(member)->name in
    lwt _ =
      context.message_member
        ~data:[ key_date, string_of_float (Calendar.to_unixfloat date)]
        ~member
        ~subject:(Printf.sprintf "(Reminder, please RSVP) %s / %s" context.society_name (CalendarLib.Printer.Calendar.sprint "%B %d" date))
        ~content:[
          (match name with "" -> pcdata "Dear member," | _ as name -> pcdata (sprintf "Dear %s," name)) ; br () ;
          br () ;
          pcdata "Quick question, will you join us on " ; pcdata (CalendarLib.Printer.Calendar.sprint "%B %d (it's a %A), at %I:%M %p?" date) ; br () ;
          br () ;
          pcdata message ;
          br () ;
          pcdata "Give us a heads up by simply replying yes or no to this email!" ; br () ;
          br () ;
          pcdata "Thanks,"
        ]
        ()
    in
    return `None

(* the playbook ***************************************************************)

PARAMETERS
   - "Crontab", "crontab"

PLAYBOOK

  *ask_for_new_crontab<forward> ~> `Message of email ~> update_crontab<forward> ~> `Message of email ~> update_crontab
                                                        update_crontab ~> `CrontabUpdated of int * string ~> crontab_updated

            crontab_updated<simple_yes_no> ~> `No of email ~> do_nothing
            crontab_updated ~> `Yes of email ~> tick_schedule_bbq ~> `Tick ~> schedule_bbq

                                                                                parse_date<forward> ~> `Message of email ~> parse_date
  *schedule_bbq ~> `AskForDate ~> ask_for_date<forward> ~> `Message of email ~> parse_date ~> `AskForCustomMessageOrAnotherDate of float ~> ask_for_custom_message_or_another_date
   schedule_bbq ~> `AskForCustomMessageOrAnotherDate of float ~> ask_for_custom_message_or_another_date

   ask_for_custom_message_or_another_date<forward_or_ask_date> ~> `AskForDate of email ~> tick_ask_for_date ~> `Tick ~> ask_for_date              mark_attending ~> `No of email ~> mark_not_attending
   ask_for_custom_message_or_another_date ~> `Message of email ~> send_invitations<simple_yes_no> ~> `Yes of email ~> mark_attending<simple_yes_no> ~> `Yes of email ~> mark_attending
                                                                  send_invitations ~> `No of email ~> mark_not_attending<simple_yes_no> ~> `No of email ~> mark_not_attending
                                                                                                      mark_not_attending ~> `Yes of email ~> mark_attending
          send_invitations ~> `RemindMember of int * float ~> remind_member<simple_yes_no> ~> `Yes of email ~> mark_attending
                                                              remind_member ~> `No of email ~> mark_not_attending

  *create_dashboard_current_run ~> `CreateDashboard of float ~> create_dashboard
