(*
 * fieldtrips
 *
 * this playbook organizes field trips for preschoolers
 *
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

open Ys_uid

let author = "william@accret.io"
let name = "Field trips"
let description = "This playbook schedules field trips"
let version = 0
let tags = ""

(* TAGS ***********************************************************************)

let tag_curator = "curator"
let tag_family = "family"

(* UTILITIES ******************************************************************)


(* CURRICULUM *****************************************************************)

let handle_suggestion_inbox context () =
  return `None

let handle_suggestion context message =
  lwt suggestion_member = context.get_message_sender ~message in
  lwt suggestion_raw = context.get_message_content ~message in
  match_lwt Ys_shortlink.create () with
    None ->
    context.log_error "couldn't create shortlink for suggestion %s from member %d"
      suggestion_raw
      suggestion_member ;
    return `None
  | Some shortlink ->
    lwt thread =
      match_lwt
        Object_thread.Store.create
          ~owner:context.society_supervisor
          ()
      with
        `Object_created thread -> return thread.Object_thread.uid
    in
    lwt uid =
      match_lwt
        Object_activity.Store.create
          ~society:context.society
          ~description:suggestion_raw
          ~thread
          ~shortlink
          ~state:Object_activity.Drafting
          ~suggestion_member
          ~suggestion_raw
          () with
      | `Object_already_exists (_, uid) -> return uid
      | `Object_created activity -> return activity.Object_activity.uid
    in
    lwt _ = $society(context.society)<-activities += (`Activity, uid) in
    lwt _ =
      context.reply_to
        ~message
        ~content:[ pcdata "Thanks for the suggestion!" ]
        ()
    in
    return (`NotifySupervisorNewSuggestion uid)

let notify_supervisor_new_suggestion context activity =
  lwt shortlink = $activity(activity)->shortlink in
  let link = context.create_link (Service.Activity (shortlink, activity)) in
  lwt _ =
    context.message_supervisor
      ~subject:"There is a new suggestion"
      ~content:[
        pcdata "Hi," ; br () ;
        br () ;
        pcdata "Here is a new suggestion. Please visit the activity page to edit the draft: " ; br () ;
        br () ;
        Raw.a ~a:[ a_href (uri_of_string (fun () -> link)) ] [ pcdata link ]; br () ;
        br () ;
        pcdata "Thanks"

      ]
      ()
  in
  return `None

let ask_curators context () =
  lwt curators = context.search_members ~query:tag_curator () in
  return `None

(* SCHEDULING A FIELD TRIP ****************************************************)

let tag_already_proposed = sprintf "alreadyproposed%d"

let propose_activity context activity =
  lwt title, description = $activity(activity)->(title, description) in
  lwt families = context.search_members ~query:tag_family () in
  context.log_info "proposing activity %d (%s) to %d families" activity title (List.length families) ;
  lwt _ =
    Lwt_list.iter_s
      (fun member ->
         match_lwt context.check_tag_member ~member ~tag:(tag_already_proposed activity) with
           true ->
           context.log_info "member %d was already notified about activity %d, skipping" member activity ;
           return_unit
         | false ->
           match_lwt
             lwt salutations = salutations member in
             context.message_member
               ~member
               ~subject:("New Field Trip: " ^ title)
               ~remind_after:(Calendar.Period.lmake ~hour:36 ())
               ~content:[
                 salutations ; br () ;
                 br () ;
                 pcdata "I hope all is well!" ; br () ;
                 br () ;
                 pcdata "Here is a new field trip suggestion for this week: " ; span ~a:[ a_style "white-space:pre-line;" ] [ pcdata description ] ; br () ;
                 br () ;
                 pcdata "As usual, lunch will be provided, pickup will be at your nearest playground and the van will have car seats." ; br () ;
                 br () ;
                 pcdata "Let me know if you are interested in joining and if you would rather ride along with your child or connect with a caregiver for this trip." ; br () ;
                 br () ;
                 pcdata "Also, the more the merrier - feel free to forward this email around!" ; br () ;
                 br () ;
                 pcdata "Thanks" ; br () ;
               ]
               ()
           with
             None ->
             context.log_error "couldn't notify member %d about activity %d, couldn't send message" member activity ;
             return_unit
           | Some _ ->
             context.log_info "member %d has been notified about activity %d" member activity ;
             lwt _ = context.tag_member ~member ~tags:[ tag_already_proposed activity ] in
             return_unit)
      families
  in
  return `None



(* the playbook ***************************************************************)

PLAYBOOK

#import core_remind

*handle_suggestion_inbox<forward> ~> `Message of email ~> handle_suggestion ~> `NotifySupervisorNewSuggestion of int ~> notify_supervisor_new_suggestion

propose_activity[int]

PROPERTIES
  - "Your duties", "None"
