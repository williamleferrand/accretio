(*
 * ecd_homeschool
 *
 * this playbook implements an experimental curriculum for preschoolers
 *
 * william@accret.io
 *
 *)

open Lwt

open Printf
open CalendarLib
open Calendar.Precise

open Api

open Eliom_content.Html5
open Eliom_content.Html5.D

open Message_parsers
open Toolbox

let author = "william@accret.io"
let name = "At home preschool"
let description = "This playbook implements an at home preschool"

let version = 0

(* principle of operation


   let's start with something simple. the supervisor can make suggestions & tag
   people based on the responses.


   the supervisor needs to come up with an entry stage for each suggestion
   and let the platform schedule the activities

   hopefully there is a lot of shared logic (carpooling) that we can reuse between
   the different activities and later move to their own playbooks

*)


(* the curriculum *************************************************************)

let tag_mandarin_circle_time = "mandarincircletime"
let tag_english_circle_time = "englishcircletime"
let tag_museum_visits = "museumvisits"
let tag_outdoor_activities = "outdooractivities"

let activities =
  [
    tag_mandarin_circle_time, "Mandarin circle time" ;
    tag_english_circle_time, "English circle time" ;
    tag_museum_visits, "Museum visits" ;
    tag_outdoor_activities, "Outdoor activities" ;
  ]

module StringMap = Map.Make(String)

let activity_titles =
  List.fold_left
    (fun acc (tag, title) ->
       StringMap.add tag title acc)
    StringMap.empty
    activities

(* some tags ******************************************************************)

let tag_preferred_organizer = "preferredorganizer"
let timer_find_host = sprintf "findhost%Ld%d"

(* the entry points for the activities ****************************************)

let schedule_mandarin_circle context () =
  let run_id = new_run_id () in
  lwt members = context.search_members ~query:tag_mandarin_circle_time () in
  return (`FindHost (run_id, tag_mandarin_circle_time, List.length members))


(* some tooling that we could move to another playbook *)

let find_host context (run_id, activity, number_of_members) =
  context.log_info "looking for host for activity %s, estimated number of members is %d" activity number_of_members ;
  match_lwt context.search_members ~query:activity () with
  | [] -> return `None
  | _ as members ->

    lwt member =
      try_lwt
        Lwt_list.find_s (fun member -> context.check_tag_member ~member ~tag:tag_preferred_organizer) members
      with Not_found ->
        return (List.nth members (Random.int (List.length members)))
    in

    let title = StringMap.find activity activity_titles in

    match_lwt context.message_member
                ~member
                ~data:(data_run_id run_id)
                ~subject:title
                ~content:[
                  pcdata "Greetings," ; br () ;
                  br () ;
                  pcdata "Do you have time to host a " ; pcdata title ; pcdata " later this week?" ; br () ;
                  br () ;
                  pcdata "There are about "; pcdata (string_of_int number_of_members) ; pcdata " people interested." ; br () ;
                  br () ;
                  pcdata "Thanks in advance!" ; br ()
                ]
                () with
    | None ->
      lwt _ =
        context.message_supervisor
          ~subject:("Couldn't find host for activity " ^ activity)
          ~content:[
            pcdata "Something went wrong while looking for a host. Could you step in?"
          ]
          ()
      in
      return `None
    | Some message ->
      lwt _ =
        context.set_timer
          ~label:(timer_find_host run_id member)
          ~duration:(Calendar.Period.lmake ~day:1 ())
          (`FindAnotherHost (run_id, activity, number_of_members, member, message))
      in
      return `None


let disable_timer context message =
  lwt member = context.get_message_sender ~message in
  match_lwt run_id_from_message context message with
    None -> return `None
  | Some run_id ->
    lwt _ = context.cancel_timers ~query:(timer_find_host run_id member) in
    return `None

let find_another_host context (run_id, activity, number_of_members, member, message) =
  lwt _ = context.cancel_timers ~query:(timer_find_host run_id member) in
  lwt _ =
    context.reply_to
      ~message
      ~content:[ pcdata "No problem, I'll ask someone else from the group." ]
      ()
  in
  return `None

(* the stages *****************************************************************)

let suggest_activities context () =
  lwt members = context.search_members "active" () in
  let suggestions = ul (List.map (fun (_, suggestion) -> li [ pcdata suggestion ]) activities) in
  lwt _ =
    Lwt_list.iter_s
      (fun member ->
         lwt _ =
           context.message_member
             ~member
             ~subject:"Children activities"
             ~content:[
               pcdata "Greetings," ; br () ;
               br () ;
               pcdata "I hope your week is off to a great start!" ; br () ;
               br () ;
               pcdata "Here are a few suggestions of activities that could be relevant for our children, could you tell me if you find anything appealing in the list? (No commitment at this point, I'm just testing the waters)"; br () ;
               suggestions ;
               pcdata "Also, do you want to suggest an activity?" ; br () ;
               br () ;
               pcdata "Looking forward to hearing from you!" ; br ()
             ]
             ()
         in
         return_unit)
      members
  in
  return `None

let ask_supervisor_to_filter_activities context message =
  let suggestions = ul (List.map (fun (tag, suggestion) -> li [ pcdata tag  ; pcdata " (" ; pcdata description ; pcdata ") " ]) activities) in
  lwt _ =
    context.forward_to_supervisor
      ~message
      ~subject:"Please filter the activities"
      ~content:[
        pcdata "Greetings," ; br () ;
        br () ;
        pcdata "Below is the reply from one of the member - please filter out the activities they are interested in so that I can tag him"; br () ;
        suggestions
      ]
      ()
  in
  return `None

let tag_member_with_activities context message =
  lwt content = context.get_message_content ~message in
  let tags =
    List.fold_left
      (fun tags (tag, _) ->
         let regexp = Str.regexp_string tag in
         try
           let _ = Str.search_forward regexp content 0 in
           tag :: tags
         with Not_found -> tags)
      []
      activities in
  lwt message = context.get_original_message ~message in
  lwt member = context.get_message_sender ~message in
  lwt _ = context.tag_member ~member ~tags in
  match tags with
    [] ->
    return `None
  | _ ->
    lwt _ =
      context.reply_to
        ~message
        ~content:[
          pcdata "Thanks, I'll keep you posted!"
        ]
        ()
    in
    return `None

(* the flow *******************************************************************)

PLAYBOOK

   #import core_join_request
   #import core_invite


 *suggest_activities<forward> ~> `Message of email ~> ask_supervisor_to_filter_activities<forward> ~> `Message of email ~> tag_member_with_activities

  schedule_mandarin_circle ~> `FindHost of (int64 * string * int) ~> find_host ~> `Message of email ~> disable_timer
                                                                     find_host ~> `FindAnotherHost of (int64 * string * int * int * int) ~> find_another_host
