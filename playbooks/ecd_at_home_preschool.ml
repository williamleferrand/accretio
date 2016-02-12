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

let key_activity = sprintf "activity-%Ld"
let tag_preferred_organizer = "preferredorganizer"
let timer_find_host = sprintf "findhost%Ld%d"

(* the entry points for the activities ****************************************)

let schedule_mandarin_circle context () =
  let run_id = new_run_id () in
  lwt members = context.search_members ~query:tag_mandarin_circle_time () in
  lwt _ = context.set ~key:(key_activity run_id) ~value:tag_mandarin_circle_time in
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

let key_host = sprintf "host-%Ld"
let key_acl = "acl"

let ask_supervisor_to_pitch context message =
  match_lwt run_id_from_message context message with
    None -> return `None
  | Some run_id ->
    match_lwt context.get ~key:(key_activity run_id) with
      None -> return `None
    | Some activity ->
      lwt host = context.get_message_sender ~message in
      lwt _ = context.set ~key:(key_host run_id) ~value:(Ys_uid.to_string host) in
      lwt members = context.search_members ~query:activity () in
      let members = List.filter (fun uid -> uid <> host) members in
      match members with
        [] ->
        lwt _ =
          context.reply_to
            ~message
            ~data:(data_run_id run_id)
            ~content:[ pcdata "Thanks! Unfortunately it appears that nobody is available that day :/ - Let's do something else soon!" ]
            ()
        in
        return `None
      | _ as members ->
        lwt _ =
          context.reply_to
            ~message
            ~data:(data_run_id run_id)
            ~content:[ pcdata "Thanks! Let me check who is coming, I'll send you a summary in 24 hours." ]
            ()
        in
        lwt _ =
          context.forward_to_supervisor
            ~message
            ~data:(data_supervisor (data_run_id run_id))
            ~subject:("Please pitch " ^ activity)
            ~content:[ pcdata "Can you pitch the activity? There are " ; pcdata (string_of_int (List.length members)) ; pcdata " potential participants, in addition to the host" ]
            ()
        in
        return `None

let announce_activity context message =
  match_lwt is_from_supervisor context message with
  | false -> return `None
  | true ->
    match_lwt run_id_from_message context message with
      None -> return `None
    | Some run_id ->
      match_lwt context.get ~key:(key_activity run_id) with
        None -> return `None
      | Some activity ->
        let title = StringMap.find activity activity_titles in
        match_lwt context.get ~key:(key_host run_id) with
          None -> return `None
        | Some host ->
          let host = Ys_uid.of_string host in
          lwt pitch = context.get_message_content ~message in
          lwt members = context.search_members ~query:activity () in
          let members = List.filter (fun uid -> uid <> host) members in
          lwt host = $member(host)->name in
          lwt _ =
            Lwt_list.iter_s
              (fun member ->
                 lwt name = $member(member)->name in
                 lwt _ =
                   context.message_member
                     ~member
                     ~data:(data_run_id run_id)
                     ~subject:title
                     ~content:[
                       (if name == "" then pcdata "Greetings," else pcdata ("Hello " ^ name ^ ",")) ; br () ;
                       br () ;
                       pcdata pitch ; br () ;
                       br () ;
                       pcdata "This time, the event will be hosted by " ; pcdata host ; pcdata "." ; br () ;
                       br () ;
                     pcdata "Please let me know if you plan to join!" ; br ()
                     ]
                     ()
                in
                return_unit)
             members
          in
         return `None

let tag_joining = sprintf "joining%Ld"
let tag_not_joining = sprintf "notjoining%Ld"

let key_practical_infos = sprintf "practical-infos-%d"

let mark_joining context message =
  match_lwt run_id_from_message context message with
    None -> return `None
  | Some run_id ->
    match_lwt context.get ~key:(key_host run_id) with
      None -> return `None
    | Some host ->
      let host = Ys_uid.of_string host in
      lwt host = $member(host)->name in
      lwt member = context.get_message_sender ~message in
      lwt _ = context.tag_member ~member ~tags:[ tag_joining run_id ] in
      lwt _ = context.untag_member ~member ~tags:[ tag_not_joining run_id ] in
      lwt _ =
        context.reply_to
          ~message
          ~content:[
            pcdata "Great! " ; pcdata host ; pcdata " will get in touch with you very soon to fine tune the details."
          ]
          ()
      in
      return `None

let mark_not_joining context message =
  match_lwt run_id_from_message context message with
    None -> return `None
  | Some run_id ->
    lwt member = context.get_message_sender ~message in
    lwt _ = context.tag_member ~member ~tags:[ tag_not_joining run_id ] in
    lwt _ = context.untag_member ~member ~tags:[ tag_joining run_id ] in
    lwt _ =
      context.reply_to
        ~message
        ~content:[
            pcdata "No problem, I'll keep you posted about future activities!"
        ]
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

(* organize the weekly schedule ***********************************************)

let key_anchor = "anchor"

let next_week () =
  let rec find_next_monday t =
    match Calendar.day_of_week t with
      Calendar.Mon -> t
    | _ -> find_next_monday (Calendar.next t `Day)
  in
  let monday = find_next_monday (Calendar.now ()) in
  let tuesday = Calendar.next monday `Day in
  let wednesday = Calendar.next tuesday `Day in
  let thursday = Calendar.next wednesday `Day in
  let friday = Calendar.next thursday `Day in
  let anchor = (Calendar.day_of_year monday) + (Calendar.day_of_year monday * 366) in
  anchor, [
    monday ;
    tuesday ;
    wednesday ;
    thursday ;
    friday ;
  ]

let assemble_weekly_schedule context () =
  (* for now, ask the supervisor for a schedule *)
  (* that should be done over the week before? *)

  let anchor, week = next_week () in

  let form =
    List.fold_left
      (fun acc day ->
         let activities =
           List.fold_left
           (fun acc (tag, title) ->
             li [ pcdata (sprintf "%s%d%d" tag anchor (Calendar.day_of_year day)) ] :: acc
           )
           []
           activities
         in
         li [
           pcdata (CalendarLib.Printer.Calendar.sprint " %A, %B %d" day) ;
           ul activities ;
         ] :: acc
      )
      []
      week
  in

  lwt _ =
    context.message_supervisor
      ~subject:"Please come up with a weekly schedule"
      ~data:(data_supervisor [ key_anchor, string_of_int anchor ])
      ~content:[
        pcdata "Greetings," ; br () ;
        br () ;
        pcdata "Could you send the weekly schedule? (not sure how to organize that yet ..)" ;
        ul (List.rev form)
      ]
      ()
  in
  return `None

let parse_schedule context message =
  match_lwt is_from_supervisor context message with
  | false -> return `None
  | true ->
    lwt content = context.get_message_raw_content ~message in
    (* todo rebuild the week from the anchor instead *)
    let anchor, week = next_week () in

    let keys =
      List.fold_left
        (fun acc day ->
           List.fold_left
             (fun acc (tag, title) ->
                sprintf "%s%d%d" tag anchor (Calendar.day_of_year day) :: acc)
               acc
               activities)
        []
        week
    in

    let pairs =
      List.fold_left
        (fun acc key ->
           let regexp = Str.regexp (key ^ " \(.*\)\n") in
           try
             let _ = Str.search_forward regexp content 0 in
             let title = Str.matched_group 1 content in
             (key, title) :: acc
           with Not_found -> acc
        )
        []
        keys
    in
    lwt _ =
      Lwt_list.iter_s
        (fun (key, value) -> context.set ~key ~value)
        pairs
    in
    let _ =
      context.reply_to
        ~message
        ~content:[
          pcdata "Thanks, here is what I found" ;
          ul (List.map (fun (key, title) -> li [ pcdata key ; pcdata " -> " ; pcdata title ]) pairs)
        ]
        ()
    in

    return `None


let suggest_schedule_to_all_members context () =

  lwt members = context.search_members ~query:"active" () in
  let anchor, week = next_week () in

  let activities_by_day activities =
    Lwt_list.fold_left_s
      (fun list day ->
         lwt activities =
           Lwt_list.fold_left_s
             (fun k (tag, title) ->
                lwt keys = context.search ~query:(sprintf "%s%d%d*" tag anchor (Calendar.day_of_year day)) in
                return ((List.map (fun k -> (k, title)) keys) @ k)
             )
             []
             activities
         in

         lwt activities =
           Lwt_list.fold_left_s
             (fun titles (key, title) ->
                match_lwt context.get ~key with
                  None -> return titles
                | Some pitch -> return (li [ pcdata title ; pcdata ": " ; pcdata pitch ] :: titles))
             []
             activities
         in
         match activities with
           [] -> return list
         | _ as activities ->
           return (
             li [
               pcdata (CalendarLib.Printer.Calendar.sprint "%A, %B %d" day) ;
               ul activities ] :: list
           ))
      []
      (List.rev week)
  in

  lwt _ =
    Lwt_list.iter_s
      (fun member ->

         (* for each member, we grab the activities they registered from *)
         lwt (preferred, others) =
           Lwt_list.partition_s (fun (tag, title) -> context.check_tag_member ~member ~tag) activities
         in
         lwt salutations = salutations member in
         lwt preferred = activities_by_day preferred in
         lwt others = activities_by_day others in
         match preferred, others with
         | [], [] -> return_unit
         | [], others ->
           lwt _ =
             context.message_member
               ~member
               ~data:[ key_anchor, string_of_int anchor ]
               ~subject:"Weekly suggestions of children activities"
               ~content:[
                 salutations ; br () ;
                 br () ;
                 pcdata "There is no scheduled event for your preferred activities this week; however here are some options that could be relevant:" ; br () ;
                 ul others ;
                 pcdata "Please let me know before the end of the weekend if you would like to attend any of these activities. I'll help working out the details!" ; br () ;
               ]
               ()
           in
           return_unit
         | preferred, [] ->
           lwt _ =
             context.message_member
               ~member
               ~data:[ key_anchor, string_of_int anchor ]
               ~subject:"Weekly suggestions of children activities"
               ~content:[
                 salutations ; br () ;
                 br () ;
                 pcdata "Here is a list of suggestions for the coming week. Would you like to attend some of these activities? Just reply inline and I'll figure the details." ; br () ;
                 ul preferred ;
                 pcdata "Please let me know before the end of the weekend," ; br () ;
               ]
               ()
           in
           return_unit
         | preferred, others ->
           lwt _ =
             context.message_member
               ~member
               ~data:[ key_anchor, string_of_int anchor ]
               ~subject:"Weekly suggestions of children activities"
               ~content:[
                 salutations ; br () ;
                 br () ;
                 pcdata "Here is a list of suggestions for the coming week based on your preferences:" ; br () ;
                 ul preferred ;
                 pcdata "Also, here are some additional activities that you might be interested in as well:" ; br () ;
                 ul others ;
                 pcdata "Would you like to attend some of these activities? Please let me know before the end of the weekend and I'll figure out the details!" ; br () ;
               ]
               ()
           in
           return_unit)
      members
  in

  lwt _ =
    context.message_supervisor
      ~subject:"Invitations are sent"
      ~content:[
        pcdata "The weekly schedule is out, let's see what comes back!"
      ]
      ()
  in
  return `None

(* the flow *******************************************************************)

PLAYBOOK

   #import core_join_request
   #import core_invite


 *suggest_activities<forward> ~> `Message of email ~> ask_supervisor_to_filter_activities<forward> ~> `Message of email ~> tag_member_with_activities

 *schedule_mandarin_circle ~> `FindHost of (int64 * string * int) ~> find_host ~> `Message of email ~> disable_timer
                                                                     find_host ~> `FindAnotherHost of (int64 * string * int * int * int) ~> find_another_host
                                                                     find_host ~> `HostHasAccepted of email ~> ask_supervisor_to_pitch<forward> ~> `Message of email ~> announce_activity
      announce_activity ~> `Joining of email ~> mark_joining
      announce_activity ~> `NotJoining of email ~> mark_not_joining

  *assemble_weekly_schedule<forward> ~> `Message of email ~> parse_schedule
  *suggest_schedule_to_all_members
