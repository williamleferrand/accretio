(*
 * children_schoolbus
 *
 * this playbook organizes a preschool on wheels by proposing activities, booking a bus
 * and organizing pickups/dropoffs
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

open Children_schoolbus_types
open Children_schoolbus_tools

let author = "william@accret.io"
let name = "Children schoolbus"
let description = "this playbook organizes a preschool on wheels by proposing activities, booking a bus and organizing pickups/dropoffs"
let version = 0
let tags = ""

(* tags ***********************************************************************)

let tag_mute = "mute"
let tag_to_invite = "toinvite"
let tag_interested_in_general = "interested"

(* keys ***********************************************************************)

let key_last_message = sprintf "last-message-%d"

(* some helpers ***************************************************************)

let get_or_create_profile context member =
  let create_and_return_new_profile () =
    lwt name, email = $member(member)->(name, preferred_email) in
    let profile = default_profile member name email in
    let stringified = Yojson_profile.to_string profile in
    lwt _ = context.set ~key:(key_profile member) ~value:stringified in
    return profile
  in
  match_lwt context.get ~key:(key_profile member) with
    None -> create_and_return_new_profile ()
  | Some profile ->
    try
      return (Yojson_profile.from_string profile)
    with _ -> create_and_return_new_profile ()


(* the stages *****************************************************************)

(******************************************************************************)
(* ONBOARDING                                                                 *)
(******************************************************************************)

let check_if_member_already_exists context message =
  lwt sender = context.get_message_sender ~message in
  lwt members = $society(context.society)->members in
  match Ys_uid.Edges.mem sender members with
    true -> return `Yes
  | false ->
    (* let's add the member now *)
    lwt _ = context.add_member ~member:sender in
    return (`No message)

let noop context () =
  return `None

let ask_supervisor_to_extract_information context message =
  lwt sender = context.get_message_sender ~message in

  lwt profile = get_or_create_profile context sender in
  let stringified = Yojson_profile.to_string profile in
  lwt email = $member(sender)->preferred_email in
  lwt _ =
    context.forward_to_supervisor
      ~message
      ~subject:"Your input is needed"
      ~content:[
        pcdata "Greetings," ; br () ;
        br () ;
        pcdata "Here is a message from " ; pcdata email ; pcdata " with some profile information" ; br () ;
        br () ;
        pcdata "Could you fill up the Json below? (and return only the json)" ; br () ;
        br () ;
        Unsafe.data stringified ; br () ;
      ]
      ()
  in
  return `None

let store_profile_and_ask_for_missing_elements context message =
  lwt content = context.get_message_content ~message in
  try
    let profile = Yojson_profile.from_string content in
    let stringified = Yojson_profile.to_string profile in
    lwt _ = context.set ~key:(key_profile profile.uid) ~value:stringified in
    Lwt_log.ign_info_f "storing profile %s (neighborhood was %s, original was %s)" stringified profile.neighborhood content ;

    let fields = [ Children ; Neighborhood ; Schedule ] in

    let missing_fields =
      List.filter
        (function
          | Name -> profile.name = ""
          | Children -> (try profile.children = [] || ((List.hd profile.children).age_string = "") with _ -> true)
          | Neighborhood -> profile.neighborhood = ""
          | Schedule -> profile.schedule = "")
        fields
    in

    lwt _ =
      context.reply_to
        ~message
        ~content:[ pcdata "Thanks, I was able to parse the message correctly and ask the member for missing information" ]
        ()
    in

    match missing_fields with
      [] ->
      lwt _ =
        context.message_supervisor
          ~subject:"new full profile, need assignment"
          ~content:[
            pcdata "there is a new full profile waiting for an assignment" ; br () ;
          ]
          ()
      in
      return (`ThankMember profile.uid)
    | _ as fields -> return (`AskMemberForMissingFields (profile.uid, fields))

  with exn ->
    Lwt_log.ign_error_f ~exn "couldn't parse json %s--" content ;
    lwt _ =
      context.reply_to
        ~message
        ~content:[ pcdata "Please try again" ]
        ()
    in
    return `None

let thank_member context member =
  lwt salutations = salutations member in
  (* we add the member at the end of the onboarding process, although we could have done it earlier? *)
  lwt _ = context.add_member ~member in
  match_lwt context.get ~key:(key_last_message member) with
  | None ->
    (match_lwt context.check_tag_member ~member ~tag:tag_mute with
       true -> return `None
     | false ->
       let _ =
         context.message_member
           ~member
           ~subject:"Preschool on wheels"
           ~content:[
             salutations ; br () ;
             br () ;
             pcdata "Thanks for your note. I'll follow up shortly with some suggestions of upcoming trips."
           ]
           () in
       return `None)
  | Some message ->
    let message = int_of_string message in
    let _ =
      context.reply_to
        ~message
        ~content:[
          pcdata "Thanks, that's great. I'll follow up shortly with some suggestions of upcoming trips."
        ]
        ()
    in
    return `None

let ask_member_for_missing_fields context (member, fields) =
  lwt salutations = salutations member in
  lwt signature = signature context in

  let weight = function
    | Children -> 1
    | Schedule -> 2
    | Neighborhood -> 3
    | Name -> 3
  in

  let fields = List.sort (fun a b -> compare (weight a) (weight b)) fields in

  let questions =
    List.map
      (function
        | Name -> "what's your name :-)?"
        | Children -> "do you plan to bring one child or several children to the activities and how old are they?"
        | Neighborhood -> "where would you like to be picked up (street/cross street is enough)?"
        | Schedule -> "are you looking for weekdays or weekend activities?")
      fields
  in

  match questions with
    [] -> return `None
  | q1 :: q2 ->
    let questions =
      match q2 with
        [] -> [ String.capitalize q1 ]
      | q2 :: others ->
        String.capitalize q1 :: ("Also, " ^ q2) :: (List.map String.capitalize others)
    in
    let questions =
      span (List.map (fun question -> span [ pcdata question ; pcdata " " ]) questions)
    in

    match_lwt context.check_tag_member ~member ~tag:tag_mute with
      true -> return `None
    | false ->
      match_lwt context.get ~key:(key_last_message member) with
      | None ->
        let _ =
          context.message_member
            ~member
            ~remind_after:(Calendar.Period.lmake ~hour:36 ())
            ~subject:"Preschool on wheels"
            ~content:[
              salutations ; br () ;
              br () ;
              pcdata "Thanks for your note! Before making relevant field trip suggestions," ; br () ;
              br () ;
              (match List.length fields with 1 -> pcdata "I've one more question for you. " | _ -> pcdata "I've a few more questions for you :) ") ; questions ; br () ;
              br () ;
              pcdata "Thanks," ; br () ;
              pcdata signature ;
            ]
            () in
        return `None
      | Some message ->
        let message = int_of_string message in
        match_lwt context.check_tag_member ~member ~tag:tag_interested_in_general with
          true ->
          lwt _ = context.untag_member ~member ~tags:[ tag_interested_in_general ] in
          let _ =
            context.reply_to
              ~message
              ~remind_after:(Calendar.Period.lmake ~hour:36 ())
              ~content:[
                pcdata "Great! I will send you more information about upcoming trips, but " ;
                (match List.length fields with 1 -> pcdata "I've one more question first :) " | _ -> pcdata "I've a few more questions first :) ") ; br () ;
                br () ;
                questions ; br () ;
                br () ;
                pcdata "Thanks," ; br () ;
                pcdata signature ;
              ]
              () in
          return `None
        | false ->
          let _ =
            context.reply_to
              ~message
              ~remind_after:(Calendar.Period.lmake ~hour:36 ())
              ~content:[
                (match List.length fields with 1 -> pcdata "Thanks, I've one more question :) " | _ -> pcdata "Thanks. I've a few more questions :) ") ; br () ;
                br () ;
                questions ; br () ;
                br () ;
                pcdata "Thanks," ; br () ;
                pcdata signature ;
              ]
              () in
          return `None

let store_reply context message =
  lwt sender = context.get_message_sender ~message in
  lwt _ = context.set ~key:(key_last_message sender) ~value:(string_of_int message) in
  return (`Message message)

(******************************************************************************)
(* COMPUTING THE GROUPS                                                       *)
(******************************************************************************)

(*
 we group people
    - so that children have more or less the same age
    - so that the groups are more or less the same day over day
    - so that the bus pickup can be done in a small amount of time
*)

let group_all_members context () =
  (* this will only work if we have less than 23 members as this is the limit
     of the TSP in google maps (and the limit of the bus size?) *)
  Lwt_log.ign_info_f "grouping all members" ;
  lwt members = $society(context.society)->members in
  lwt profiles =
    Lwt_list.fold_left_s
      (fun acc (_, uid) ->
         match_lwt get_profile context uid with
         | Some profile when profile.neighborhood <> "" -> return (profile :: acc)
         | _ -> return acc)
      []
      members
  in
  Lwt_log.ign_info_f "we have %d profiles" (List.length profiles) ;
  let profiles = Ys_list.take 22 profiles in
  try_lwt
    let open Ys_googlemaps in
    let open Ys_googlemaps_types in
    (* bit of hard coding here .. *)
    lwt directions =
      get_directions
        "1168 Greenwich Street San Francisco"
        "SF Zoo"
        (List.map (fun profiles -> profiles.neighborhood) profiles)
    in
    lwt _ =
      context.message_supervisor
        ~subject:"Directions between all members"
        ~content:[
          pcdata "Here are the directions between home & the SF Zoo, picking up all members on the way:" ; br () ;
          br () ;
          ul (List.map (fun profile -> li [ pcdata profile.name ; pcdata " -> " ; pcdata profile.neighborhood ]) profiles) ;
          br () ;
          div
            (List.map
               (fun route ->
                  let total_duration = List.fold_left (fun acc leg -> acc + leg.duration.value + 120) 0 route.legs in
                  let total_duration = total_duration / 60 in
                  div [
                    div [ pcdata "total duration including 2m per stop: " ; pcdata (string_of_int total_duration) ; pcdata "min" ] ;
                    div
                      (List.map
                         (fun leg ->
                            div [
                              div [ pcdata "duration: " ; pcdata leg.duration.text ] ;
                              ul (List.map (fun step -> li [ Unsafe.data step.html_instructions]) leg.steps)
                            ])
                         route.legs)
                  ])
               directions.routes)
        ]
        ()
    in
    return `None
  with exn ->
    lwt _ =
      context.message_supervisor
        ~subject:"Couldn't compute the directions"
        ~content:[
          pcdata "I couldn't compute the directions because of " ; pcdata (Printexc.to_string exn) ; br () ;
          br () ;
          pcdata "You might need to edit some of the profiles to get the full address"
        ]
        ()
    in
  return `None

let new_member__ context member =
  (* let's see if we have a profile *)
  match_lwt get_profile context member with
    Some _ -> return `None
  | None ->
    lwt _ = context.tag_member ~member ~tags:[ tag_to_invite ] in
    return `ExecutePendingInvites

(* profile management *)

let send_profiles context () =
  context.log_info "send profiles" ;
  lwt members = context.search_members ~query:"active" () in
  context.log_info "found %d members" (List.length members) ;
  lwt _ =
    Lwt_list.iter_s
      (fun member ->
         context.log_info "inspecting profile for member %d" member ;
         match_lwt get_profile context member with
           None -> return_unit
         | Some profile ->
           Lwt_list.iter_s
             (fun group ->
                match_lwt context.search_societies ~query:group () with
                  [] ->
                  context.log_info "skipping profile %d because I couldn't locate society %s" member group ;
                  return_unit
                | society :: _ ->
                  lwt _ =
                    context.message_society
                      ~society
                      ~stage:"store_profile"
                      ~subject:"updated profile"
                      ~content:(Yojson_profile.to_string profile)
                      ()
                  in
                  return_unit)
             profile.groups)
      members
  in
  return `None

(* invites. this could be made generic in a component?? ***********************)

let invite context () =
  return `None

let extract_emails context message =
  lwt content = context.get_message_content ~message in
  let emails = Ys_email.get_all_emails content in
  context.log_info "got new invite request, there are %d emails to handle" (List.length emails) ;
  lwt members =
    Lwt_list.fold_left_s
      (fun acc email ->
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
         match_lwt context.is_member ~member with
           true -> return acc
         | false -> return (member :: acc))
      []
      emails
  in
  lwt _ = Lwt_list.iter_s (fun member -> context.tag_member ~member ~tags:[ tag_to_invite ]) members in
  return `ExecutePendingInvites

let execute_pending_invites context () =
  match_lwt context.search_societies ~query:"planner" () with
    [] ->
    context.log_info "couldn't find a planner, not sending invites" ;
    return `None
  | society :: _ ->
    context.log_info "planner found, ask for an updated pitch" ;
    lwt _ =
      context.message_society
        ~society
        ~stage:"retrieve_pitch"
        ~subject:"pitch"
        ~content:""
        ()
    in
    return `None

let extract_pitch context message =
  (* for now the pitch is an activity *)
  try_lwt
    lwt content = context.get_message_content ~message in
    let activity = Yojson_activity.from_string content in
    let now = Calendar.now () in
    context.log_info "now is %d:%d" (Calendar.hour now) (Calendar.minute now) ;
    if Calendar.hour now > 8 && Calendar.hour now < 17 then
      return (`Pitch activity)
    else
      begin
        let date = Calendar.next now (`Day) in
        let date = Calendar.rem date (Calendar.Period.lmake ~hour:(Calendar.hour now) ()) in
        let date = Calendar.add date (Calendar.Period.lmake ~hour:8 ()) in
        let diff = Calendar.sub date now in
        lwt _ =
          context.set_timer
            ~duration:diff
            (`Pitch activity)
        in
        return `None
      end
  with _ ->
    lwt _ =
      context.forward_to_supervisor
        ~message
        ~subject:"Couldn't decode the pitch"
        ~content:[ pcdata "Couldn't get the pitch from the planner" ]
        ()
    in
    return `None

let send_invites context activity =
  lwt members = context.search_members ~query:tag_to_invite () in
  context.log_info "inviting now %d members" (List.length members) ;
  lwt _ =
    Lwt_list.iter_s
      (fun member ->
         lwt _ =
           context.message_member
             ~member
             ~remind_after:(Calendar.Period.lmake ~hour:16 ())
             ~subject:"Looking for a flexible preschool option? Get on the preschool bus!"
             ~content:[
               pcdata "Good morning," ; br () ;
               br () ;
               pcdata "I'm a SF resident, father of two. As many of us do, I've been struggling with preschool options for our children." ; br () ;
               br () ;
               pcdata "Despite the preschool situation, there is no shortage of opportunities to play and learn in the city. The SF Zoo, SF Rec Park and many others, public and private, provide high quality classes that our children could benefit from if we had a way to get them there safely, with a consistent group of little friends and considerate caregivers." ; br () ;
               br () ;
               pcdata "Along with other parents, we are proposing to set up a preschool bus: a safe, on-demand school bus staffed with professionals that would pick up our children and drive them to activities all through the town." ; br () ;
               br () ;
               pcdata "We are doing a first experimental field trip on ";
               pcdata (sprintf "%d/%d" activity.activity_date.month activity.activity_date.day) ;
               pcdata " to the SF Zoo. Parents and children will attend a class there, then will spend some time discovering the animals while making new friends along the way." ; br () ;
               br () ;
               pcdata "Would you like to learn more about it?" ; br () ;
               br () ;
             ]
             ()
         in
         lwt _ = context.untag_member ~member ~tags:[ tag_to_invite ] in
         return_unit
      )
      members
  in
  return `None


let mark_interested_in_general context message =
  lwt member = context.get_message_sender ~message in
  lwt _ = context.tag_member ~member ~tags:[ tag_interested_in_general ] in
  lwt _ = context.set ~key:(key_last_message member) ~value:(string_of_int message) in
  return (`Message message)

let process_join_request context () =
  return `None

let init__ context () =
  lwt _ = context.set ~key:"core-remind-verbatim" ~value:"Hi, have you seen my previous email about the preschool bus? Would you be interested in learning more about it?" in
  return `None

(* the playbook ***************************************************************)

PLAYBOOK

   #import core_remind

*init__

*process_join_request<forward> ~> `Message of email ~> check_if_member_already_exists

(* onboarding process *)

 check_if_member_already_exists ~> `Yes ~> noop
 check_if_member_already_exists ~> `No of int ~> ask_supervisor_to_extract_information<forward> ~> `Message of email ~> store_profile_and_ask_for_missing_elements

 store_profile_and_ask_for_missing_elements<forward> ~> `Message of email ~> store_profile_and_ask_for_missing_elements
 store_profile_and_ask_for_missing_elements ~> `ThankMember of int ~> thank_member
 store_profile_and_ask_for_missing_elements ~> `AskMemberForMissingFields of (int * profile_fields) ~> ask_member_for_missing_fields<forward> ~> `Message of email ~> store_reply ~> `Message of email ~> ask_supervisor_to_extract_information

 new_member__ ~> `ExecutePendingInvites ~> execute_pending_invites

*group_all_members

*send_profiles

*invite<forward> ~> `Message of email ~> extract_emails ~> `ExecutePendingInvites ~> execute_pending_invites<forward> ~> `Message of email ~> extract_pitch ~> `Pitch of activity ~> send_invites

send_invites ~> `InterestedInGeneral of email ~> mark_interested_in_general ~> `Message of int ~> ask_supervisor_to_extract_information

PROPERTIES
  - "Your duties", "None! All you have is to show up on time with your child at the pickup spot."
