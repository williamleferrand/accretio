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

let author = "william@accret.io"
let name = "Children schoolbus"
let description = "this playbook organizes a preschool on wheels by proposing activities, booking a bus and organizing pickups/dropoffs"
let version = 0
let tags = ""

(* keys ***********************************************************************)

let key_original_message = "original-message"
let key_profile = sprintf "profile-%d"
let key_last_message = sprintf "last-message-%d"

(* the profile of a member ****************************************************)

type child = {
  name : string ;
  age_string : string ;
  age_in_months : int ;
} with yojson

type profile = {
  uid : int ;
  email : string ;
  name : string ;
  children : child list ;
  neighborhood : string ;
  schedule : string ;
} with yojson

type profile_field = Name | Children | Neighborhood | Schedule with yojson
type profile_fields = profile_field list with yojson

let default_child =
  {
    name = "";
    age_string = "" ;
    age_in_months = 0  ;
  }

let default_profile uid name email = {
  uid ;
  email ;
  name ;
  children = [ default_child ] ;
  neighborhood = "" ;
  schedule = ""
}

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

let get_profile context member =
  match_lwt context.get ~key:(key_profile member) with
    None -> return_none
  | Some profile ->
    try
      return (Some (Yojson_profile.from_string profile))
    with _ -> return_none

(* the stages *****************************************************************)

(******************************************************************************)
(* ONBOARDING                                                                 *)
(******************************************************************************)

let check_if_member_already_exists context message =
  lwt sender = context.get_message_sender ~message in
  lwt members = $society(context.society)->members in
  match Ys_uid.Edges.mem sender members with
    true -> return `Yes
  | false -> return (`No message)

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
      ~data:[ key_original_message, string_of_int message ]
      ~subject:"Your input is needed"
      ~content:[
        pcdata "Greetings," ; br () ;
        br () ;
        pcdata "Here is a message from " ; pcdata email ; pcdata " with some profile information" ; br () ;
        br () ;
        pcdata "Could you fill up the Json below? (and return only the json)" ; br () ;
        br () ;
        pcdata stringified ; br () ;
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

    lwt fields =
      match_lwt context.get ~key:(key_last_message profile.uid) with
        Some _ -> return [ Name ; Children ; Neighborhood ; Schedule ]
      | None -> return [ Children ; Neighborhood ; Schedule ]
    in

    let missing_fields =
      List.filter
        (function
          | Name -> profile.name = ""
          | Children ->(try profile.children = [] || ((List.hd profile.children).age_string = "") with _ -> true)
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
      [] -> return (`ThankMember profile.uid)
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
  (* we add the member at the end of the onboarding process *)
  lwt _ = context.add_member ~member in
  match_lwt context.get ~key:(key_last_message member) with
  | None ->
    let _ =
      context.message_member
        ~member
        ~subject:"Preschool on wheels"
        ~content:[
        salutations ; br () ;
        br () ;
        pcdata "Thanks for your note. We would need a little more participants to fill up the first bus, do you know people who would be interested in joining us?"
      ]
      () in
    return `None
  | Some message ->
    let message = int_of_string message in
    let _ =
      context.reply_to
        ~message
        ~content:[
          pcdata "Thanks, that's great. We would need a little more participants to fill up the first bus, do you know people who would be interested in joining us?"
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

  match_lwt context.get ~key:(key_last_message member) with
  | None ->
    let _ =
      context.message_member
        ~member
        ~subject:"Preschool on wheels"
        ~content:[
          salutations ; br () ;
          br () ;
          pcdata "Thanks for your note! I'm very excited by the feedback that we had so far, let's see if we can get enough people to set up a first field trip." ; br () ;
          br () ;
          (match List.length fields with 1 -> pcdata "I've one more question. " | _ -> pcdata "I've a few more questions :) ") ; questions ; br () ;
          br () ;
          pcdata "Thanks," ; br () ;
          pcdata signature ;
      ]
      () in
    return `None
  | Some message ->
    let message = int_of_string message in
    let _ =
      context.reply_to
        ~message
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





(* the playbook ***************************************************************)

PLAYBOOK

   #import core_join_request

   process_join_request<forward> ~> `Message of email ~> check_if_member_already_exists

      check_if_member_already_exists ~> `Yes ~> noop
      check_if_member_already_exists ~> `No of int ~> ask_supervisor_to_extract_information<forward> ~> `Message of email ~> store_profile_and_ask_for_missing_elements

        store_profile_and_ask_for_missing_elements<forward> ~> `Message of email ~> store_profile_and_ask_for_missing_elements
        store_profile_and_ask_for_missing_elements ~> `ThankMember of int ~> thank_member
        store_profile_and_ask_for_missing_elements ~> `AskMemberForMissingFields of (int * profile_fields) ~> ask_member_for_missing_fields<forward> ~> `Message of email ~> store_reply ~> `Message of email ~> ask_supervisor_to_extract_information

*group_all_members

PROPERTIES
  - "Your duties", "None! All you have is to show up on time with your child at the pickup spot."
