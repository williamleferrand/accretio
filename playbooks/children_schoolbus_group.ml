(*
 * children_schoolbus
 *
 * this playbook organizes field trips for a group of children
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

open Children_schoolbus_types
open Children_schoolbus_tools

let author = "william@accret.io"
let name = "Children schoolbus group"
let description = "this playbook organizes field trips for a group of children"
let version = 0
let tags = ""

(* initially let's do pickups at playgrounds *)

(* some keys ******************************************************************)

let key_pickup_point = "pickup-point"

(* some tags ******************************************************************)

let tag_asked = "asked"
let tag_agrees_with_pickup_point = "agreeswithpickuppoint"

(* the stages *****************************************************************)

(******************************************************************************)
(* Setting up the group                                                       *)
(******************************************************************************)

let init__ context () =
  context.log_info "calling init for the society" ;
  lwt _ =
    context.message_supervisor
      ~subject:"Welcome"
      ~content:[
        pcdata "Greetings," ; br () ;
        br () ;
        pcdata "This society just got created. What will be the pickup point for the group?" ; br () ;
      ]
      ()
  in
  return `None

let extract_pickup_point context message =
  match_lwt context.get ~key:key_pickup_point with
    Some _ ->
    lwt _ =
      context.reply_to
        ~message
        ~content:[ pcdata "Sorry the pickup point can only be updated from the control panel" ]
        ()
    in
    return `None
  | None ->
    lwt content = context.get_message_content ~message in
    lwt _ = context.set ~key:key_pickup_point ~value:content in
    lwt _ =
      context.reply_to
        ~message
        ~content:[
          pcdata "Thanks, I registered the following pickup point:" ; br () ;
          br () ;
          i [ pcdata content ] ; br () ;
          br ()
        ]
        ()
    in
    return `None

let new_member__ context member =
  context.log_info "we have a new member, %d" member ;
  match_lwt context.check_tag_member ~member ~tag:tag_asked with
    true -> return `None
  | false ->
    match_lwt context.get ~key:key_pickup_point with
      None ->
      (* let's ask if the member has a preferred pickup point *)
      return (`AskMemberForPreferredPickupPoint member)
    | Some pickup_point ->
      lwt salutations = salutations member in
      lwt _ =
        context.message_member
          ~member
          ~remind_after:(Calendar.Period.lmake ~hour:36 ())
          ~subject:"Preschool on wheels - quick question"
          ~content:[
            salutations ; br () ;
            br () ;
            pcdata "I'm making progress on a proposal for a first trip. No date yet, but the destination would very likely be the SF Zoo. I am working on getting firm quotes from various charter companies." ; br () ;
            br () ;
            pcdata "To make things easier for them what would you think of doing the pickup from " ; pcdata pickup_point ; pcdata " sometime around 8:30am? We could go doorstep to doorstep later." ; br () ;
          ]
          ()
      in
      lwt _ = context.tag_member ~member ~tags:[ tag_asked ] in
      return `None

let ask_member_for_preferred_pickup_point context member =
  match_lwt context.check_tag_member ~member ~tag:tag_asked with
    true -> return `None
  | false ->
    context.log_info "asking %d for a preferred pickup point" member ;
    lwt salutations = salutations member in
    lwt _ =
        context.message_member
          ~member
          ~remind_after:(Calendar.Period.lmake ~hour:36 ())
          ~subject:"Preschool on wheels - quick question"
          ~content:[
            salutations ; br () ;
            br () ;
            pcdata "I'm making progress on a proposal for a first trip. No date yet, but the destination would very likely be the SF Zoo. I am working on getting firm quotes from various charter companies." ; br () ;
            br () ;
            pcdata "To make things easier for them it would help if we could do the pickup at a playground. Would that work with you and would you have a preferred playground? We could go doorstep to doorstep later." ; br () ;
            br () ;
            pcdata "Thanks!" ; br () ;
          ]
          ()
    in
    lwt _ = context.tag_member ~member ~tags:[ tag_asked ] in
    return `None

let mark_agrees_with_pickup_point context message =
  lwt member = context.get_message_sender ~message in
  match_lwt context.check_tag_member ~member ~tag:tag_agrees_with_pickup_point with
    true -> return `None
  | false ->
    lwt _ = context.tag_member ~member ~tags:[ tag_agrees_with_pickup_point ] in
    return `None

let count_participants context () =
  lwt members = context.search_members ~query:tag_agrees_with_pickup_point () in
  lwt members =
    Lwt_list.map_s
      (fun uid ->
         lwt name, preferred_email = $member(uid)->(name, preferred_email) in
         return (li [ pcdata name ; pcdata " -> " ; pcdata preferred_email ]))
      members
  in
  lwt _ =
    context.message_supervisor
      ~subject:"Participants count"
      ~content:[
        pcdata "Greetings," ; br () ;
        br () ;
        pcdata "There are " ; pcdata (string_of_int (List.length members)) ; pcdata " participants:" ; br () ;
        br ();
        ul members
      ]
      ()
  in
  return `None

let validate_pricing context () =
  return `None

let tag_asked_for_pricing = "askedforpricing"

let ask_members_for_their_opinion_on_pricing context message =
  try
    lwt content = context.get_message_content ~message in
    let quotes = Yojson_quotes.from_string content in
    lwt members = context.search_members (sprintf "-%s %s" tag_asked_for_pricing tag_agrees_with_pickup_point) () in
    context.log_info "asking %d members for their opinion on pricing" (List.length members) ;
    let quotes =
      List.map
        (fun quote ->
           li [ pcdata quote.description ;
                pcdata ", ends up costing " ;
                pcdata (sprintf "$%.2f per seat" (quote.cost /. (float_of_int quote.number_of_seats))) ]
        )
        quotes
    in
    lwt _ =
      Lwt_list.iter_s
        (fun member ->
           lwt greetings = salutations member in
           lwt _ =
             context.message_member
               ~remind_after:(Calendar.Period.lmake ~hour:36 ())
               ~member
               ~subject:"Preschool bus"
               ~content:[
                 greetings ; br () ;
                 br () ;
                 pcdata "I have made some progress this week about the preschool bus; here are the quotes I got for a first full day trip to the SF Zoo:" ; br () ;
                 ul quotes ;
                 br () ;
                 pcdata "What are your thoughts? Cost would definitely go down if/once we do these trips on a regular basis, but for a first experiment we don't have much leverage." ; br ()  ;
                 br () ;
                 pcdata "Costs for the activity itself and a lunch would be below $30 total for a child and one parent. Does anything here sound reasonable to you?"
               ]
               ()
           in
           lwt _ = context.tag_member ~member ~tags:[ tag_asked_for_pricing ] in
           return_unit
        )
        members
    in
    return `None
  with _ ->
    lwt _ =
      context.reply_to
        ~message
        ~content:[ pcdata "Couldn't understand your message" ]
        ()
    in
    return `None

let tag_too_expensive = "tooexpensive"
let tag_pricing_ok = "pricingok"

let pricing_too_much context message =
  lwt member = context.get_message_sender ~message in
  lwt _ = context.tag_member ~member ~tags:[ tag_too_expensive ] in
  lwt _ =
    context.reply_to
      ~message
      ~content:[ pcdata "Indeed it is a bit steep. Still looking at different options (even carpooling, maybe!), what would be a decent cost for a day trip to the zoo, in your opinion, including transportation?" ]
      ()
  in
  return `None

let pricing_ok context message =
  lwt member = context.get_message_sender ~message in
  lwt _ = context.tag_member ~member ~tags:[ tag_pricing_ok ] in
  lwt _ =
    context.reply_to
      ~message
      ~content:[ pcdata "Ok thanks for the feedback. Let me try to keep this rolling .." ]
      ()
  in
  return `None

(* the activity suggestion stages *********************************************)

let activity_uid = sprintf "%04d%04d%04d"
let tag_already_suggested = sprintf "alreadysuggested%04d%04d%04d"
let key_activity_uid = "activity-uid"

let suggest_activity context () =
  return `None

let suggest_activity_to_members context message =
  context.log_info "suggesting a new activity to all the members" ;
  lwt content = context.get_message_content ~message in
  match_lwt context.get ~key:key_pickup_point with
    None -> return `None
  | Some meeting_point ->
  try_lwt
    let activity = Yojson_activity.from_string content in
    let tag_already_suggested = tag_already_suggested activity.activity_date.year activity.activity_date.month activity.activity_date.day in
    let activity_uid = activity_uid activity.activity_date.year activity.activity_date.month activity.activity_date.day in
    lwt members = context.search_members ~query:(sprintf "%s -%s" tag_agrees_with_pickup_point tag_already_suggested) () in
    context.log_info "suggesting a new activity to %d members" (List.length members) ;

    let attachments =
      List.map
        (fun attachment ->
           { Object_message.filename = attachment.filename ;
             Object_message.content_type = attachment.content_type ;
             Object_message.content = attachment.content ;
           })
        activity.activity_attachments
    in

    let format_date_time date time =
      let date = Calendar.lmake
          ~year:date.year
          ~month:date.month
          ~day:date.day
               ~hour:time.hour
               ~minute:time.minute ()
      in
      pcdata (CalendarLib.Printer.Calendar.sprint "%I:%M %p" date)
    in

    let format_date date =
      let date = Calendar.lmake
          ~year:date.year
          ~month:date.month
          ~day:date.day
          ~hour:0
          ~minute:0 ()
      in
      pcdata (CalendarLib.Printer.Calendar.sprint "%B %d (it's a %A)" date)
    in

    lwt _ =
      Lwt_list.iter_s
        (fun member ->
           match_lwt get_profile context member with
             None ->
             context.log_info "skipping member %d, there is no profile" member ;
             (* here we should pull the profile from the parent society *)
             return_unit
           | Some profile ->
             match List.exists (fun child -> child.age_in_months >= activity.activity_min_age_in_months &&
                                             child.age_in_months <= activity.activity_max_age_in_months) profile.children with
             | false -> context.log_info "member %d has no children in the range" member ; return_unit
             | true ->
               lwt salutations = salutations member in
               lwt _ =
                 context.message_member
                   ~member
                   ~remind_after:(Calendar.Period.lmake ~hour:16 ())
                   ~data:[ key_activity_uid, activity_uid ]
                   ~subject:activity.activity_title
                   ~attachments
                   ~content:
                     (match activity.activity_status with
                        Confirmed activity_confirmed ->
                        [
                          salutations ; br () ;
                          br () ;
                          pcdata activity.activity_description ; br () ;
                          br () ;
                          pcdata "Here is the proposed schedule for " ; format_date activity.activity_date ; pcdata ":" ; br () ;
                          br () ;
                          pcdata "The meeting point would be " ; pcdata meeting_point ; pcdata "." ; br () ;
                          ul
                            (List.map
                               (fun step ->
                                  li [
                                    b (match step.step_time with
                                          TimeRange (t1, t2) ->
                                          [ format_date_time activity.activity_date t1 ;
                                            pcdata " - " ;
                                            format_date_time activity.activity_date t2 ;
                                          ]
                                        | Time t ->
                                          [ format_date_time activity.activity_date t ]) ;
                                    pcdata ": " ;
                                    pcdata step.step_description
                                  ])
                          activity.activity_steps) ;
                          pcdata activity_confirmed.activity_price_description ; br () ;
                          br () ;
                          pcdata activity_confirmed.activity_price_remark ; br () ;
                          br () ;
                          pcdata "What do you think? Are you in :) ?" ; br () ;
                        ]
                      | Suggestion activity_suggestion ->
                          [
                          salutations ; br () ;
                          br () ;
                          pcdata activity.activity_description ; br () ;
                          br () ;
                          pcdata "The meeting point would be " ; pcdata meeting_point ; pcdata "." ; br () ;
                          br () ;
                          pcdata "Here is the proposed schedule for " ; format_date activity.activity_date ; pcdata ":" ; br () ;
                          ul
                            (List.map
                               (fun step ->
                                  li [
                                    b (match step.step_time with
                                          TimeRange (t1, t2) ->
                                          [ format_date_time activity.activity_date t1 ;
                                            pcdata " - " ;
                                            format_date_time activity.activity_date t2 ;
                                          ]
                                        | Time t ->
                                          [ format_date_time activity.activity_date t ]) ;
                                    pcdata ": " ;
                                    pcdata step.step_description
                                  ])
                          activity.activity_steps) ;
                          pcdata activity_suggestion.activity_suggestion ; br () ;
                          br () ;
                          pcdata "What do you think? Would you be interested?" ; br () ;
                        ]
                     )
                   ()
               in
               lwt _ = context.tag_member ~member ~tags:[ tag_already_suggested ] in
               return_unit)
        members
    in
    return `None
  with _ ->
    lwt _ = context.reply_to
        ~message
        ~content:[ pcdata "Something went wrong" ]
        ()
    in
    return `None

let tag_declined = sprintf "declined%s"

let decline_activity context message =
  match_lwt context.get_message_data ~message ~key:key_activity_uid with
  | None ->
    lwt _ =
      context.forward_to_supervisor
        ~message
        ~subject:"Couldn't find activity_uid"
        ~content:[ pcdata "Couldn't find activity_uid" ]
        ()
    in
    return `None
  | Some activity_uid ->
    lwt member = context.get_message_sender ~message in
    lwt _ = context.tag_member ~member ~tags:[ tag_declined activity_uid ] in
    return `None

(* profile management *)

let store_profile context () =
  return `None

let decode_and_store_profile context message =
  lwt content = context.get_message_content ~message in
  let profile = Yojson_profile.from_string content in
  context.log_info "we got a profile for member %s" profile.email ;
  lwt _ = set_profile context profile in
  return `None

(* the playbook ***************************************************************)

PLAYBOOK

#import core_remind

*init__<forward> ~> `Message of email ~> extract_pickup_point

new_member__ ~> `AgreesWithPickupPoint of email ~> mark_agrees_with_pickup_point
new_member__ ~> `AskMemberForPreferredPickupPoint of int ~> ask_member_for_preferred_pickup_point

*count_participants

*validate_pricing<forward> ~> `Message of email ~> ask_members_for_their_opinion_on_pricing ~> `PricingTooMuch of email ~> pricing_too_much
                                                   ask_members_for_their_opinion_on_pricing ~> `PricingOk of email ~> pricing_ok

*suggest_activity<forward> ~> `Message of email ~> suggest_activity_to_members ~> `DeclineActivity of email ~> decline_activity
*store_profile<forward> ~> `Message of email ~> decode_and_store_profile

PROPERTIES
  - "Your duties", "None"
