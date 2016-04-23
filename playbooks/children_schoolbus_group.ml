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
let tag_confirmed = "confirmed" (* <- people that have accepted the pickup spot *)
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

(* the playbook ***************************************************************)

PLAYBOOK

#import core_remind

*init__<forward> ~> `Message of email ~> extract_pickup_point

new_member__ ~> `AgreesWithPickupPoint of email ~> mark_agrees_with_pickup_point
new_member__ ~> `AskMemberForPreferredPickupPoint of int ~> ask_member_for_preferred_pickup_point

*count_participants

*validate_pricing<forward> ~> `Message of email ~> ask_members_for_their_opinion_on_pricing

PROPERTIES
  - "Your duties", "None"
