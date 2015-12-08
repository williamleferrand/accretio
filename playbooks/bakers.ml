(* the recipes are instantiated by a leader before execution *)
(* there is one leader per recipe. maybe the vocabulary isn't great there? *)


(** fair warning to developers: your receipe must be re-entrant, meaning that
    the flow should be stoppable & restartable.
    if you don't use mutable datastructures other than what the Leader module is
    providing you, you will be fine.
    if you do, you have to be careful. ask on the mailing list for more info *)

(* do we want to pass the context as an argument, or is it on the functor?

   passing the context as an argument is very "ocaml like"
   passing it as a module makes it easier for the developer. also, it makes the
   playbook a functor, thus preventing any kind of cross-context side effect.
   that doesn't prevent someone from dynlinking some other code. we'll need a
   manual review anyways *)

open Printf
open Lwt
open CalendarLib

open Eliom_content.Html5
open Eliom_content.Html5.D

open Ys_uid

open Api


let author = "william@accret.io"
let name = "Recurring dinners between friends"
let description = "Meet with friends & have dinner outside on a regular basis"

let version = 0

(* the individual stages ****************************************************)

let call_supervisor context () =
  lwt _ =
    context.message_supervisor
      ~subject:"please review manually the context"
      ~content:[
        pcdata "You're needed .."
      ]
  in
  return `None

let pick_up_an_organizer context () =
  context.log_error "hello" ;
  let now = Calendar.now () in
  let week = Calendar.year now * 53 + Calendar.week now in
  lwt week =
    match_lwt context.get ~key:"force_week" with
      None -> return week
    | Some week ->
      context.log_info "found forced week %s" week ;
      return (int_of_string week) in
  match_lwt context.search_members ~query:(sprintf "active") () with
    [] ->
    context.log_error "no candidate available on week %d, the event won't happen" week ;
    return (`NoCandidateAvailable)
  | candidate :: _ ->
    return (`MessageCandidate candidate)

let message_candidate context member =
  Library.message_with_timeout
    context
     ~member
     ~timeout:(Calendar.Period.lmake ~hour:6 ())
     ~subject:"Daddy's diner"
     ~content:[ pcdata "Hi, do you mind picking up a place?" ]

let decode_candidate_message context message =
  Library.decode_message_yes_no
    context
    ~message

let extract_member_from_message_when_yes context message = Library.member_from_message context ~message
let extract_member_from_message_when_no context message = Library.member_from_message context ~message

let ask_member_for_a_place context member =
  Library.message_with_timeout
    context
    ~member
    ~timeout:(Calendar.Period.lmake ~hour:6 ())
    ~subject:"can you suggest a place?"
    ~content:[ pcdata "We're planning to meet on Thusrday night as usual. Could you suggest a place? Please send something within 6 hours, a yelp link would be awesome" ]

let member_is_not_available context member =
  context.log_info "member %d isn't available. tagging & moving forward" member ;
  lwt _ = context.tag_member ~member ~tags:[ "not_available" ] in
  return `PickUpAnotherCandidate

let decode_message_manually context message =
  context.log_info "message %d can't be parsed, forwarding it to the author" message ;
  lwt _ = context.forward_to_supervisor ~subject:"can you decode?" ~message in
  return `None

let decode_organizer_suggestion context message =
  lwt content = context.get_message_content ~message in
  (* http://www.yelp.com/biz/tadu-ethiopian-kitchen-san-francisco-3?utm_campaign=www_business_share_popup&utm_medium=copy_link&utm_source=(direct) *)
  let yelp_regexp = Str.regexp "yelp.com/\\([^' ' \t]+\\)" in
  try
    let _ = Str.search_forward yelp_regexp content 0 in
    let url = Str.matched_group 1 content in
    return (`Yelp url)
  with Not_found -> return (`Unknown message)

let forward_suggestion_to_all_members context suggestion =
  lwt members = context.search_members ~query:"NOT organizer" () in
  lwt _ = Lwt_list.iter_p
      (fun member ->
         context.message_member ~member ~subject:"Up for a dinner next thursday" ~content:[
           pcdata "what about " ; pcdata suggestion
         ])
      members
  in
  return `None

let prepare_dashboard context () =
  context.log_info "time to prepare the weekly dashboard" ;
  match_lwt context.search_members ~query:"has_replied_yes" () with
    [] -> return `NoParticipants
  | _ as participants -> return (`Participants participants)

let ask_organizer_to_book context participants =
  let organizer = 1 in
  Library.message_with_timeout
    context
    ~member:organizer
    ~timeout:(Calendar.Period.lmake ~hour:6 ())
    ~subject:"can you book the place?"
    ~content:[ pcdata "Could you book that yelp place? there are " ; pcdata (string_of_int (List.length participants)) ]

let decode_organizer_response context message =
  return `Unknown

let confirm_to_all_participants context _ =
  lwt participants = context.search_members ~query:"has_replied_yes" () in
  lwt _ =
    Lwt_list.iter_p
      (fun member ->
         context.message_member ~member ~subject:"dinner confirmed!" ~content:[
           pcdata "see you on thursday!"
         ])
      participants in
  return `None

let forward_suggestion_to_all_members context yelp =
  lwt participants = context.search_members ~query:"*" () in
  lwt _ =
    Lwt_list.iter_p
      (fun member ->
         context.message_member
           ~member
           ~subject:"will you come next thursday?"
           ~content:[
             pcdata "this is the place: " ; pcdata yelp
           ])
      participants in
  return `None

let decode_member_message context message =
  Library.decode_message_yes_no
    context
    ~message

let tag_member_as_coming context message =
  return `None

let tag_member_as_not_coming context message =
  return `None


(* the graph ****************************************************************)

PLAYBOOK

  (* every week, pickup an an organizer *)

  pick_up_an_organizer ~> `NoCandidateAvailable ~> call_supervisor
  pick_up_an_organizer ~> `MessageCandidate of int ~> message_candidate ~> `TimedOut ~> pick_up_an_organizer
                                                      message_candidate ~> `Message of int ~> decode_candidate_message

 (* decode what the organizer says, pick up someone else if he/she bails out *)

                                             extract_member_from_message_when_yes ~> `NoMemberFound ~> call_supervisor
  decode_candidate_message ~> `Yes of int ~> extract_member_from_message_when_yes ~> `Member of int ~> ask_member_for_a_place

                                             extract_member_from_message_when_no ~> `NoMemberFound ~> call_supervisor


  decode_candidate_message  ~> `No of int  ~> extract_member_from_message_when_no  ~> `Member of int ~> member_is_not_available ~> `PickUpAnotherCandidate ~> pick_up_an_organizer

  decode_candidate_message ~> `Unknown of int ~> decode_message_manually

  (* if we have a suggestion, send it around to see what people say *)

  ask_member_for_a_place ~> `TimedOut     ~> pick_up_an_organizer
  ask_member_for_a_place ~> `Message of int ~> decode_organizer_suggestion ~> `Unknown of int ~> decode_message_manually
                                               decode_organizer_suggestion ~> `Yelp of string ~> forward_suggestion_to_all_members

  (* count the number of replies *)

  forward_suggestion_to_all_members ~> `Message of int ~> decode_member_message ~> `Yes of int ~> tag_member_as_coming
                                                          decode_member_message ~> `No of int ~> tag_member_as_not_coming
                                                          decode_member_message ~> `Unknown of int ~> decode_message_manually

  (* prepare a dashboard a few days before so that the organizer can book the place *)

  *prepare_dashboard ~> `NoParticipants ~> call_supervisor
   prepare_dashboard ~> `Participants of int list ~> ask_organizer_to_book ~> `TimedOut ~> call_supervisor
                                                    ask_organizer_to_book ~> `Message of int ~> decode_organizer_response ~> `Booked ~> confirm_to_all_participants
                                                                                              decode_organizer_response ~> `Unknown ~> call_supervisor



(* cron jobs on some states so that the receipe starts on a regular basis *)

 CRON pick_up_an_organizer "0 * * * * *"
 CRON prepare_dashboard "0 11 * * * *"
