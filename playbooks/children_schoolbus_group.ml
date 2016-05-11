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
            pcdata "I'm working on the first trip to the zoo on 5/27." ; br () ;
            br () ;
            pcdata "Does " ; pcdata pickup_point ; pcdata " work for you as a pickup point?" ; br () ;
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
let key_activity_reference = "activity-uid"

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
    lwt _ = context.set ~key:(activity.activity_reference) ~value:(Yojson_activity.to_string activity) in
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
                   ~data:[ key_activity_reference, activity.activity_reference ]
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

let with_activity context message f =
  match_lwt context.get_message_data ~message ~key:key_activity_reference with
  | None ->
    lwt _ =
      context.forward_to_supervisor
        ~message
        ~subject:"Couldn't find activity_uid"
        ~content:[ pcdata "Couldn't find activity_uid" ]
        ()
    in
    return `None
  | Some activity_reference ->
    match_lwt context.get ~key:activity_reference with
      None ->
      lwt _ =
        context.forward_to_supervisor
          ~message
          ~subject:"The activity doesn't exist in this group"
          ~content:[ pcdata "Couldn't figure out the activity" ]
          ()
      in
      return `None
    | Some activity ->
      let activity = Yojson_activity.from_string activity in
      f activity

let decline_activity_and_stay context message =
  with_activity context message
    (fun activity ->
       lwt member = context.get_message_sender ~message in
       lwt _ = context.tag_member ~member ~tags:[ tag_declined activity.activity_reference ] in
       lwt _ = context.reply_to
                 ~message
                 ~content:[
                   pcdata "Ok, no problem, thanks for responding. I will keep you posted about future events!"
                 ]
                 ()
       in
       return `None)

let decline_activity_and_leave context message =
  with_activity context message
    (fun activity ->
       lwt member = context.get_message_sender ~message in
       lwt _ = context.tag_member ~member ~tags:[ tag_declined activity.activity_reference ] in
       lwt _ =
         context.reply_to
           ~message
           ~content:[
             pcdata "Ok, no problem, thanks for responding. I am removing you from this list"
           ]
           ()
       in
       context.log_info "removing member %d from the list" member ;
       lwt _ = context.remove_member ~member in
       return `None)

let label_remind_activity = sprintf "remindactivity%d%s"

let remind_in_one_week context message =
  with_activity context message
    (fun activity ->
       lwt _ =
         context.reply_to
           ~message
           ~content:[
             pcdata "Ok, no problem, thanks for responding. I will get back in touch in a week!"
           ]
           ()
       in
       lwt member = context.get_message_sender ~message in
       lwt _ =
         context.set_timer
           ~duration:(Calendar.Period.lmake ~day:7 ())
           ~label:(label_remind_activity member activity.activity_reference)
           (`RemindActivity message)
       in
       return `None)

let remind_activity context message =
  with_activity context message
    (fun activity ->
       lwt member = context.get_message_sender ~message in
       lwt salutations = salutations member in
       lwt _ =
         context.reply_to
           ~message
           ~content:[
             salutations ; br () ;
             br () ;
             pcdata "We are still planning to go to " ; pcdata activity.activity_summary ; br () ;
             br () ;
             pcdata "Would you like to join us?" ; br ()
           ]
           ()
       in
       return `None)

let with_planner context f =
  match_lwt context.search_societies ~query:"planner" () with
    [] ->
    lwt _ =
      context.message_supervisor
        ~subject:"Missing planner"
        ~content:[ pcdata "couldn't locate planner" ]
        ()
    in
    return `None
  | planner :: _ ->
    f planner

let key_original_message = "original-message"

let join_activity context message =
  with_activity context message
    (fun activity ->
       with_planner context
         (fun planner ->
            lwt member = context.get_message_sender ~message in
            lwt _ =
              context.message_society
                ~society:planner
                ~data:[ key_original_message, string_of_int message ;
                        key_activity_reference, activity.activity_reference ]
                ~stage:"request_lock_spot"
                ~subject:"block spot"
                ~content:(Yojson_request_lock_spots.to_string {
                    request_lock_spots_member = member ;
                    request_lock_spots_activity_uid = activity.activity_uid ;
                    request_lock_spots_count = 1
                  })
                ()
            in
            return `None))

let with_original_message context message f =
  match_lwt context.get_message_data ~message ~key:key_original_message with
    None ->
    lwt _ = context.forward_to_supervisor
        ~message
        ~subject:"Message doesn't have original message"
        ~content:[]
              ()
    in
    return `None
  | Some original_message -> f (int_of_string original_message)

let ask_payment context message =
  with_activity context message
    (fun activity ->
       with_original_message context message
         (fun original_message ->
            lwt content = context.get_message_content ~message in
            match Yojson_reply_lock_spots.from_string content with
              EventFull remaining ->
              lwt _ =
                context.reply_to
                  ~message:original_message
                  ~content:[ pcdata "Sorry, there are only " ; pcdata (string_of_int remaining) ; pcdata " spots left ;(" ]
                  ()
              in
              return `None
            | EventLock lock ->
              (* let's hardcode the cost for now *)
              lwt member = context.get_message_sender ~message:original_message in
              let evidence =
                List.map
                  (fun attachment ->
                     { Object_message.filename = attachment.filename ;
                       Object_message.content_type = attachment.content_type ;
                       Object_message.content = attachment.content ;
                     })
                  lock.lock_attachments
              in

              match_lwt context.request_payment
                          ~member
                          ~label:"One spot for the SF Zoo class"
                          ~evidence
                          ~amount:30.0
                          ~on_success:(fun uid -> `PaymentSuccess (uid, activity.activity_uid, original_message))
                          ~on_failure:(fun uid -> `PaymentFailure uid) with
              None ->
                lwt _ =
                  context.forward_to_supervisor
                    ~message
                    ~subject:"Payment creation failed"
                    ~content:[ pcdata "Couldn't create the payment object" ]
                    ()
                in
                return `None
              | Some payment ->
                lwt link = context.payment_direct_link ~payment in
                lwt salutations = salutations member in
                lwt _ =
                  context.reply_to
                    ~message:original_message
                    ~remind_after:(Calendar.Period.lmake ~hour:36 ())
                    ~content:[
                      salutations ; br () ;
                      br () ;
                      pcdata "Great news, the event is filling up!" ; br () ;
                      br () ;
                      pcdata "I still haven't finalized the transportation solution (I apologize for the sloppiness, all this is very experimental) but the final cost will definitely be at or below $80 total as promised (and it will likely be less than that)." ; br () ;
                      br () ;
                      pcdata "However, I booked the class at the Zoo and they are now asking for the children's real names. The cost is $30 and it includes the zoo entrance for one adult and one children, as well as the class itself (see " ; Raw.a ~a:[ a_href (uri_of_string (fun () -> "http://www.sfzoo.org/learn/childrens-programs.htm")) ] [ pcdata "here" ]; pcdata ")" ; br () ;
                      br () ;
                      pcdata "If you wish to join us for this first trip, please send me the payment for the class, ideally using the link below. Once I receive your payment I'll do the name update and send you the ticket for the class, so that at least that part is set." ; br () ;
                      br () ;
                      Raw.a ~a:[ a_href (uri_of_string (fun () -> link)) ] [ pcdata link ] ; br () ;
                      br () ;
                      pcdata "If you have any questions or if you prefer doing Paypal, please let me know." ; br () ;
                      br () ;
                      pcdata "Thanks" ; br () ;
                      br () ;
                      pcdata "William"
                    ]
                    ()
                in
                return `None))


(* profile management *)

let store_profile context () =
  return `None

let decode_and_store_profile context message =
  lwt content = context.get_message_content ~message in
  let profile = Yojson_profile.from_string content in
  context.log_info "we got a profile for member %s" profile.email ;
  lwt _ = set_profile context profile in
  return `None

let payment_success context (payment, activity, original_message) =
  with_planner context
    (fun planner ->
       context.log_info "payment success for payment %d" payment ;
       lwt _ =
         context.message_supervisor
           ~subject:"Payment success"
           ~content:[ pcdata "Investigate payment success" ]
           ()
       in
       lwt _ =
         context.message_society
           ~society:planner
           ~stage:"request_confirm_booking"
           ~data:[ key_original_message, string_of_int original_message ]
           ~subject:"confirm booking"
           ~content:(Yojson_request_confirm_booking.to_string
                       {
                         request_confirm_booking_activity = activity ;
                         request_confirm_booking_payment = payment ;
                       })
           ()
       in
       return `None)

let payment_failure context payment =
  context.log_info "payment failure for payment %d" payment ;
  lwt _ =
    context.message_supervisor
      ~subject:"Payment failure"
      ~content:[ pcdata "Investigate payment failure" ]
      ()
  in
  return `None

let send_confirmation context message =
  with_original_message context message
    (fun message ->
       lwt content = context.get_message_content ~message in
       let confirmation = Yojson_reply_confirm_booking.from_string content in
       let activity = confirmation.reply_confirm_booking_activity in
       lwt member = context.get_message_sender ~message in
       match confirmation.reply_confirm_booking_count with
         1 ->
         lwt _ =
           context.reply_to
             ~message
             ~remind_after:(Calendar.Period.lmake ~hour:12 ())
             ~data:[ key_activity_reference, activity.activity_reference ]
             ~content:[ pcdata "Thanks for your payment, you're in!" ; br () ;
                        br () ;
                        (match confirmation.reply_confirm_booking_count with
                           1 -> pcdata "You have booked one spot total for this activity."
                         | n -> pcdata (sprintf "You have booked %d spots total for this activity." n)) ; br () ;
                        br () ;
                        pcdata "The Zoo is asking for the date of birth of your child, his name, and your mobile number - could you send me the information so that I can update the ticket?" ; br () ;
                      ]
             ()
         in
         return `None
       | _ ->
         lwt _ =
           context.reply_to
             ~message
             ~remind_after:(Calendar.Period.lmake ~hour:12 ())
             ~data:[ key_activity_reference, activity.activity_reference ]
             ~content:[ pcdata "Thanks for your payment, you're in!" ; br () ;
                        br () ;
                        (match confirmation.reply_confirm_booking_count with
                           1 -> pcdata "You have booked one spot total for this activity."
                         | n -> pcdata (sprintf "You have booked %d spots total for this activity." n)) ; br () ;
                        br () ;
                        pcdata "The Zoo is asking for the date of birth of the children who will attend the class, their names, and your mobile number - could you send me the information so that I can update the tickets?" ; br () ;
                      ]
             ()
         in
         return `None)

let ask_how_many_spots context message =
  lwt content = context.get_message_content ~message in
  let confirmation = Yojson_reply_confirm_booking.from_string content in
  lwt _ =
    context.reply_to
      ~message
      ~content:[
        (match confirmation.reply_confirm_booking_count with
           1 -> pcdata "You have already booked one spot for this activity."
         | n -> pcdata (sprintf "You have already booked %d spots for this activity." n)) ; br () ;
        br () ;
        pcdata "How many additional spots do you need?" ; br () ;
      ]
      ()
  in
  return `None

(* the join requests **********************************************************)

let process_join_request context () =
  return `None

let forward_to_supervisor context message =
  lwt _ =
    context.forward_to_supervisor
      ~message
      ~subject:"New join request"
      ~content:[ pcdata "not sure what to do here" ]
      ()
  in
  return `None

(* the playbook ***************************************************************)

PLAYBOOK

#import core_remind

*process_join_request ~> `Message of email ~> forward_to_supervisor

*init__<forward> ~> `Message of email ~> extract_pickup_point

new_member__ ~> `AgreesWithPickupPoint of email ~> mark_agrees_with_pickup_point
new_member__ ~> `AskMemberForPreferredPickupPoint of int ~> ask_member_for_preferred_pickup_point

*count_participants

*validate_pricing<forward> ~> `Message of email ~> ask_members_for_their_opinion_on_pricing ~> `PricingTooMuch of email ~> pricing_too_much
                                                   ask_members_for_their_opinion_on_pricing ~> `PricingOk of email ~> pricing_ok

*suggest_activity<forward> ~> `Message of email ~> suggest_activity_to_members ~> `DeclineActivityAndStay of email ~> decline_activity_and_stay
                                                   suggest_activity_to_members ~> `DeclineActivityAndLeave of email ~> decline_activity_and_leave
                                                   suggest_activity_to_members ~> `RemindInOneWeek of email ~> remind_in_one_week ~> `RemindActivity of int ~> remind_activity
                                                   suggest_activity_to_members ~> `JoinActivity of email ~> join_activity<forward> ~> `Message of email ~> ask_payment

ask_payment ~> `PaymentSuccess of (int * int * int) ~> payment_success<forward> ~> `Message of email ~> send_confirmation
                                                       payment_success ~> `GetMoreSpots of email ~> ask_how_many_spots
ask_payment ~> `PaymentFailure of int ~> payment_failure

*store_profile<forward> ~> `Message of email ~> decode_and_store_profile

PROPERTIES
  - "Your duties", "None"
