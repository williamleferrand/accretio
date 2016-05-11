(*
 * children_schoolbus_planner
 *
 * this playbook plans the trips
 *
 *
 * william@accret.io
 *
 *)

(* this entier playbook should be rewritten at some point, there are millions of
   hardcoded things here *)

open Lwt

open Printf
open CalendarLib

open Api

open Eliom_content.Html5
open Eliom_content.Html5.D

open Message_parsers
open Toolbox

open Ys_uid
open Ys_googlemaps_types

open Children_schoolbus_types
open Children_schoolbus_tools

let author = "william@accret.io"
let name = "Children schoolbus planner"
let description = "this playbook plans the trips"
let version = 0
let tags = ""

(* this playbook expects to be connected to children_schoolbus_transportation
    and children_schoolbus_groups *)

(* some keys ******************************************************************)

(* some tags ******************************************************************)

(* the stages *****************************************************************)

(* the playbook ***************************************************************)

let validate_transporation context () =
  context.log_info "validate tranportation" ;
  match_lwt context.search_societies ~query:"tran*" () with
  | [] -> return `NoTransporationProvider
  | _ as providers ->
    let quote_request =
      {
        reference = "testquote" ;
        route = { legs = [] } ;
        comment = "this is a testing quote request"
      }
    in
    lwt _ =
      Lwt_list.iter_s
        (fun society ->
           context.log_info "messaging society %d" society ;
           lwt _ =
             context.message_society
               ~society
               ~stage:"desk" (* how am I supposed to know that? *)
               ~subject:"quote request"
               ~content:(Yojson_quote_request.to_string quote_request)
               ()
           in
           return_unit)
        providers
    in
    return `None

let no_transportation_provider context () =
  lwt _ =
    context.message_supervisor
      ~subject:"No transporation provider"
      ~content:[
        pcdata "Greetings," ; br () ;
        br () ;
        pcdata "This society isn't connected to a transporation provider"
      ]
      ()
  in
  return `None

let extract_quote context message =
  lwt content = context.get_message_content ~message in
  try
    let quotes = Yojson_quotes.from_string content in
    lwt groups = context.search_societies ~query:"preschool*" () in
    context.log_info "sending the quotes to %d groups" (List.length groups) ;
    lwt _ =
      Lwt_list.iter_s
        (fun society ->
           lwt _ =
             context.message_society
               ~society
               ~stage:"validate_pricing"
               ~subject:"Validate pricing"
               ~content:(Yojson_quotes.to_string quotes)
               ()
           in
           return_unit)
        groups
    in
    return `None
  with _ ->
    context.log_error "couldn't decode message %d" message ;
    lwt _ =
      context.forward_to_supervisor
        ~message
        ~subject:"Invalid reply from the transportation code"
        ~content:[ pcdata "Couldn't understand this message" ]
        ()
    in
    return `None



(* more generic suggestion for the zoo trip, youngest children *)

let generic_suggest context () =
  let activity_older_kids = {
    activity_uid = 1 ;
    activity_reference = sprintf "%04d%04d%04d" 2016 5 27 ;
    activity_min_age_in_months = 36 ;
    activity_max_age_in_months = 60 ;
    activity_date = { year = 2016 ; month = 5 ; day = 27 } ;
    activity_title = "Field trip proposal - SF Zoo on 5/27 - $80 all included" ;
    activity_description = "I'm making some progress! I was able to secure 8 spots for the SF Zoo Animal Adventure Class on 5/27. We would go together to the Zoo, attend the class and spend some time wandering among the animals, before heading back home." ;
    activity_summary = "SF zoo on 5/27" ;
    activity_steps = [] ;
    activity_status =
      Confirmed {
        activity_number_of_spots = 8 ;
        activity_price_per_spot = 80 ;
        activity_price_description = "The cost is $80. It includes the class fee, the Zoo admission and the transportation for 1 child and 1 parent, as well as one lunchbox per child." ;
        activity_price_remark = "This trip is done 'at cost' so that our children can benefit from the SF Zoo's amazing class. If you decide to join and once I receive your payment I will send your contact info to the SF Zoo so that they put your name on the reservation for the class (it is already paid for)." ;
      } ;
    activity_attachments = [] ;
    activity_bookings = []
  }
  in
  let activity_younger_kids = {
    activity_uid = 1 ;
    activity_reference = sprintf "2to3on%04d%04d%04d" 2016 5 27 ;
    activity_min_age_in_months = 18 ;
    activity_max_age_in_months = 30 ;
    activity_date = { year = 2016 ; month = 5 ; day = 27 } ;
    activity_title = "Field trip proposal - SF Zoo on 5/27 - $80 all included" ;
    activity_description = "I'm making some progress! I was able to secure 8 spots for the SF Zoo Animal Adventure Class on 5/27. We would go together to the Zoo, attend the class and spend some time wandering among the animals, before heading back home." ;
    activity_summary = "SF zoo on 5/27" ;
    activity_steps = [] ;
    activity_status =
      Confirmed {
        activity_number_of_spots = 8 ;
        activity_price_per_spot = 80 ;
        activity_price_description = "The cost is $80. It includes the class fee, the Zoo admission and the transportation for 1 child and 1 parent, as well as one lunchbox per child." ;
        activity_price_remark = "This trip is done 'at cost' so that our children can benefit from the SF Zoo's amazing class. If you decide to join and once I receive your payment I will send your contact info to the SF Zoo so that they put your name on the reservation for the class (it is already paid for)." ;
      } ;
    activity_attachments = [] ;
    activity_bookings = []
  }
  in
  lwt groups = context.search_societies ~query:"group" () in
  lwt _ =
    Lwt_list.iter_s
      (fun society ->
         lwt _ =
           context.message_society
             ~society
             ~stage:"suggest_activity"
             ~subject:""
             ~content:(Yojson_activity.to_string activity_older_kids)
             ()
         in
         lwt _ =
           context.message_society
             ~society
             ~stage:"suggest_activity"
             ~subject:""
             ~content:(Yojson_activity.to_string activity_younger_kids)
             ()
         in
         return_unit)
      groups
  in
  return `None

(* the activity scheduler - still pretty hardcoded ********************************)

let suggest_earlier_zoo_trip context () =
  context.log_info "suggesting an earlier trip" ;
  let steps pickup_time dropoff_time =
    [
      {
        step_time = TimeRange ({ hour = 8 ; minute = 00 }, pickup_time) ;
        step_description = "Children and parents arrive and meet at the playground" ;
      } ;
      {
        step_time = Time pickup_time ;
        step_description = "Children and parents board the passenger van" ;
      } ;
      {
        step_time = TimeRange (pickup_time, { hour = 8 ; minute = 45 }) ;
        step_description = "We all cruise to the SF Zoo (singing 'the wheels on the bus' ..)" ;
      } ;
      {
        step_time = TimeRange ({ hour = 9 ; minute = 0 }, { hour = 10 ; minute =30 }) ;
        step_description = "'Little learners' class, focused on 'Cuddly Koalas'. Little Learners is designed for young learners and their caregivers. Each class focuses on a different animal and includes a craft, small snack, an education animal visitor and an experience full of exciting discoveries." ;
      } ;
      {
        step_time = TimeRange ({ hour = 10 ; minute = 30 }, { hour = 12 ; minute = 30 }) ;
        step_description = "Free time in the Zoo, playtime in the Zoo playground and lunch"
      } ;
      {
        step_time = TimeRange ({ hour = 12 ; minute = 30 }, dropoff_time) ;
        step_description = "Heading back home in the van" ;
      }
    ] in

  let activity activity_steps = {
    activity_uid = 1 ;
    activity_reference = sprintf "%04d%04d%04d" 2016 5 27 ;
    activity_min_age_in_months = 18 ;
    activity_max_age_in_months = 42 ;
    activity_date = { year = 2016 ; month = 5 ; day = 27 } ;
    activity_title = "Field trip proposal - SF Zoo on 5/27 - 'Cuddly Koalas' - $80 all included" ;
    activity_description = "I'm making some progress! I was able to secure 8 spots for the SF Zoo Animal Adventure Class on 5/27. The class itself is targetting children 3 to 4, but there is another class earlier at 9:00am for younger kids. If there is enough interest for the earlier class, I could do an additional trip with the rented van and bring a group to the Zoo in time for the early class." ;
    activity_summary = "SF zoo on 5/27" ;
    activity_steps ;
    activity_status =
      Suggestion {
        activity_suggestion = "The cost would be $80. It includes the class fee, the Zoo admission and the transportation for 1 child and 1 parent, as well as one lunchbox per child." ;
      } ;
    activity_attachments = [] ;
    activity_bookings = []
  }
  in

  let activity_financial_district = activity (steps { hour = 8 ; minute = 0 } { hour = 13 ; minute = 15 }) in
  let activity_nob_hill = activity (steps { hour = 8 ; minute = 10 } { hour = 13 ; minute = 25 }) in
  let activity_north_beach = activity (steps { hour = 8 ; minute = 20 } { hour = 13 ; minute = 35 }) in

  lwt north_beach = context.search_societies ~query:"Preschoolbus North Beach" () in
  lwt nob_hill = context.search_societies ~query:"Preschoolbus Nob Hill" () in
  lwt financial_district = context.search_societies ~query:"Preschoolbus Financial District" () in

  match north_beach, nob_hill, financial_district with
    north_beach::_, nob_hill::_, financial_district::_ ->
    lwt _ =
      Lwt_list.iter_s
        (fun (society, activity) ->
           lwt _ =
             context.message_society
               ~society
               ~stage:"suggest_activity"
               ~subject:""
               ~content:(Yojson_activity.to_string activity)
               ()
           in
           return_unit)
        [
          financial_district, activity_financial_district ;
          nob_hill, activity_nob_hill ;
          north_beach, activity_north_beach ;
        ]
    in
    return `None
  | _ ->
    lwt _ =
      context.message_supervisor
        ~subject:"couldn't ask people"
        ~content:[ pcdata "I wasn't able to locate children societies North Beach, Nob Hill & Financial District" ]
        ()
    in
    return `None

let schedule_zoo_trip context message =
  lwt activity_attachments = $message(message)->attachments in
  let activity_attachments =
    List.map
      (fun attachment ->
         {
           filename = attachment.Object_message.filename ;
           content_type = attachment.Object_message.content_type ;
           content = attachment.Object_message.content ;
         })
      activity_attachments
  in

  context.log_info "schedule zoo trip" ;
  (* let's hardcode the zoo trip here first *)
  (* the planner has to take into account all different subgroups, but for now
     we'll do it manually *)
  let steps pickup_time dropoff_time =
    [
      {
        step_time = TimeRange ({ hour = 8 ; minute = 30 }, pickup_time) ;
        step_description = "Children and parents arrive and meet at the playground" ;
      } ;
      {
        step_time = Time pickup_time ;
        step_description = "Children and parents board the passenger van" ;
      } ;
      {
        step_time = TimeRange (pickup_time, { hour = 10 ; minute = 0 }) ;
        step_description = "We all cruise to the SF Zoo (singing 'the wheels on the bus' ..)" ;
      } ;
      {
        step_time = TimeRange ({ hour = 10 ; minute = 0 }, { hour = 11 ; minute = 0 }) ;
        step_description = "Quick snack and free time in the Zoo, at our own pace" ;
      } ;
      {
        step_time = TimeRange ({ hour = 11 ; minute = 0 }, { hour = 12 ; minute = 30 }) ;
        step_description = "Animal Adventure Class focused on Lemurs. (Filled with music, crafts, games, and an education animal visitor, Animal Adventures is designed for 3 and 4-year-olds and their caregivers.  Each class focuses on a specific letter and corresponding animal. Animal Adventures also utilizes other preschool concepts such as colors, patterns and sizes to highlight the animals)"
      } ;
      {
        step_time = TimeRange ({ hour = 12 ; minute = 30 }, { hour = 13 ; minute = 30 }) ;
        step_description = "Lunch time (children lunches are provided, details to come)" ;
      } ;
      {
        step_time = TimeRange ({ hour = 13 ; minute = 30 }, dropoff_time) ;
        step_description = "Heading back to the playground" ;
      }
    ] in

  let activity activity_steps = {
    activity_uid = 1 ;
    activity_reference = sprintf "%04d%04d%04d" 2016 5 27 ;
    activity_min_age_in_months = 30 ;
    activity_max_age_in_months = 60 ;
    activity_date = { year = 2016 ; month = 5 ; day = 27 } ;
    activity_title = "First field trip, SF Zoo on 5/27 - Lemur class! $80 all included, please RSVP" ;
    activity_summary = "zoo on 5/27" ;
    activity_description = "I have very exciting news! I was able to secure 8 spots for the SF Zoo Animal Adventure Class on 5/27! Regarding transporation, bus chartering was a bit on the expensive side so for this first trip I will rent a large passenger van and drive people around to keep cost under control." ;
    activity_steps ;
    activity_status =
      Confirmed {
        activity_number_of_spots = 8 ;
        activity_price_per_spot = 80 ;
        activity_price_description = "The cost is $80. It includes the class fee, the Zoo admission and the transportation for 1 child and 1 parent, as well as one lunchbox per child." ;
        activity_price_remark = "This trip is done 'at cost' so that our children can benefit from the SF Zoo's amazing class. If you decide to join and once I receive your payment I will send your contact info to the SF Zoo so that they put your name on the reservation for the class (it is already paid for, receipt is attached)." ;
      } ;
    activity_attachments ;
    activity_bookings = []
  }
  in

  let activity_financial_district = activity (steps { hour = 8 ; minute = 45 } { hour = 14 ; minute = 0 }) in
  let activity_nob_hill = activity (steps { hour = 9 ; minute = 0 } { hour = 14 ; minute = 15 }) in
  let activity_north_beach = activity (steps { hour = 9 ; minute = 15 } { hour = 14 ; minute = 30 }) in

  lwt north_beach = context.search_societies ~query:"Preschoolbus North Beach" () in
  lwt nob_hill = context.search_societies ~query:"Preschoolbus Nob Hill" () in
  lwt financial_district = context.search_societies ~query:"Preschoolbus Financial District" () in

  match north_beach, nob_hill, financial_district with
    north_beach::_, nob_hill::_, financial_district::_ ->
    lwt _ =
      Lwt_list.iter_s
        (fun (society, activity) ->
           lwt _ =
             context.message_society
               ~society
               ~stage:"suggest_activity"
               ~subject:""
               ~content:(Yojson_activity.to_string activity)
               ()
           in
           return_unit)
        [
          financial_district, activity_financial_district ;
          nob_hill, activity_nob_hill ;
          north_beach, activity_north_beach ;
        ]
    in
    return `None
  | _ ->
    lwt _ =
      context.reply_to
        ~message
        ~content:[ pcdata "I wasn't able to locate children societies North Beach, Nob Hill & Financial District" ]
        ()
    in
    return `None

let plan_activity context () =
  lwt _ =
    context.message_supervisor
      ~subject:"The activity"
      ~content:[
        pcdata "I'm about to trigger the activity. Please attach the files you want to pass around"
      ]
      ()
  in
  return `None

(* pick up an activity to pitch *)

let retrieve_pitch context () =
  return `None

let return_pitch context message =
  let activity = {
    activity_uid = 0 ;
    activity_reference = "pitch" ;
    activity_min_age_in_months = 18 ;
    activity_max_age_in_months = 60 ;
    activity_date = { year = 2016 ; month = 5 ; day = 27 } ;
    activity_title = "first field trip to the Zoo" ;
    activity_description = "" ;
    activity_summary = "" ;
    activity_steps = [] ;
    activity_status =
      Confirmed {
        activity_number_of_spots = 8 ;
        activity_price_per_spot = 80 ;
        activity_price_description = "The cost is $80. It includes the class fee, the Zoo admission and the transportation for 1 child and 1 parent, as well as one lunchbox per child." ;
        activity_price_remark = "This trip is done 'at cost' so that our children can benefit from the SF Zoo's amazing class. If you decide to join and once I receive your payment I will send your contact info to the SF Zoo so that they put your name on the reservation for the class (it is already paid for, receipt is attached)." ;
      } ;
    activity_attachments = [] ;
    activity_bookings = [] ;
  }
  in
  lwt _ =
    context.reply_to
      ~message
      ~content:[ Unsafe.data (Yojson_activity.to_string activity) ]
      ()
  in
  return `None

(* locking slots **************************************************************)

let request_lock_spot context () =
  return `None


let is_booking_final_and_mark_non_final_payments booking =
  match_lwt $booking(booking)->status with
    Object_booking.Pending -> return_false
  | Object_booking.Confirmed payment ->
    match_lwt $payment(payment)->state with
      Object_payment.Paid -> return_true
    | Object_payment.Pending ->
      lwt _ = $payment(payment)<-state = Object_payment.Failed "invalidated" in
      return_false
    | Object_payment.Failed _ -> return_false

let remove_all_non_final_bookings_from_member member activity =
  lwt _ =
    $activity(activity)<-bookings %%% (fun bookings ->
        Lwt_list.filter_s
          (fun (`Booking, uid) ->
             lwt owner = $booking(uid)->member in
             if owner <> member then
               return_true
             else
               is_booking_final_and_mark_non_final_payments uid)
          bookings)
  in
  return_unit

(* we have the executor lock around this call, we're good *)
let lock_spot context message =
  lwt content = context.get_message_content ~message in
  let request = Yojson_request_lock_spots.from_string content in

  let now = Ys_time.now () in

  lwt _ =
    remove_all_non_final_bookings_from_member
      request.request_lock_spots_member
      request.request_lock_spots_activity_uid
  in

  lwt number_of_spots, bookings = $activity(request.request_lock_spots_activity_uid)->(number_of_spots, bookings) in

  lwt number_of_spots =
    Lwt_list.fold_left_s
      (fun acc (`Booking, uid) ->
         match_lwt $booking(uid)->(status, count) with
           | Object_booking.Confirmed _, count -> return (acc - count)
           | Object_booking.Pending, count -> return (acc - count) (* todo, expire there *))
      number_of_spots
      bookings
  in

  if number_of_spots < request.request_lock_spots_count then
    lwt _ =
      context.reply_to
        ~message
        ~content:[ Unsafe.data (Yojson_reply_lock_spots.to_string (EventFull number_of_spots)) ]
        ()
    in
    return `None
  else
    begin

      lwt booking =
        match_lwt Object_booking.Store.create
                    ~member:request.request_lock_spots_member
                    ~count:request.request_lock_spots_count
                    ~cost:32.0 (* TODO: change that asap *)
                    ~status:Object_booking.Pending
                    ()
        with
        | `Object_created booking -> return booking.Object_booking.uid
      in

      lwt _ = $activity(request.request_lock_spots_activity_uid)<-bookings += (`Booking, booking) in

      lwt _ =
        context.reply_to
        ~message
        ~content:[
          Unsafe.data
            (Yojson_reply_lock_spots.to_string
               (EventLock {
                   lock_spots_activity_uid = request.request_lock_spots_activity_uid ;
                   lock_spots_count = request.request_lock_spots_count ;
                   lock_attachments = [] ;
                   lock_until = now ;
                 }))
        ]
        ()
    in
    return `None
    end

(* the curriculum *************************************************************)
(* lots and lots of todo here .. *)

let persist context activity =
  lwt uid =
    match_lwt Object_activity.Store.find_by_reference activity.activity_reference with
    | Some uid ->
      lwt _ = $activity(uid)<-society = context.society in
      return uid
    | None ->
      match_lwt
        Object_activity.Store.create
          ~society:context.society
          ~reference:activity.activity_reference
          ~min_age_in_months:activity.activity_min_age_in_months
          ~max_age_in_months:activity.activity_max_age_in_months
          ~title:activity.activity_title
          ~description:activity.activity_description
          ~summary:activity.activity_summary
          ~number_of_spots:8
          () with
      | `Object_already_exists (_, uid) -> return uid
      | `Object_created activity -> return activity.Object_activity.uid
  in
  (* WARN: this is super dangerous at it will erase other edges with
    the same target uid but different label (to be fixed one day in pa_vertex *)
  lwt _ = $society(context.society)<-objects +=! (`Activity, uid) in
  return_unit


 let zoo_5_27_16_3_4 =
   {
     activity_uid = 1 ;
     activity_reference = sprintf "%04d%04d%04d" 2016 5 27 ;
     activity_min_age_in_months = 30 ;
     activity_max_age_in_months = 60 ;
     activity_date = { year = 2016 ; month = 5 ; day = 27 } ;
     activity_title = "First field trip, SF Zoo on 5/27 - Lemur class! $80 all included, please RSVP" ;
     activity_summary = "zoo on 5/27" ;
     activity_description = "I have very exciting news! I was able to secure 8 spots for the SF Zoo Animal Adventure Class on 5/27! Regarding transporation, bus chartering was a bit on the expensive side so for this first trip I will rent a large passenger van and drive people around to keep cost under control." ;
     activity_steps = [] ;
     activity_status =
      Confirmed {
        activity_number_of_spots = 8 ;
        activity_price_per_spot = 80 ;
        activity_price_description = "The cost is $80. It includes the class fee, the Zoo admission and the transportation for 1 child and 1 parent, as well as one lunchbox per child." ;
        activity_price_remark = "This trip is done 'at cost' so that our children can benefit from the SF Zoo's amazing class. If you decide to join and once I receive your payment I will send your contact info to the SF Zoo so that they put your name on the reservation for the class (it is already paid for, receipt is attached)." ;
      } ;
    activity_attachments = [] ;
    activity_bookings = []
   }

 let zoo_5_27_16_2_3 =
   {
     activity_uid = 2 ;
     activity_reference = sprintf "2to3on%04d%04d%04d" 2016 5 27 ;
     activity_min_age_in_months = 30 ;
     activity_max_age_in_months = 60 ;
     activity_date = { year = 2016 ; month = 5 ; day = 27 } ;
     activity_title = "First field trip, SF Zoo on 5/27 - Lemur class! $80 all included, please RSVP" ;
     activity_summary = "zoo on 5/27" ;
     activity_description = "I have very exciting news! I was able to secure 8 spots for the SF Zoo Animal Adventure Class on 5/27! Regarding transporation, bus chartering was a bit on the expensive side so for this first trip I will rent a large passenger van and drive people around to keep cost under control." ;
     activity_steps = [] ;
     activity_status =
      Confirmed {
        activity_number_of_spots = 8 ;
        activity_price_per_spot = 80 ;
        activity_price_description = "The cost is $80. It includes the class fee, the Zoo admission and the transportation for 1 child and 1 parent, as well as one lunchbox per child." ;
        activity_price_remark = "This trip is done 'at cost' so that our children can benefit from the SF Zoo's amazing class. If you decide to join and once I receive your payment I will send your contact info to the SF Zoo so that they put your name on the reservation for the class (it is already paid for, receipt is attached)." ;
      } ;
    activity_attachments = [] ;
    activity_bookings = []
   }

let persist context () =
  lwt _ =
    Lwt_list.iter_s
      (persist context)
      [
         zoo_5_27_16_3_4 ;
         zoo_5_27_16_2_3
      ]
  in
  context.log_info "activity was persisted" ;
  return `None

(* confirm the booking ********************************************************)

let request_confirm_booking context message =
  return `None

let confirm_booking context message =
  lwt content = context.get_message_content ~message in

  let request_confirm_booking =
    Yojson_request_confirm_booking.from_string
      content in

  context.log_info "confirming booking for activity %d"
    request_confirm_booking.request_confirm_booking_activity ;

  lwt member = $payment(request_confirm_booking.request_confirm_booking_payment)->member in
  lwt amount = $payment(request_confirm_booking.request_confirm_booking_payment)->amount in

  let cost = 32.0 in

  (* let's check that the payment is valid *)

  match_lwt $payment(request_confirm_booking.request_confirm_booking_payment)->state with
    Object_payment.Failed _
  | Object_payment.Pending ->
    lwt _ =
      context.forward_to_supervisor
        ~message
        ~subject:"Payment is invalid"
        ~content:[ pcdata "The payment isn't successful. Please step in" ]
        ()
    in
    return `None
  | Object_payment.Paid ->

    (* let's run the calculations inside the booking lock *)

    lwt _ =
      $activity(request_confirm_booking.request_confirm_booking_activity)<-bookings %%% (fun bookings ->

        (* first we check if the payment has already been stored  *)

        lwt already_exists =
          Lwt_list.exists_s
            (fun (`Booking, uid) ->
               match_lwt $booking(uid)->status with
                 | Object_booking.Confirmed payment when payment = request_confirm_booking.request_confirm_booking_payment -> return_true
                 | _ -> return_false)
             bookings
        in

        match already_exists with
          true ->
          context.log_info
            "payment %d has already been stored in activity %d"
            request_confirm_booking.request_confirm_booking_payment
            request_confirm_booking.request_confirm_booking_activity ;
          return bookings
        | false ->

          (* now we need to look for in-flight, non confirmed bookings for this member *)

          lwt pending =
            Lwt_list.filter_s
              (fun (`Booking, uid) ->
                 lwt owner, status = $booking(uid)->(member, status) in
                  if owner <> member then
                    return_false
                  else
                    match status with
                      Object_booking.Confirmed _ -> return_false
                    | Object_booking.Pending -> return_true)
              bookings in

          (* mark the bookings as paid as we go *)

          lwt credit =
            Lwt_list.fold_left_s
              (fun credit (`Booking, uid) ->
                 lwt count = $booking(uid)->cost in
                 if credit < cost then
                   return credit
                 else
                   lwt _ =
                     $booking(uid)<-status = Object_booking.Confirmed request_confirm_booking.request_confirm_booking_payment
                   in
                   return (credit -. count))
              amount
              bookings
          in

          (* and we finally create new bookings if the credit is still positive and there are enough spots *)
          (* TODO: deal with the case where the event is filling up *)

          if credit > cost then
            begin
              lwt booking =
                match_lwt Object_booking.Store.create
                            ~member
                            ~count:(int_of_float (credit /. cost))
                            ~cost
                            ~status:(Object_booking.Confirmed request_confirm_booking.request_confirm_booking_payment)
                            ()
                with
                | `Object_created booking -> return booking.Object_booking.uid
              in
              return ((`Booking, booking) :: bookings)
            end
          else
            return bookings)
  in

  (* let's remove all bookings from that user that haven't been fulfilled *)

  lwt _ =
    remove_all_non_final_bookings_from_member
      member
      request_confirm_booking.request_confirm_booking_activity
  in

  (* now we have to make a reply to the user *)

  lwt activity = to_activity_json request_confirm_booking.request_confirm_booking_activity in
  lwt bookings = $activity(request_confirm_booking.request_confirm_booking_activity)->bookings in

  lwt count =
     Lwt_list.fold_left_s
       (fun acc (`Booking, uid) ->
          lwt owner, count, status = $booking(uid)->(member, count, status) in
          if (owner = member) then
            begin
              match status with
                Object_booking.Confirmed payment ->
                begin
                  match_lwt $payment(payment)->state with
                    Object_payment.Paid -> return (acc + count)
                  | _ -> return acc
                end
              | _ -> return acc
            end
          else
            return acc)
   0
   bookings
   in

   let confirmation =
      {
        reply_confirm_booking_activity = activity ;
        reply_confirm_booking_count = count ;
      }
    in

    lwt _ =
      context.reply_to
        ~message
        ~content:[ Unsafe.data (Yojson_reply_confirm_booking.to_string confirmation) ]
        ()
    in
    return `None


(* the plumbing ***************************************************************)

PLAYBOOK

#import core_remind

*validate_transporation ~> `NoTransporationProvider ~> no_transportation_provider
 validate_transporation ~> `Message of email ~> extract_quote

*plan_activity<forward> ~> `Message of email ~> schedule_zoo_trip

*suggest_earlier_zoo_trip

*retrieve_pitch<forward> ~> `Message of email ~> return_pitch

*request_lock_spot<forward> ~> `Message of email ~> lock_spot

*request_confirm_booking<forward> ~> `Message of email ~> confirm_booking

*persist
*generic_suggest

PROPERTIES
  - "Your duties", "None"
