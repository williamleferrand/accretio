(*
 * bakery
 *
 * this playbook organizes a small amateur bakery
 *
 * the bakers are the members tagged with `baker`
 *
 * on Wednesdays, the playbook will get in touch with the bakers to find someone
 * available for the coming weekend.
 *
 * once such member has been located, the playbook will proceed with asking the
 * members tagged with `customer` for their orders
 *
 * after getting confirmation from the baker, the playbook will emit payment
 * requests through stripe
 *
 * the playbook will compile the orders on Fridays and send them to the bakers,
 * along with an optimized delivery route
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
let name = "Bakery"
let description = "This playbook organizes a small amateur bakery"
let version = 0
let tags = ""

(* some keys and some tags ****************************************************)

let key_run = "run"
let key_order_size = sprintf "order-size-%d-%d"
let key_address = sprintf "address-%d"
let key_original_message = "original-message"

let tag_baker = "baker"
let tag_customer = "customer"
let tag_not_available = sprintf "notavailable%d"
let tag_available = sprintf "available%d"
let tag_already_asked = sprintf "alreadyasked%d"
let tag_orders = sprintf "orders%d"

let label_baker_reminder = sprintf "bakerreminder%d%d"

(* some helpers ***************************************************************)

let current_run () =
  let now = Calendar.now () in
  Calendar.year now * 53 + Calendar.week now

(* the stages *****************************************************************)

(* this stage is called when the society is created. its purpose is to send
   a welcome message to the society supervisor and mark him/her with the baker
   tag (this tag can always be removed later) *)
let init__ context () =
  lwt salutations = salutations context.society_supervisor in
  lwt _ = context.tag_member ~member:context.society_supervisor ~tags:[ tag_baker ] in
  lwt _ =
    context.message_member
      ~member:context.society_supervisor
      ~subject:"Welcome!"
      ~content:[
        salutations ; br () ;
        br () ;
        pcdata "Welcome to your new Bakery!" ; br () ;
        br () ;
        pcdata "If you wish, you can visit the administration interface to invite additional bakers or start registering your customers using the following link:" ;br () ;
        br () ;
        Raw.a ~a:[ a_href (uri_of_string (fun () -> context.direct_link)) ] [ pcdata context.direct_link ] ; br () ;
        br () ;
        pcdata "Otherwise, I'll start scheduling a bread delivery round for this coming Sunday." ; br () ;
      ]
      ()
  in
  return `None

(* this stage is called periodically to schedule a baking session *)
let schedule_baking_session context () =
  let run = current_run () in
  return (`LookForBaker run)

(* this stage checks if a baker is available this weekend. it goes through the
   list of all available bakers until it finds one; if it doesn't it will
   message the supervisor *)
let look_for_baker context run =
  context.log_info "checking which bakers are available for run %d" run ;
  match_lwt context.search_members ~query:(sprintf "%s -%s" tag_baker (tag_not_available run)) () with
    [] -> return `NoBakerAvailable
  | baker :: _ -> return (`AskIfBakerIsAvailable (run, baker))

(* this stage messages the supervisor to let him/her know that no baker is
   available *)
let no_baker_available context () =
  lwt salutations = salutations context.society_supervisor in
  lwt _ =
    context.message_supervisor
      ~subject:"No bakers are available this week"
      ~content:[
        salutations ; br () ;
        br () ;
        pcdata "Unfortunaly, I wasn't able to locate an available baker this week." ; br () ;
        br () ;
        pcdata "See you next week!" ; br () ;
      ]
      ()
  in
  return `None

let ask_if_baker_is_available context (run, member) =
  lwt salutations = salutations member in
  lwt _ =
    context.message_member
      ~remind_after:(Calendar.Period.lmake ~hour:16 ())
      ~data:[ key_run, string_of_int run ]
      ~member
      ~subject:"Baking bread next weekend"
      ~content:[
        salutations ; br () ;
        br () ;
        pcdata "Are you available to bake and deliver bread next Sunday?" ; br () ;
      ]
      ()
  in
  lwt _ =
    context.set_timer
      ~label:(label_baker_reminder run member)
      ~duration:(Calendar.Period.lmake ~hour:24 ())
      (`BakerNeverReplied (run, member))
  in
  return `None

(* this is a small helper that extracts the run id from the meta data attached
   to the message *)
let with_run context message f =
  let run = current_run () in
  match_lwt context.get_message_data ~message ~key:key_run with
  | Some run' when int_of_string run' = run ->
    begin
      lwt member = context.get_message_sender ~message in
      lwt _ = context.cancel_timers ~query:(label_baker_reminder run member) in
      f run message
    end
  | _ ->
    lwt _ =
      context.forward_to_supervisor
        ~message
        ~subject:"Run id issue"
        ~content:[ pcdata "I couldn't find a current run id for the following message" ]
        ()
    in
    return `None

(* we call this stage if a baker doesn't reply within 24 hours *)
let baker_never_replied context (run, member) =
  lwt _ = context.cancel_timers ~query:(label_baker_reminder run member) in
  match current_run () = run with
    false -> (* that must have been an old message *) return `None
  | true -> return (`LookForBaker run)

(* this stage marks the baker as available and moves forward *)
let baker_is_available context message =
  with_run context message
    (fun run message ->
      lwt member = context.get_message_sender ~message in
      lwt _ = context.tag_member ~member ~tags:[ tag_available run ] in
      lwt _ =
        context.reply_to
          ~message
          ~content:[ pcdata "Great, let me get a sense of the demand. I'll be in touch soon" ]
          ()
      in
      return (`AskCustomersForOrders run))

(* this stage marks the baker as not available and looks for someone else *)
let baker_is_not_available context message =
  with_run context message
    (fun run message ->
      lwt member = context.get_message_sender ~message in
      lwt _ = context.tag_member ~member ~tags:[ tag_not_available run ] in
      lwt _ =
        context.reply_to
          ~message
          ~content:[ pcdata "No worries, I'll find someone else!" ]
          ()
      in
      return (`LookForBaker run))


(* this stage gets in touch with all the customers that haven't been contacted
   yet for this run and take their orders *)
let ask_customers_for_orders context run =
  lwt customers = context.search_members ~query:(sprintf "%s -%s" tag_customer (tag_already_asked run)) () in
  lwt _ =
    Lwt_list.iter_s
      (fun member ->
         lwt salutations = salutations member in
         lwt _ =
           context.message_member
             ~member
             ~remind_after:(Calendar.Period.lmake ~hour:8 ())
             ~data:[ key_run, string_of_int run ]
             ~subject:"Bread delivery"
             ~content:[
               salutations ; br () ;
               br () ;
               pcdata "Would you be interested in some bread this coming Sunday?" ; br ()


             ]
             ()
         in
         lwt _ = context.tag_member ~member ~tags:[ tag_already_asked run ] in
         return_unit)
      customers
  in
  return `None

(* this is an helper stage that can be triggered manually if needed to ask new
   members for orders in the current run *)

let ask_all_customers_for_current_run context () =
  (* let's check that we have a baker .. *)
  let run = current_run () in
  match_lwt context.search_members ~query:(sprintf "%s %s" tag_baker (tag_available run)) () with
    [] ->
    context.log_error "you can't ask for orders if you don't have a baker available" ;
    lwt _ =
      context.message_supervisor
        ~subject:"Unable to ask customers for orders"
        ~content:[
          pcdata "I can't ask customers for orders if no baker is available"
        ]
        ()
    in
    return `None
  | _ -> return (`AskCustomersForOrders run)

let customer_doesnt_order context message =
  lwt _ =
    context.reply_to
      ~message
      ~content:[ pcdata "No problem, have a great end of week!" ]
      ()
  in
  return `None

let customer_order count context message =
  with_run context message
    (fun run message ->
       lwt member = context.get_message_sender ~message in
       lwt _ = context.tag_member ~member ~tags:[ tag_orders run ] in
       lwt _ = context.set ~key:(key_order_size run member) ~value:(string_of_int count) in
       match_lwt context.get ~key:(key_address member) with
         Some _ -> return (`ThankMemberForOrder message)
       | None -> return (`ThankMemberAndAskForAddress message))

let customer_order1 = customer_order 1
let customer_order2 = customer_order 2
let customer_order3 = customer_order 3

let thank_member_for_order context message =
  lwt _ =
    context.reply_to
      ~message
      ~content:[ pcdata "Thanks, I will get back in touch soon with the estimated delivery hour. Looking forward to it!" ]
      ()
  in
  return `None

let thank_member_and_ask_for_address context message =
  lwt _ =
    context.reply_to
      ~message
      ~remind_after:(Calendar.Period.lmake ~hour:6 ())
      ~content:[ pcdata "Thanks, and where should we deliver?" ]
      ()
  in
  return `None

(* this stage is called once a member replies with an address *)
let store_address context message =
  lwt content = context.get_message_content ~message in
  try_lwt
    (* we try to geocode the address *)
    lwt addresses = Ys_googlemaps.fetch_raw content in
    match addresses with
      [] -> failwith "no addresses found"
    | address :: _  ->
      let address = Ys_googlemaps.extract_formatted_address address in

      lwt message, original =
        match_lwt context.get_message_data ~message ~key:key_original_message with
          Some message -> return (int_of_string message, false)
        | None -> return (message, true) in
      lwt member = context.get_message_sender ~message in

      lwt _ = context.set ~key:(key_address member) ~value:address in
      lwt email = $member(member)->preferred_email in
      lwt _ =
        if original then
          begin
            lwt _ =
              context.forward_to_supervisor
                ~message
                ~data:[ key_original_message, string_of_int message ]
                ~subject:"Verifying address"
                ~content:[
                  pcdata "Member " ; pcdata email ; pcdata " just entered the following address:" ; br () ;
                  br ();
                  pcdata address ; br () ;
                  br () ;
                  pcdata "If it looks good you don't have to do anything; if it doesn't, please reply with a corrected address" ;
                  br ()
                ]
                ()
            in
            lwt _ =
              context.reply_to
                ~message
                ~content:[
                  pcdata "Thanks, I will be back in touch soon with an estimated delivery time. Looking forward to it!"
                ]
                ()
            in
            return_unit
          end
        else return_unit
      in
    return `None
  with _ ->
    (* if anything goes wrong, let's ask the supervisor to sort things out *)
    lwt _ =
      context.forward_to_supervisor
        ~message
        ~data:[ key_original_message, string_of_int message ]
        ~subject:"Could you please extract an address from the message below?"
        ~content:[]
        ()
    in
    return `None

(* the two following stages are currently used to create a dashboard, later
   they should request confirmation / payments *)
let create_dashboard_for_current_run context () =
  let run = current_run () in
  return (`CreateDashboard run)

let create_dashboard context run =
  lwt bakers = context.search_members ~query:(sprintf "%s %s" tag_baker (tag_available run)) () in
  lwt customers = context.search_members ~query:(sprintf "%s %s" tag_customer (tag_orders run)) () in
  lwt salutations = salutations context.society_supervisor in

  let format_members member =
    lwt name, preferred_email = $member(member)->(name, preferred_email) in
    lwt address =
      match_lwt context.get ~key:(key_address member) with
        None -> return (pcdata "no address")
      | Some address -> return (pcdata address)
    in
    return
      (li [
          pcdata preferred_email ; pcdata " (" ; pcdata name ; pcdata ")" ; pcdata " - " ; address
        ])
  in
  lwt bakers = Lwt_list.map_s format_members bakers in
  lwt customers = Lwt_list.map_s format_members customers in

  lwt _ =
    context.message_supervisor
      ~subject:(sprintf "Dashboard for run %d" run)
      ~content:[
        salutations ; br () ;
        br () ;
        pcdata "Here is what I have for the current run:" ; br () ;
        br () ;
        pcdata "Bakers:" ; br () ;
        ul bakers ;
        pcdata "Customers:" ; br () ;
        ul customers ;
      ]
      ()
  in
  return `None

 (* the playbook ***************************************************************)

PLAYBOOK

#import core_remind

*schedule_baking_session ~> `LookForBaker of int ~> look_for_baker ~> `NoBakerAvailable ~> no_baker_available
                                                    look_for_baker ~> `AskIfBakerIsAvailable of (int * int) ~> ask_if_baker_is_available


ask_if_baker_is_available ~> `BakerNeverReplied of int * int ~> baker_never_replied ~> `LookForBaker of int ~> look_for_baker
ask_if_baker_is_available ~> `No of email ~> baker_is_not_available ~> `LookForBaker of int ~> look_for_baker
ask_if_baker_is_available ~> `Yes of email ~> baker_is_available ~> `AskCustomersForOrders of int ~> ask_customers_for_orders

*ask_all_customers_for_current_run ~> `AskCustomersForOrders of int ~> ask_customers_for_orders

ask_customers_for_orders ~> `No of email ~> customer_doesnt_order
ask_customers_for_orders ~> `Yes1 of email ~> customer_order1
ask_customers_for_orders ~> `Yes2 of email ~> customer_order2
ask_customers_for_orders ~> `Yes3 of email ~> customer_order3

customer_order1 ~> `ThankMemberForOrder of int ~> thank_member_for_order
customer_order1 ~> `ThankMemberAndAskForAddress of int ~> thank_member_and_ask_for_address<forward> ~> `Message of email ~> store_address<forward> ~> `Message of email ~> store_address

customer_order2 ~> `ThankMemberForOrder of int ~> thank_member_for_order
customer_order2 ~> `ThankMemberAndAskForAddress of int ~> thank_member_and_ask_for_address

customer_order3 ~> `ThankMemberForOrder of int ~> thank_member_for_order
customer_order3 ~> `ThankMemberAndAskForAddress of int ~> thank_member_and_ask_for_address

*create_dashboard_for_current_run ~> `CreateDashboard of int ~> create_dashboard

*init__

(*
CRON check_if_baker_is_available_this_week "0 0 * * 1 *"
CRON create_dashboard_this_week "0 0 * * 4 *"
*)
