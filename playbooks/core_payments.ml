(*
 * core - payments
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

(* some tags *)

let key_invoice = "core-payment-invoice"
let tag_has_payments_pending = sprintf "corepaymentshaspending%s"
let timer_reminder = sprintf "corepaymentsreminder%d%s"

(* the stages *)

let request_payment context (member, label, amount, evidence_message) =
  context.log_info "requesting payment to member %d" member ;
  lwt evidence = $message(evidence_message)->attachments in
  match_lwt
    context.create_invoice
      ~member
      ~label
      ~evidence
      ~amount
      ~on_success:(`PaymentSuccessful(member)) with
  | None ->
    context.log_error "couldn't issue invoice for %d, panic" member ;
    return (`AlertSupervisor (member, label, amount))
  | Some invoice ->
    let content =
      match evidence with
        [] ->
        [
          pcdata "Hi," ; br () ;
          br ()  ;
          pcdata "Thanks for your participation to " ; pcdata context.society_name ; pcdata ", I hope you enjoyed it!" ; br () ;
          br () ;
          pcdata "Your share ends up being "; pcdata (Printf.sprintf "$%.2f" amount) ; pcdata ", would you mind visiting the secured link below & authorize the payment?" ; br () ;
          br () ;
          pcdata "Payments on Accretio are processed by Stripe. Accretio doesn't add any fees to the transaction but the Stripe fees." ; br () ;
          br () ;
          pcdata "Thanks!" ;
        ]
      | _ ->
        [
          pcdata "Hi," ; br () ;
          br () ;
          pcdata "Thanks for your participation to " ; pcdata context.society_name ; pcdata ", I hope you enjoyed it!" ; br () ;
          br () ;
          pcdata "Your share ends up being "; pcdata (Printf.sprintf "$%.2f" amount) ; pcdata ", would you mind visiting the secured link below & authorize the payment?" ; br () ;
          br () ;
          pcdata "Attached is a scan of the total bill. " ; pcdata "Payments on Accretio are processed by Stripe. Accretio doesn't add any fees to the transaction but the Stripe fees." ; br () ;
          br () ;
          pcdata "Thanks!" ;
        ]
    in
    lwt _ =
      context.message_member
        ~member
        ~attachments:evidence
        ~data:[ key_invoice, invoice ]
        ~subject:label
        ~content
        ()
    in
    lwt _ =
      context.tag_member ~member ~tags:[ tag_has_payments_pending invoice ]
    in
    lwt _ =
      context.set_timer
        ~label:(timer_reminder member invoice)
        ~duration:(Calendar.Period.lmake ~hour:24 ())
        (`RemindMemberOfPayment (member, label, invoice, 0))
    in
    return `None

let alert_supervisor context (member, label, amount) =
  context.log_warning "alerting supervisor for member %d, label %s, amount %f" member label amount ;
  lwt email = $member(member)->preferred_email in
  lwt _ =
    context.message_supervisor
      ~subject:"Couldn't create invoice"
      ~content:[
        pcdata "Hi," ; br () ;
        br () ;
        pcdata "I couldn't create an invoice for " ; pcdata email ; pcdata ", amount is " ; pcdata (Printf.sprintf "$%.2f" amount) ; pcdata "." ; br () ;
        br () ;
        pcdata "The label of the payment was:" ; br () ;
        br () ;
        i [ pcdata label ] ;
        br () ;
      ]
      ()
  in
  return `None

let remind_member_of_payment context (member, label, invoice, attempts) =
  if attempts > 2 then
    begin
      context.log_info "member %d hasn't responded in %d attempts" member attempts ;
      lwt email = $member(member)->preferred_email in
      lwt _ =
        context.message_supervisor
          ~data:[ key_invoice, invoice ]
          ~subject:(Printf.sprintf "Missed payment for %s" label)
          ~content:[
            pcdata "Hi," ; br () ;
            br () ;
            pcdata "Member " ; pcdata email ; pcdata " hasn't responded about invoice " ; pcdata invoice ; pcdata "." ; br () ;
            br () ;
            pcdata "The label of the invoice is:" ; br () ;
            br () ;
            i [ pcdata label ] ; br () ;
            br () ;
            pcdata "You're needed" ; br () ;
          ]
          ()
      in
      return `None
    end
  else
    lwt _ =
      context.message_member
        ~member
        ~data:[ key_invoice, invoice ]
        ~subject:label
        ~content:[
          pcdata "Hi," ; br ();
          br () ;
          pcdata "Sorry for the reminder; would you mind visiting the link below to settle this transaction?" ; br () ;
          br () ;
          pcdata "thelink" ; br () ;
          br () ;
          pcdata "If you have any question, please get in touch!" ; br ()
        ]
        ()
    in
    lwt _ =
      context.set_timer
        ~label:(timer_reminder member invoice)
        ~duration:(Calendar.Period.lmake ~hour:24 ())
        (`RemindMemberOfPayment (member, label, invoice, attempts + 1))
    in
    return `None

let payment_success context (member, invoice) =
  context.log_info "payment success from member %d" member ;
  lwt _ = context.cancel_timers ~query:(timer_reminder member invoice) in
  lwt _ =
    context.message_member
      ~member
      ~subject:"Thanks!"
      ~content:[
        pcdata "Thanks for the payment!"
      ]
      ()
  in
  return `None

(* the plumbing *)

COMPONENT

 request_payment ~> `AlertSupervisor of (int * string * float) ~> alert_supervisor
 request_payment ~> `RemindMemberOfPayment of (int * string * string * int) ~> remind_member_of_payment ~> `RemindMemberOfPayment of (int * string * string * int) ~> remind_member_of_payment
 request_payment ~> `PaymentSuccess of (int * string) ~> payment_success
