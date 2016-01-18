(*
 * Accretio is an API, a sandbox and a runtime for social playbooks
 *
 * Copyright (C) 2015 William Le Ferrand
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)


{shared{

open Lwt
open Ys_uid
open Vault

type stripe = { stripe_customer : string ; stripe_last4 : string }

type bundle =
  {
    payment : View_payment.t ;
    stripe : stripe option ;
  }
}}

{server{

let _ =
  Ys_shortlink.register_checker Object_payment.Store.find_by_shortlink

let retrieve uid =
  lwt payment = View_payment.to_view uid in
  lwt stripe =
    lwt member = $payment(uid)->member in
    match_lwt $member(member)->stripe with (* better not pull the wrong member there! *)
    | None -> return_none
    | Some stripe -> return (Some { stripe_customer = stripe.Object_member.stripe_customer;
                                    stripe_last4 = stripe.Object_member.stripe_last4 })
  in
  return
    (Some
      {
        payment ;
        stripe ;
      })

let retrieve = server_function ~name:"payment-retrieve" Json.t<int> retrieve

open Ys_stripe

let perform (uid, stripe_customer, stripe_last4) =
  (* rely on the field lock to prevent double charges *)

  let callback_success () =
    try_lwt
      lwt society, callback_success = $payment(uid)->(society, callback_success) in
      match callback_success with
        None -> return_unit
      | Some call -> Executor.push_and_execute society call
    with exn -> Lwt_log.ign_error_f ~exn "couldn't call callback success for payment %d" uid ;
      return_unit
  in

  let callback_failure () =
    try_lwt
      lwt society, callback_failure = $payment(uid)->(society, callback_failure) in
      match callback_failure with
        None -> return_unit
      | Some call -> Executor.push_and_execute society call
    with exn -> Lwt_log.ign_error_f ~exn "couldn't call callback failure for payment %d" uid ;
      return_unit
  in

  lwt amount, label, member = $payment(uid)->(amount, label, member) in
  $payment(uid)<-state %%% (function
      | Object_payment.Paid as state ->
        Lwt_log.ign_warning_f "attempting to double perform payment %d" uid ;
        return state
        | _ ->
          match_lwt charges
                      ~customer:stripe_customer
                      ~amount:(int_of_float (amount *. 100.0)) (* stripe amount is in cents *)
                      ~currency:"usd"
                      ~description:label
                      () with
            Error exn ->
            Lwt_log.ign_info_f
              "error while processing paymement for payment %d" uid ;
            lwt _ = callback_failure () in
            return (Object_payment.Failed (Printexc.to_string exn))
          | Ok charge ->
            match charge.ReplyCharges.paid, charge.ReplyCharges.failure_message with
            | false, Some msg ->
              Lwt_log.ign_info_f
                "charge %s was returned unpaid, message is %s, payment is %d"
                charge.ReplyCharges.id
                msg
                uid ;
              lwt _ = callback_failure () in
              return (Object_payment.Failed msg)
            | false, None ->
              Lwt_log.ign_info_f
                "charge %s was returned unpaid, no message, payment is %d"
                charge.ReplyCharges.id
                uid ;
              lwt _ = callback_failure () in
              return (Object_payment.Failed (Printf.sprintf "Rejected by Stripe (%s)" charge.ReplyCharges.id))
            | true, _ ->
              Lwt_log.ign_info_f "payment %d was processed successfully" uid ;
              lwt _ = callback_success () in
              return Object_payment.Paid)

let perform_with_token (uid, token) =
  Lwt_log.ign_info_f "validating token %s, created for payment %d" token uid ;
  lwt member = $payment(uid)->member in
  lwt result =
    match_lwt $member(member)->stripe with
    | None ->
      lwt preferred_email = $member(member)->preferred_email in
      (* we need to create the customer *)
      create_customer ~source:token ~email:preferred_email ()
    | Some stripe ->
      update_customer ~source:token ~customer_id:stripe.Object_member.stripe_customer ()
  in
  lwt state =
    match result with
    | Error exn ->
      Lwt_log.ign_error_f "error when performing payment with token %d, %s: %s" uid token
        (Printexc.to_string exn) ;
      return (Object_payment.Failed (Printexc.to_string exn))
    | Ok customer ->
      match customer.ReplyCreateCustomer.sources.Sources.data with
      | [] ->
        Lwt_log.ign_error_f "customer %s has no sources" customer.ReplyCreateCustomer.id ;
        return (Object_payment.Failed "couldn't create stripe handle")
      | data :: _ ->
        let stripe = Object_member.({
            stripe_customer = customer.ReplyCreateCustomer.id ;
            stripe_last4 = data.Sources.last4 }) in
        $member(member)<-stripe = Some stripe ;
        perform (uid, customer.ReplyCreateCustomer.id, data.Sources.last4) in
  return (Some (View_payment.to_state state))

let perform (uid, stripe_customer, stripe_last4) =
  lwt state = perform (uid, stripe_customer, stripe_last4) in
  return (Some (View_payment.to_state state))

let perform = server_function ~name:"payment-perform" Json.t<int * string * string> perform
let perform_with_token = server_function ~name:"payment-perform-with_token" Json.t<int * string> perform_with_token

}}

{client{

open React
open Ys_react
open Eliom_content.Html5
open Eliom_content.Html5.D
open View_payment

let builder = function
    None -> pcdata ""
  | Some bundle ->


    let view = bundle.payment in

    let payment_amount =
      div ~a:[ a_class [ "payment-amount" ]] [
        (* todo: manage currencies here *)
        pcdata (Printf.sprintf "$%.2f" view.amount) ;
      ]
    in

    let payment_type, update_payment_type =
      S.create
        (match view.state with
         | Paid -> `Paid
         | Failed message -> Help.warning message ; `CreatePaymentMethod None
         | Pending -> match bundle.stripe with
             None -> `CreatePaymentMethod None
           | Some stripe -> `Use stripe) in

    let dispatch =
      function
      | Paid -> update_payment_type `Paid
      | Failed message -> Help.warning message ;  update_payment_type (`CreatePaymentMethod None)
      | Pending -> update_payment_type (`CreatePaymentMethod None)
    in

    let perform (stripe_customer, stripe_last4) =
      detach_rpc %perform (view.uid, stripe_customer, stripe_last4) dispatch
    in
    let perform_with_token token =
      detach_rpc %perform_with_token (view.uid, token) dispatch
    in

    let payment_method =
      S.map
        (function
          | `Paid ->
            div ~a:[ a_class [ "payment-paid" ]] [
              pcdata "PAID"
            ]
          | `Use stripe ->
            let use_current_stripe =
              button
                ~button_type:`Button
                ~a:[ a_onclick (fun _ -> perform (stripe.stripe_customer, stripe.stripe_last4)) ]
                [ pcdata "Pay using card ending in " ; pcdata stripe.stripe_last4 ]
            in
            let use_another_card =
              button
                ~button_type:`Button
                ~a:[ a_onclick (fun _ -> update_payment_type (`CreatePaymentMethod (Some stripe))) ]
                [ pcdata "Use another card" ]
            in
            div ~a:[ a_class [ "box" ; "payment-has-stripe" ]] [
              use_current_stripe ; use_another_card
            ]
          | `CreatePaymentMethod (stripe_option) ->
            let cc_number = Raw.input ~a:[ a_class [ "cc-number" ] ;
                                           a_placeholder "Card number" ; a_input_type `Text ;  ] () in
            let cc_exp_month = Raw.input ~a:[ a_class [ "cc-exp-month" ] ; a_placeholder "MM"; a_input_type `Text ] () in
            let cc_exp_year = Raw.input ~a:[ a_class [ "cc-exp-year" ] ; a_placeholder "YYYY"; a_input_type `Text ] () in
            let cc_cvc = Raw.input ~a:[ a_class [ "cc-cvc" ] ; a_placeholder "CVC" ; a_input_type `Text ] () in

            let add_card _ =
              match Ys_dom.get_value cc_number, Ys_dom.get_value cc_exp_month,
                    Ys_dom.get_value cc_exp_year, Ys_dom.get_value cc_cvc with
             | cc_number, _, _, _ when String.length cc_number <> 16 ->
               Help.warning "Please enter a 16 digits card number"
             | number, exp_month, exp_year, cvc ->
               try
               ignore_result
                 (lwt status, resp = Ys_stripe.create_token
                     ~number
                     ~exp_month
                     ~exp_year
                     ~cvc
                     () in
                  (match status with
                   | 200 | 400 | 402 ->
                     (* everything went fine, from an API perspective *)
                     Js.Opt.case
                       (resp##error : Ys_stripe.error Js.t Js.opt)
                       (fun () ->
                          let token = Js.to_string resp##id in
                          perform_with_token token)
                       (fun error ->
                          Help.warning (Js.to_string error##message)) ;
                     return_unit
                   | 401 -> Help.warning "Unauthorized"; return_unit
                   | 404 -> Help.warning "Not found" ; return_unit
                   | 500 | _ -> Help.warning "Panic, please contact team@accret.io" ; return_unit))
               with _ -> Help.warning "Incorrect inputs"
            in

            let use_current_stripe_option =
              match stripe_option with
                None -> pcdata ""
              | Some stripe ->
                button
                  ~button_type:`Button
                  ~a:[ a_onclick (fun _ -> perform (stripe.stripe_customer, stripe.stripe_last4)) ]
                  [ pcdata "Use card ending in " ; pcdata stripe.stripe_last4 ]
            in
            div ~a:[ a_class [ "box"; "payment-create-method" ]] [
              h3 [ pcdata "Use a new credit or debit card" ] ;
              div ~a:[ a_class [ "box-section" ]] [
                cc_number ;
              ] ;
              div ~a:[ a_class [ "box-section" ]] [
                cc_exp_month ; cc_exp_year ; cc_cvc
              ] ;
              div ~a:[ a_class [ "box-action" ]] [
                button ~button_type:`Button ~a:[ a_onclick add_card ] [ pcdata "Process payment" ] ;
                use_current_stripe_option ;
              ] ;
            ])
        payment_type

    in

    div ~a:[ a_class [ "payment" ]] [
      h1 [ pcdata view.label ] ;
      payment_amount ;
      R.node payment_method ;
      div ~a:[ a_class [ "payment-footnote" ]] [
        pcdata "Payments are secured by Stripe. Accretio doesn't store your payment information"
      ]
    ]

let dom = Template.apply %retrieve builder

}}
