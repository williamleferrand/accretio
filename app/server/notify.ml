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


open Lwt

open Eliom_content.Html5
open Eliom_content.Html5.D

open Ys_uid

type locator = { uid: Ys_uid.uid ; name : string ; email : string }

(* email content **************************************************************)

let footer () =
  div [
    pcdata "To tune your emails notifications settings, please visit the settings page"
  ]

(* helpers ********************************************************************)

let uids_to_locators ?(filter=(fun _ _ -> true)) uids =
  Lwt_list.map_p
    (fun uid ->
       try_lwt
         Lwt_log.ign_info_f "getting fields for user %d" uid ;
         match_lwt $member(uid)->(name, preferred_email, settings) with
            | (name, email, settings) when filter uid settings ->
              return (Some { uid; name; email })
            | _ -> return_none
       with
         exn ->
           Lwt_log.ign_info_f "error when getting field from user %d: %s" uid (Printexc.to_string exn) ;
           return_none)
    uids
  >>= Ys_list.flatten_lwt

let append_to_reference r s =
  r := !r ^ s

(* email sending queue (used to throttle AWS SES calls) ***********************)

module Throttler =
  Lwt_throttle.Make
    (struct
      type t = unit
      let equal _ _ = true
      let hash _ = 1 end)

type message = {
  uid : uid option ;
  locator : locator ;
  references : string option ; (* the parent in case we are forwarding the email *)
  in_reply_to : string option ; (* this is the message Id we're replying from *)
  reply_to : (string * string) option ;
  attachments : Object_message.attachment list ;
  subject : string ;
  content : Html5_types.div_content_fun Eliom_content.Html5.D.elt list ;
}

(* wrappers *******************************************************************)

let with_signature locator content =
  (* let url_settings = (Ys_config.get_string "url-prefix")^"/settings" in
  let greetings =
    if locator.name = "" then "Greetings," else Printf.sprintf "Dear %s," locator.name
  in
  [ pcdata greetings ; br () ;
    br ()
  ] @ content @ [ br () ;
                  pcdata "Should you need assistance, please don't hesitate to reply to this email." ; br () ;
                  br () ;
                  pcdata "Sincerely," ; br ();
                  br () ;
                  pcdata "The Accretio team" ; br () ;
                  br () ;
                  pcdata "-----" ; br () ;
                  pcdata "Too many messages? Turn on batch notifications at " ;
                  Raw.a ~a:[ a_href (uri_of_string  (fun () -> url_settings)) ] [ pcdata url_settings ] ; br () ;
                ] *)


  content @ [
    br () ;
    br () ;
    pcdata "-----" ; br () ;
    pcdata "Sent via accretio. Have you read our latest " ;
    Raw.a ~a:[ a_href (uri_of_string (fun () -> "https://medium.com/@wleferrand/the-global-village-23b7b467d1d8")) ] [ pcdata "Medium post" ] ;
    pcdata "?" ; br () ;
  ]

let with_batch_signature locator content =
  let url_settings = (Ys_config.get_string "url-prefix")^"/settings" in
  let greetings =
    if locator.name = "" then "Greetings," else Printf.sprintf "Dear %s," locator.name
  in
  [ pcdata greetings ; br () ;
    br ()
  ] @ content @ [ pcdata "That's all for today! Should you need assistance, please don't hesitate to reply to this email." ; br () ;
                  br () ;
                  pcdata "The Accretio team" ; br () ;
                  br () ;
                  pcdata "-----" ; br () ;
                  pcdata "Too much latency? Turn off batch notifications at " ;
                  Raw.a ~a:[ a_href (uri_of_string  (fun () -> url_settings)) ] [ pcdata url_settings ]
      ]

let message_queue, pusher =
  Lwt_stream.create_bounded
    (Ys_config.get_int "ses-outbound-max-queue-size")

let batches = Ocsipersist.open_table "notify_batches"
let batches_mutex = Lwt_mutex.create ()

let batch message =
  (* TODO: there should be a mutex here *)
  let key = Ys_uid.to_string message.locator.uid in
  Lwt_mutex.with_lock
    batches_mutex
    (fun () ->
       let m = ref "" in
       Printer.print_list (append_to_reference m) message.content ;
       lwt batch = try_lwt Ocsipersist.find batches key with Not_found -> return [] in
       Ocsipersist.add batches key ((message.locator, !m) :: batch))

let enqueue_message ?(immediate = false) ?(force_delayed = false) message =
(*  Lwt_log.ign_info_f "enqueing message to %d %s %s" message.locator.uid message.locator.name message.locator.email ;
  if message.locator.uid < 1 || immediate then
    begin
      Lwt_log.ign_info_f "pushing message immediately to %d %sn email is %s" message.locator.uid message.locator.name message.locator.email ;
      pusher#push { message with content = with_signature message.locator message.content }
    end
  else
    lwt settings = $member(message.locator.uid)->settings in
     match settings.Object_member.email_notify_batch_emails, force_delayed with
    | true, _ | _, true ->
      Lwt_log.ign_info_f "batching message for %d %s, will send later" message.locator.uid message.locator.name ;
      batch message
    | false, _ -> *)
      Lwt_log.ign_info_f "pushing message immediately to %d %s %s" message.locator.uid message.locator.name message.locator.email ;
      pusher#push { message with content = with_signature message.locator message.content }

let send_batches () =
  Lwt_mutex.with_lock
    batches_mutex
    (fun () ->
       Lwt_log.ign_info_f "Sending batched messages" ;
       lwt keys =
         Ocsipersist.fold_step
           (fun key batches keys ->
              match batches with
                [] -> return (key::keys)
              | (locator, _) :: _ ->
                let subject =
                  match List.length batches with
                    1 -> "1 message from Accretio today"
                  | size -> Printf.sprintf "%d messages from Accretio today" size
                in
                lwt _ = pusher#push
                  {
                    uid = None ;
                    subject ;
                    attachments = [] ;
                    references = None ; in_reply_to = None ; reply_to = None ;
                    locator ;
                    content =
                      with_batch_signature locator ([
                        pcdata "Here is your daily batch of Accretio news:" ; br () ;
                        br () ;
                        pcdata "-----" ; br () ;
                        br () ;
                        ] @ (List.fold_left (fun acc (_, message) -> Unsafe.data message :: br () :: pcdata "-----" :: br () :: br () :: acc ) [] batches))
                  } in
                return (key :: keys))
           batches
           [] in
       Lwt_list.iter_s (Ocsipersist.remove batches) keys)

let rec retry counter f a =
  try_lwt
    f a
  with
  | exn when counter = 0 -> Lwt.fail exn
  | _ -> retry (counter - 1) f a

let write_list list =
  let buf = Buffer.create 16 in
  let ch = new Netchannels.output_buffer buf in
  Netmime_string.write_value ch list ;
  Buffer.contents buf

let create_message message =
  let m = ref "" in
  Printer.print_list (append_to_reference m) ([ html (head (title (pcdata "")) []) (body (message.content)) ]) ;

  Lwt_log.ign_info_f "shipping message %s" !m ;

  lwt from_ =
    match message.reply_to with
      None -> return "'Accretio' <hi@accret.io>"
    | Some (shortlink, stage) ->
      let imap_prefix = Ys_config.get_string Ys_config.imap_prefix in
      match_lwt Object_society.Store.find_by_shortlink shortlink with
      | None ->
        return (Printf.sprintf "'Accretio' <%s+%s@accret.io>" imap_prefix shortlink)
      | Some uid ->
        lwt leader = $society(uid)->leader in
        lwt name = $member(leader)->name in
        return (Printf.sprintf "'%s' <%s+%s_%s@accret.io>" name imap_prefix shortlink stage)
  in

  let date =
    Netdate.mk_mail_date ~zone:Netdate.localzone (Unix.time()) in

  let header =
    [ "MIME-version", "1.0";
      "To", message.locator.email ;
      "From", from_ ;
      "Date", date ;
      "Content-Type", "text/html; charset=UTF-8" ;
      "Content-Transfer-Encoding", "base64" ]
  in

  let header =
    match message.in_reply_to with
      None ->
      ("Subject", message.subject) :: header
    | Some message_id ->
      ("Subject", message.subject)
      :: ("In-reply-to", message_id)
      :: header
  in

  lwt header =
    match message.references with
      None -> return header
    | Some reference ->
      return (("References", reference) :: header)
  in

  let mail_header =
    new Netmime.basic_mime_header header in

  let main_text_body =
    new Netmime.memory_mime_body !m in

  (* here we either do a simple body or a parts, depending if we have attachments *)

  let tree =
    match message.attachments with
      [] -> (mail_header, `Body main_text_body)
    | _ as attachments ->
      mail_header#update_field "content-type" "multipart/mixed" ;
      let text_body_header =
        Netmime.basic_mime_header [
          "Content-Type", "text/html; charset=UTF-8" ;
          "Content-Transfer-Encoding", "base64" ;
        ]
      in
      let attachments =
        List.map
          (fun attachment ->
             let header =
               Netmime.basic_mime_header [
                 "Content-Type", attachment.Object_message.content_type ;
                 "Content-Disposition",  "inline; filename=receipt" ;
                 "Content-Transfer-Encoding", "base64" ;
               ]
             in
             (header, `Body (new Netmime.memory_mime_body attachment.Object_message.content))
          )
          attachments
      in
      (mail_header, `Parts ((text_body_header, `Body main_text_body) :: attachments))
  in

  let buf = Buffer.create 16 in
  let ch = new Netchannels.output_buffer buf in
  Netmime_channels.write_mime_message ch tree ;
  let contents = Buffer.contents buf in
  return contents

let dequeue_messages =
  (* our current AWS rate is 5 emails/s, according to the management console *)
  let throttler = Throttler.create ~rate:4 ~max:8192 ~n:1 in
  let send message =
    try_lwt
      lwt _ = Throttler.wait throttler () in
      lwt raw_message = create_message message in
      retry 3 (SES.send_raw_email
                 ~creds:Vault.creds
                 ~raw_message) ()
      >>= fun message_id ->
      Lwt_log.ign_info_f "email to %s successfully sent, message_id is %s" message.locator.email message_id ;
      match message.uid with
        None -> return_unit
      | Some uid ->
        $message(uid)<-transport = (Object_message.(Email { offset = 0 ; message_id })) ;
        return_unit
    with exn -> Lwt_log.error_f ~exn "couldn't send email to %s: %s" message.locator.email (Printexc.to_string exn)
  in
  ignore_result (Lwt_stream.iter_s send message_queue)

let rec dequeue_batches () =
  let suggestion_batch_mailing_hour = Ys_config.get_int Ys_config.suggestion_batch_mailing_hour in
  let today = Int64.mul (Int64.div (Ys_time.now ()) 86400L) 86400L in
  let todaytime = Int64.add today (Int64.of_int (suggestion_batch_mailing_hour * 3600)) in
  let diff = Int64.sub todaytime (Ys_time.now ()) in
  let duration =
    if (diff > 0L) then
      Int64.to_int diff
    else
      Int64.to_int (Int64.add diff 86400L)
  in
  let timer = Lwt_timeout.create duration (fun () -> ignore_result (lwt _ = send_batches () in dequeue_batches (); return_unit)) in
  ignore (Lwt_timeout.start timer)


(* welcome message ************************************************************)

let send_welcome_message uid =
  lwt name, email = $member(uid)->(name, preferred_email) in
  let locator = { uid; name; email } in
  enqueue_message ~immediate:true
    {
      uid = None ;
      attachments = [] ;
      subject ="Welcome!" ;
      references = None ;
      in_reply_to = None ;
      reply_to = None ;
      locator ;
      content =
        [
          pcdata "Thanks for giving Accretio a try - we wish you fruitful encounters." ; br () ;
          br () ;
          pcdata "We are curently rolling out the alpha version of the platform and your comments are greatly appreciated." ; br ();
        ]
    }

(* recover the password *******************************************************)

let send_recovery_message uid token =
  lwt name, email = $member(uid)->(name, preferred_email) in
  let url = (Ys_config.get_string "url-prefix")^"/recover/"^token in
  let locator = { uid; name; email } in
  enqueue_message ~immediate:true
    {
      uid = None ;
      attachments = [] ;
      subject ="Password recovery" ;
      references = None ;
      in_reply_to = None ;
      reply_to = None ;
      locator ;
      content =
        [
          pcdata "To reset your password for your Accretio account, please visit the link below:" ; br () ;
          br () ;
          Raw.a ~a:[ a_href (uri_of_string  (fun () -> url)) ] [ pcdata url ] ; br ();
          br () ;
          pcdata "If clicking the link above doesn't work, please copy and paste the URL in a new browser window instead." ; br () ;
          br () ;
          pcdata "If you've received this mail in error, it's likely that another user entered
your email address by mistake while trying to reset a password. If you didn't
initiate the request, you don't need to take any further action and can safely
disregard this email." ; br ();
        ]
    }

(* send feedback **************************************************************)

let send_feedback session feedback contact_info =
  let session_string = Vault.session_to_string session in
  Lwt_log.ign_info_f "sending feedback %s from %s (session %s)" feedback
    contact_info session_string ;
  let locator =
    { uid = 1 ;
      name = "feedback admin" ;
      email = Ys_config.get_string Ys_config.email_feedback
    } in
  enqueue_message
    {
      uid = None ;
      locator ;
      references = None ;
      in_reply_to = None ;
      reply_to = None ;
      attachments = [] ;
      subject = "New feedback" ;
      content =
        [
          pcdata "New feedback:" ; br () ;
          br () ;
          i [ pcdata feedback ] ; br ();
          br () ;
          pcdata "Contact info: " ; pcdata contact_info ; br () ;
          pcdata "Session info: " ; pcdata session_string ; br () ;
      ]
    }


let new_message thread message =
  lwt owner, members = $thread(thread)->(owner, followers) in
  lwt author_name = $member(message.Object_thread.author)->name in

  lwt locators =
    uids_to_locators
      ~filter:(fun uid _ -> uid <> message.Object_thread.author)
      (owner :: (List.filter (fun i -> i <> owner) (Ys_uid.Edges.uids members)))
  in

  match message.Object_thread.content with
    Object_thread.Text text ->
    Lwt_list.iter_p
      (fun locator ->
         enqueue_message
           {
             uid = None ;
             locator ;
             references = None ;
             in_reply_to = None ;
             reply_to = None ;
             attachments = [] ;
             subject = author_name ^ " posted a new message" ;
             content =
               [
                 pcdata author_name ; pcdata " posted a new message to a discussion that you are following:" ; br () ;
                 br () ;
                 i [ pcdata text ] ; br () ;
                 br () ;
                 pcdata "You can reply to " ; pcdata author_name ; pcdata " by visiting the following url:" ; br () ;
                 br () ;
                 (* Raw.a ~a:[ a_href (uri_of_string (fun _ -> url)) ; a_target "_blank" ] [ pcdata url ] ; *) br () ;
               ]
           })
          locators
  | Object_thread.Image image ->
    Lwt_list.iter_p
      (fun locator ->
         enqueue_message
           {
             uid = None ;
             locator ;
             references = None ;
             in_reply_to = None ;
             reply_to = None ;
             attachments = [] ;
             subject = author_name ^ " posted a new message" ;
             content =
               [
                 pcdata author_name ; pcdata " posted a new picture to a discussion that you are following:" ; br () ;
                 pcdata "You can reply to " ; pcdata author_name ; pcdata " by visiting the following url:" ; br () ;
                 br () ;
                 (* Raw.a ~a:[ a_href (uri_of_string (fun _ -> url)) ; a_target "_blank" ] [ pcdata url ] ; *) br () ;
               ]
           })
      locators

(* send direct message ********************************************************)

let send_direct_message sender target message =
  lwt sender_name, sender_email = $member(sender)->(name, preferred_email) in
  lwt locators = uids_to_locators [ target ] in
  Lwt_list.iter_p
    (fun locator ->
       enqueue_message
         {
           uid = None ;
           locator ;
           references = None ; in_reply_to = None ; reply_to = None ;
           attachments = [] ;
           subject = Printf.sprintf "New direct message from %s" sender_name ;
           content = [
             pcdata sender_name ; pcdata " sent you a direct message:" ; br () ;
             br () ;
             i [ pcdata message] ; br ();
               br () ;
             pcdata "You can reply direcly to " ; pcdata sender_name ; pcdata " using their email address, " ; pcdata sender_email ; br () ;
           ]
         })
    locators


(* api emails going out *******************************************************)

let api_send_message ?(attachments=[]) ?in_reply_to reference society destination subject member content =
  lwt locators = uids_to_locators [ member ] in
  lwt shortlink = $society(society)->shortlink in
  Lwt_list.iter_p
    (fun locator ->
       enqueue_message
         {
           uid = None ;
           locator ;
           references = Some reference ;
           in_reply_to ;
           reply_to = Some (shortlink, destination) ;
           attachments ;
           subject ;
           content
         })
    locators

let api_forward_message ?(attachments=[]) reference society destination subject member original content =
  lwt locators = uids_to_locators [ member ] in
  lwt shortlink = $society(society)->shortlink in
  Lwt_list.iter_p
    (fun locator ->
       enqueue_message
         {
           uid = None ;
           locator ;
           references = Some reference ;
           in_reply_to = None ;
           reply_to = Some (shortlink, destination) ;
           attachments ;
           subject ;
           content ;
         })
    locators

let check_society society =
  lwt leader, name = $society(society)->(leader, name) in
  lwt locators = uids_to_locators [ leader ] in
  let url = (Ys_config.get_string "url-prefix")^"/society/"^(string_of_int society) in
   Lwt_list.iter_p
    (fun locator ->
       enqueue_message
         {
           uid = None ;
           locator ;
           references = None ;
           in_reply_to = None ;
           reply_to = None ;
           attachments = [] ;
           subject = Printf.sprintf "check society '%s'" name ;
           content =
             [
               pcdata "Hi," ; br () ;
               br () ;
               pcdata "Please check your society using the following link" ; br () ;
               br () ;
               Raw.a ~a:[ a_href (uri_of_string  (fun () -> url)) ] [ pcdata url ] ; br ();
               br () ;
               pcdata "Thanks," ; br () ;
             ]
         })
    locators

let send_message message =
  match_lwt $message(message)->destination with
  | Object_message.Member member ->
    lwt locators = uids_to_locators [ member ] in
    lwt subject, content, reference, society = $message(message)->(subject, content, reference, society) in
    lwt shortlink = $society(society)->shortlink in
    Lwt_list.iter_p
    (fun locator ->
       enqueue_message
         {
           uid = None ;
           locator ;
           references = Some reference ;
           in_reply_to = None ;
           reply_to = Some (shortlink, "") ;
           attachments = [] ;
           subject ;
           content =
             [
               pcdata content
             ]
         })
    locators
| _ -> return_unit
