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


(* this is the piece of code receiving emails *)
(* we'll have emails address such as reply+shortlink@accret.io *)


open Lwt
open Imap

open Ys_uid

let io_buffer_size = 4096

let debug_flag = ref true

exception ImapError

let run sock i o c v =
  let rec write_fully s off len =
    if len > 0 then
      lwt rc = Lwt_ssl.write sock s off len in
      write_fully s (off + rc) (len - rc)
    else
      return_unit
  in
  let rec loop = function
    | `Await_src ->
      lwt rc = Lwt_ssl.read sock i 0 (Bytes.length i) in
      Lwt_log.ign_info_f ">>> %d\n%s>>>\n%!" rc (String.sub i 0 rc) ;
      Imap.src c i 0 rc;
      loop (Imap.run c `Await)
    | `Await_dst ->
      let rc = Bytes.length o - Imap.dst_rem c in
      lwt _ = write_fully o 0 rc in
      Lwt_log.ign_info_f "<<< %d\n%s<<<\n%!" rc (String.sub o 0 rc) ;
      Imap.dst c o 0 (Bytes.length o);
      loop (Imap.run c `Await)
    | `Untagged _ as r -> return r
    | `Ok _ -> return `Ok
    | `Error e ->
      let s  = Format.asprintf "@[IMAP Error: %a@]@." Imap.pp_error e in
      Lwt_log.ign_info_f "%s" s ;
      Lwt.fail ImapError
  in
  loop (Imap.run c v)

(* let () =
  Ssl_threads.init () ;
  Ssl.init () *)

let sslctx = Ocsigen_http_client.sslcontext

let persistent_imap_offset : Uint32.t Eliom_reference.eref =
  Eliom_reference.eref
    ~scope:`Global
    ~persistent:"__mu_imap_offset"
    Uint32.one

let reply_accretio_regexp =
  Str.regexp (Printf.sprintf "%s\+\\([A-Za-z0-9]+\\)_\\([A-Za-z0-9\_]*\\)" (Ys_config.get_string Ys_config.imap_prefix))

let rec never_stop f =
  try_lwt
    lwt _ = f () in never_stop f
  with exn ->
    Lwt_log.ign_error_f ~exn "something weird happened, restarting: %s" (Printexc.to_string exn) ;
    lwt _ = Lwt_unix.sleep 10. in
    never_stop f

(* reimplem the imap loop *)

let process_email =
  let find_accretio_target addresses =
    try
      Some
        (List.find
           (fun address ->
              (address.ad_host = "accret.io") && Str.string_match reply_accretio_regexp address.ad_mailbox 0)
           addresses)
    with Not_found -> None
  in

  let map_senders_to_accretio_users attributes =
    Lwt_list.fold_left_s
      (fun acc response ->
         match response with
         | `Envelope env ->
           Lwt_list.fold_left_s
             (fun acc address ->
                let email = Printf.sprintf "%s@%s" address.ad_mailbox address.ad_host in
                match_lwt Object_member.Store.find_by_email email with
                | None ->
                  Lwt_log.ign_info_f "member not found for email %s, let's create a ghost" email ;
                  (match_lwt Object_member.Store.create
                               ~preferred_email:email
                               ~name:""
                               ~emails:[ email ]
                               ~state:Object_member.Ghost
                               () with
                  | `Object_already_exists _ -> return acc
                  | `Object_created obj -> return (UidSet.add obj.Object_member.uid acc))
                | Some uid -> return (UidSet.add uid acc))
             acc
             (env.env_from @ env.env_sender)
         | _ -> return acc)
      UidSet.empty
      attributes
  in

  let rec extract_body ((plain, html) as acc) (header, body) =
    match body with
    | `Body body ->
      (match Netmime_header.get_content_type header with
       | "text/html", _ -> (plain, Some body#value)
       | "text/plain", _ -> (Some body#value, html)
       | (_ as s, _) -> Lwt_log.ign_info_f "can't read content-type, %s" s ; (plain, html))
    | `Parts parts -> List.fold_left extract_body acc parts
  in

  let rec extract_attachments attachments (header, body) =
    match body with
    | `Body body ->
      (try
         (match Netmime_header.get_content_disposition header with
          | "attachment", params ->
            Lwt_log.ign_info_f "found attachment" ;
            (try
               let content_type, _ = Netmime_header.get_content_type header in
               Lwt_log.ign_info_f "found attachment with content_type %s" content_type ;
               let filename = Netmime_string.param_value (List.assoc "filename" params) in
               (filename, content_type, body#value) :: attachments
             with _ -> attachments)
          | _ -> attachments)
       with _ -> attachments)
    | `Parts parts -> List.fold_left extract_attachments attachments parts
  in

  let dispatch (offset, attributes) =
    try_lwt
      Lwt_log.ign_info_f "dispatching email %s" (Uint32.to_string offset) ;

      let accretio_target =
        List.fold_left
          (fun acc response ->
             match acc with
               Some _ as acc -> acc
             | None ->
               match response with
               | `Envelope env -> find_accretio_target (env.env_to @ env.env_cc @ env.env_bcc)
               | _ -> acc)
          None
          attributes
      in

      match accretio_target with
        None ->
        Lwt_log.ign_error_f "can't dispatch email %s, there is no accretio target in the enveloppe" (Uint32.to_string offset) ;
        return_none

      | Some address ->
        Lwt_log.ign_info_f "found accretio email %s@%s\n" address.ad_mailbox address.ad_host ; flush stdout ;

        let target_shortlink = Str.matched_group 1 address.ad_mailbox in
        let target_stage = Str.matched_group 2 address.ad_mailbox in

        match_lwt Object_society.Store.find_by_shortlink target_shortlink with
        | None -> Lwt_log.ign_error_f "shortlink %s doesn't map to any known society" target_shortlink ; return_none
        | Some society ->

          lwt playbook = $society(society)->playbook in
          let module Playbook = (val (Registry.get playbook) : Api.PLAYBOOK) in

          (* let's see if we can find the sender *)
          (* what do we want to do if we can't find the sender?? *)
          (* do we want to create it?? *)

          lwt senders = map_senders_to_accretio_users attributes in
          let senders = UidSet.elements senders in
          match senders with
          | [] -> Lwt_log.ign_error_f "no sender in message %d" (Uint32.to_int offset) ;
            return_none
          | sender :: _ ->

            let mime_option =
              List.fold_left
                (fun acc -> function
                   | `Rfc822 (Some text) ->
                     let ch = new Netchannels.input_string text in
                     let nstr = new Netstream.input_stream ch in
                     let mime = Netmime_channels.read_mime_message nstr in
                     nstr#close_in() ;
                     Some (text, mime)
                   | _ -> acc)
                None
                attributes
            in

            match mime_option with
              None ->
              Lwt_log.ign_info_f "no mime in email %s" (Uint32.to_string offset) ;
              return_none

            | Some (raw, ((header, _) as mime)) ->

              let body_plain, body_html = extract_body (None, None) mime in

              Lwt_log.ign_info_f "body extracted" ;

              let attachments = extract_attachments [] mime in

              Lwt_log.ign_info_f "found %d attachments" (List.length attachments) ;

              let message_id = header#field "message-id" in
              Lwt_log.ign_info_f "message id is %s" message_id ;

              let references =
                try
                  let references_string = header#field "references" in
                  Lwt_log.ign_info_f "references are %s" references_string ;
                  let references =
                    Netmime_string.fold_lines_p
                      (fun acc p0 p1 p2 is_last ->
                         String.trim (String.sub references_string p0 (p1 - p0)) :: acc)
                      []
                      references_string
                      0
                      (String.length references_string)
                  in
                  references @ Str.split (Str.regexp "[ \t]+") references_string

                with exn ->
                  Lwt_log.ign_error_f ~exn "error when reading references"; []
              in

              List.iter (fun reference -> Lwt_log.ign_info_f "found reference %s" reference) references ;
              Lwt_log.ign_info_f "there are %d references" (List.length references) ;

              lwt head_plain =
                match body_plain with
                  None ->
                  Lwt_log.ign_info_f "no head_plain ??" ;
                  return_none
                | Some raw ->
                  lwt head = Ys_reply_parser.parse_reply raw in
                  return (Some (raw, head))
              in

              match head_plain with
                None ->
                Lwt_log.ign_error_f "no head in message %d" (Uint32.to_int offset) ;
                return_none
              | Some (raw, content) ->

                let subject =
                  List.fold_left
                    (fun acc -> function
                       | `Envelope env -> env.env_subject
                       | _ -> acc)
                    ""
                    attributes
                in

                let transport = Object_message.(Email { offset = Uint32.to_int offset ; message_id }) in
                let origin = Object_message.(Member sender) in
                let destination =
                  if target_stage = "" then
                    Object_message.CatchAll
                  else
                    Object_message.(Stage target_stage) in

                let attachments =
                  List.map
                    (fun (filename, content_type, content) ->
                       Object_message.({ filename ; content_type ; content }))
                    attachments
                in

                match_lwt
                  Object_message.Store.create
                    ~society
                    ~subject
                    ~transport
                    ~content
                    ~origin
                    ~destination
                    ~reference:(Object_message.create_reference content)
                    ~references
                    ~attachments
                    ~raw
                    () with
                | `Object_already_exists _ ->
                  Lwt_log.ign_info_f "message already exists" ;
                  return (Some society)
                | `Object_created message ->
                  lwt _ = $member(sender)<-messages += (`Email, message.Object_message.uid) in
                  lwt _ = $society(society)<-inbox += ((`Message Object_society.({ received_on = Ys_time.now () ; read = false })), message.Object_message.uid) in
                  Lwt_log.ign_info_f "message object created, uid is %d" message.Object_message.uid ;

                  (* here we need to cancel reminding timers *)
                  lwt _ = Executor.cancel_reminders society message.Object_message.uid in
                  (* should we add the member to the society automatically?? *)

                  if target_stage = "" then
                    return (Some society)
                  else
                    match_lwt Playbook.dispatch_message_automatically message.Object_message.uid target_stage with
                    | None ->
                      Lwt_log.ign_info_f "dispatching the message automatically didn't succeeded for message %d and stage %s" message.Object_message.uid target_stage ;
                      return (Some society)
                    | Some call ->
                      Lwt_log.ign_info_f "dispatching the message automatically succeeded for message %d and stage %s" message.Object_message.uid target_stage ;
                      lwt _ = $message(message.Object_message.uid)<-action = Object_message.RoutedToStage call.Ys_executor.stage in
                      lwt _ = $society(society)<-stack %% (fun stack -> call :: stack) in
                      return (Some society)

    with
    | exn ->
      Lwt_log.ign_error_f ~exn "something happened: %s when parsing offset %d" (Printexc.to_string exn) (Uint32.to_int offset) ;
      Lwt.fail exn

  in

  dispatch


let process_emails emails =
  lwt targets = Lwt_list.map_s process_email emails in
  let targets = List.fold_left (fun acc -> function None -> acc | Some uid -> uid :: acc) [] targets in
  lwt _ = Lwt_list.iter_p Executor.step targets in
  (* this is a little bit liberal *)
  lwt _ = Lwt_list.iter_p Notify.check_society targets in

  lwt _ =
    Lwt_list.iter_p
      (fun society ->
         lwt leader = $society(society)->leader in
  Lwt_log.ign_info_f "asking %d to check society %d" leader society ;
  return_unit)
targets
in
return
  (List.fold_left
     (fun o1 (o2, _) -> if Uint32.compare o1 o2 > 0 then o1 else o2)
     Uint32.zero
     emails)


let wait_mail host port user pass mbox =

  let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  lwt he = Lwt_unix.gethostbyname host in
  lwt _ = Lwt_unix.connect fd (Lwt_unix.ADDR_INET (he.Lwt_unix.h_addr_list.(0), port)) in
  lwt sock = Lwt_ssl.ssl_connect fd !sslctx in

  let c = Imap.connection () in
  let i = Bytes.create io_buffer_size in
  let o = Bytes.create io_buffer_size in
  Imap.dst c o 0 (Bytes.length o);

  (* there is a strong styling issue here *)

  lwt imap_offset = Eliom_reference.get persistent_imap_offset in
  (* let imap_offset = Uint32.of_int 1 in *)

  let run_await () = run sock i o c `Await in
  let run_cmd cmd  = run sock i o c (`Cmd cmd) in

  let rec wait_ok = function
    | `Ok ->
      Lwt_log.ign_info_f "we're ok" ;
      return_unit
    | _ -> run_await () >>= wait_ok
  in

  let rec iterate_over_emails offset () =
    Lwt_log.ign_info_f "iterating over emails, starting at offset %s" (Uint32.to_string offset) ;
    let cmd = Imap.fetch ~uid:true ~changed:Uint64.one [ offset, None ] [ `Envelope ; `Rfc822 ] in
    run_cmd cmd >>= extract_messages offset []
  and extract_messages offset accum = function
    | `Untagged (`Fetch (_, attrs)) ->
      let accum =
        try
          match List.fold_left (fun acc -> function (`Uid uid) when uid >= offset -> Some uid | _ -> acc) None attrs with
            None -> accum
          | Some uid -> (uid, attrs) :: accum
        with _ -> accum
      in
      run_await () >>= extract_messages offset accum
    | `Untagged _ ->
      run_await () >>= extract_messages offset accum
    | `Ok ->
      match accum with
        [] ->
        Lwt_log.ign_info_f "no messages found, waiting" ;
        let cmd, stop = Imap.idle () in
        run_cmd cmd >>= idle stop offset
      | _ as accum ->
      Lwt_log.ign_info_f "done with the callback fetch, got %d messages to inspect\n" (List.length accum) ;
      lwt offset = process_emails accum in
      Lwt_log.ign_info_f "done investingating emails, max offset is %s" (Uint32.to_string offset) ;
      let offset = Uint32.succ offset in
      lwt _ = Eliom_reference.set persistent_imap_offset offset in
      iterate_over_emails offset ()
  and idle stop offset = function
    | `Untagged (`Exists _) ->
      Lazy.force stop ;
      run_await () >>= idle stop offset
    | `Untagged _ ->
      run_await () >>= idle stop offset
    | `Ok -> iterate_over_emails offset ()
  in

  Lwt_log.ign_info_f "connecting" ;

  run_await ()
  >>= wait_ok
  >>= (fun _ -> run_cmd (Imap.login user pass))
  >>= wait_ok
  >>= (fun _ -> run_cmd (Imap.examine mbox))
  >>= wait_ok
  >>= iterate_over_emails imap_offset


let start () =
  Lwt_log.ign_info_f "starting imap loop" ;
  never_stop
    (fun () ->
       wait_mail
         (Ys_config.get_string Ys_config.imap_host)
         (Ys_config.get_int Ys_config.imap_port)
         (Ys_config.get_string Ys_config.imap_user)
         (Ys_config.get_string Ys_config.imap_pass)
         (Ys_config.get_string Ys_config.imap_mbox))
