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
      if !debug_flag then Format.eprintf ">>> %d\n%s>>>\n%!" rc (String.sub i 0 rc);
      Imap.src c i 0 rc;
      loop (Imap.run c `Await)
    | `Await_dst ->
      let rc = Bytes.length o - Imap.dst_rem c in
      lwt _ = write_fully o 0 rc in
      if !debug_flag then Format.eprintf "<<< %d\n%s<<<\n%!" rc (String.sub o 0 rc);
      Imap.dst c o 0 (Bytes.length o);
      loop (Imap.run c `Await)
    | `Untagged _ as r -> return r
    | `Ok _ -> return `Ok
    | `Error e ->
      Format.eprintf "@[IMAP Error: %a@]@." Imap.pp_error e;
      Lwt.fail ImapError
  in
  loop (Imap.run c v)

let () =
  Ssl.init ()

let persistent_imap_offset : Uint32.t Eliom_reference.eref =
  Eliom_reference.eref
    ~scope:`Global
    ~persistent:"__mu_imap_offset"
    Uint32.zero

let reply_accretio_regexp =
  Str.regexp (Printf.sprintf "%s\+\\([A-Za-z0-9]+\\)_\\([A-Za-z0-9\_]*\\)" (Ys_config.get_string Ys_config.imap_prefix))

let rec never_stop f =
  try_lwt
    lwt _ = f () in never_stop f
  with exn ->
    Lwt_log.ign_error_f ~exn "something weird happened, restarting: %s" (Printexc.to_string exn) ;
    never_stop f


(* reimplem the imap loop *)

let process_email =
  let find_accretio_target addresses =
    List.find
      (fun address ->
         (address.ad_host = "accret.io") && Str.string_match reply_accretio_regexp address.ad_mailbox 0)
      addresses
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
                               ~name:address.ad_mailbox
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

  let rec extract_mime ((plain, html) as acc) (header, body) =
    match body with
    | `Body body ->
      (match fst (Netmime_string.scan_value_with_parameters (header#field "content-type") []) with
       | "text/html" -> (plain, Some body#value)
       | "text/plain" -> (Some body#value, html)
       | _ as s -> Lwt_log.ign_info_f "can't read content-type, %s" s ;  (plain, html))
    | `Parts parts -> List.fold_left extract_mime acc parts
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
                 | `Envelope env -> Some (find_accretio_target (env.env_to @ env.env_cc @ env.env_bcc))
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
            match List.mem target_stage Playbook.mailables with
            | false -> Lwt_log.ign_error_f "message %d attempted to wakeup stage %s in society %d, playbook %d, but it isn't mailable" (Uint32.to_int offset) target_stage society playbook ;
              return_none
            | true ->

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

              let body_plain, body_html = extract_mime (None, None) mime in

              let message_id = header#field "message-id" in
              Lwt_log.ign_info_f "message id is %s" message_id ;

              (* let references =
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
                  references @  Str.split (Str.regexp "[ \t]+") references_string

                with exn ->
                  Lwt_log.ign_error_f ~exn "error when reading references"; []
              in
              List.iter (fun reference -> Lwt_log.ign_info_f "found reference %s" reference) references ;
              Lwt_log.ign_info_f "there are %d references" (List.length references) ; *)

              lwt head_plain =
                match body_plain with
                  None ->
                  Lwt_log.ign_info_f "no head_plain ??" ;
                  return_none
                | Some reply ->
                  lwt head = Ys_reply_parser.parse_reply reply in
                  return (Some head)
              in

              match head_plain with
                None ->
                Lwt_log.ign_error_f "no head in message %d" (Uint32.to_int offset) ;
                return_none
              | Some content ->

              let subject =
                List.fold_left
                  (fun acc -> function
                     | `Envelope env -> env.env_subject
                     | _ -> acc)
                  ""
                  attributes
              in

              let transport = Object_message.(Email { offset = Uint32.to_int offset }) in
              let origin = Object_message.(Member sender) in
              let destination = Object_message.(Stage target_stage) in

              match_lwt
                Object_message.Store.create
                  ~society
                  ~subject
                  ~transport
                  ~content
                  ~origin
                  ~destination
                  () with
              | `Object_already_exists _ ->
                Lwt_log.ign_info_f "message already exists" ;
                return (Some society)
              | `Object_created message ->
                Lwt_log.ign_info_f "message object created, uid is %d" message.Object_message.uid ;
                let call = Ys_executor.({ stage = target_stage ; args = Executor.int_args message.Object_message.uid ; schedule = Immediate }) in
                lwt _ = $society(society)<-stack %% (fun stack -> call :: stack) in
                return (Some society)

      with
      | exn ->
        Lwt_log.ign_error_f ~exn "something happened: %s when parsing offset %d" (Printexc.to_string exn) (Uint32.to_int offset) ;
        return_none

  in

  dispatch


let process_emails emails =
  lwt targets = Lwt_list.map_s process_email emails in
  let targets = List.fold_left (fun acc -> function None -> acc | Some uid -> uid :: acc) [] targets in
  (* do something with the targets *)
  lwt _ = Lwt_list.iter_p Executor.step targets in
  return
    (List.fold_left
       (fun o1 (o2, _) -> if Uint32.compare o1 o2 > 0 then o1 else o2)
       Uint32.zero
       emails)


let wait_mail host port user pass mbox =

  let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  lwt he = Lwt_unix.gethostbyname host in
  lwt _ = Lwt_unix.connect fd (Lwt_unix.ADDR_INET (he.Lwt_unix.h_addr_list.(0), port)) in
  let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in
  lwt sock = Lwt_ssl.ssl_connect fd ctx in

  let c = Imap.connection () in
  let i = Bytes.create io_buffer_size in
  let o = Bytes.create io_buffer_size in
  Imap.dst c o 0 (Bytes.length o);

  match_lwt run sock i o c `Await with
  | `Ok ->
      (* let rec logout = function
        | `Untagged _ -> run sock i o c `Await >>= logout
        | `Ok -> return_unit *)
    let rec idle stop uidn = function
        | `Untagged (`Exists _) -> Lazy.force stop; run sock i o c `Await >>= idle stop uidn
        | `Untagged _ -> run sock i o c `Await >>= idle stop uidn
        | `Ok ->
          run sock i o c (`Cmd (Imap.search ~uid:true (`Uid [uidn, None])))
          >>= search uidn Uint32.zero
      and search uidn n = function
        | `Untagged (`Search (n :: _, _)) -> run sock i o c `Await >>= search uidn n
        | `Untagged _ -> run sock i o c `Await >>= search uidn n
        | `Ok ->
            if n = Uint32.zero then
              let cmd, stop = Imap.idle () in
              run sock i o c (`Cmd cmd) >>= idle stop uidn
            else
            let cmd = Imap.fetch ~uid:true ~changed:Uint64.one [n, Some n] [ `Envelope ; `Rfc822 ] in
            run sock i o c (`Cmd cmd) >>= fetch uidn n []
      and fetch uidn n accum = function
        | `Untagged (`Fetch (_, attrs)) ->
          let accum =
            try
              match List.fold_left (fun acc -> function (`Uid uid) -> Some uid | _ -> acc) None attrs with
                None -> accum
              | Some uid -> (uid, attrs) :: accum
            with _ -> accum
          in
          run sock i o c `Await >>= fetch uidn n accum
        | `Untagged _ -> run sock i o c `Await >>= fetch uidn n accum
        | `Ok ->
           Lwt_log.ign_info_f "done with the callback fetch, got %d elements to inspect\n" (List.length accum); flush stdout ;
           lwt imap_offset = process_emails accum in
           lwt _ = Eliom_reference.set persistent_imap_offset imap_offset in
           run sock i o c (`Cmd (Imap.examine mbox)) >>= select imap_offset
(*
          let name = match name with None -> "<unnamed>" | Some name -> name in
          Format.printf "New mail from %s, better go and check it out!\n%!" name;
          run sock i o c (`Cmd Imap.logout) >>= logout *)
      and select uidn = function
        | `Untagged (`Ok (`Uid_next uidn, _)) -> run sock i o c `Await >>= select uidn
        | `Untagged _ -> run sock i o c `Await >>= select uidn
        | `Ok ->
          let cmd, stop = Imap.idle () in
          run sock i o c (`Cmd cmd) >>= idle stop uidn
      and login = function
        | `Untagged _ -> run sock i o c `Await >>= login
        | `Ok ->
          lwt imap_offset = Eliom_reference.get persistent_imap_offset in
          run sock i o c (`Cmd (Imap.examine mbox)) >>= select imap_offset
      in
      run sock i o c (`Cmd (Imap.login user pass)) >>= login
  | `Untagged _ -> assert false

(*
let _ =
  Lwt_log.ign_info_f "starting imap loop" ;
  ignore_result
    (never_stop
       (fun () ->
          wait_mail
            (Ys_config.get_string Ys_config.imap_host)
            (Ys_config.get_int Ys_config.imap_port)
            (Ys_config.get_string Ys_config.imap_user)
            (Ys_config.get_string Ys_config.imap_pass)
            (Ys_config.get_string Ys_config.imap_mbox)))
*)
