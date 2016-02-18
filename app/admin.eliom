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
open React

open Eliom_content.Html5
open Eliom_content.Html5.D

open Vault
open Sessions

type member_op =
    MemberUpdatePreferredEmail of string
  | MemberAddEmail of string
  | MemberRemoveEmail of string
  | MemberUpdateName of string
  | MemberMakeArchived
  | MemberMakeActive
  | MemberMakeGhost
  | MemberMakeSuspended of string
  | MemberResetPassword
  | MemberMakeAdmin
  | MemberMakeDefault
  | MemberUpdateSettings
  | MemberUpdateSettingsBatched of bool
  | MemberUpdateRecoveryToken of string
  | MemberUpdateStatement of string deriving (Json)

type thread_op =
  | ThreadUpdateOwner of int
  | ThreadUpdateSubject of string
  | ThreadUpdateMessage
  | ThreadMessagesReset deriving (Json)

}}

{server{

(* getters ********************************************************************)

let fetch label getter =
  server_function ~name:("fetch-admin-"^label) Json.t<int>
  (fun (uid:int) ->
     Vault.protected_admin
       (fun session ->
          try_lwt
            lwt v = getter uid in
            return (Some v)
          with _ -> return_none
       ))

let fetch_member =
  fetch
    "member"
    (Object_member.Store.Unsafe.get : (int -> Object_member.t Lwt.t))

let fetch_thread =
  fetch
    "thread"
    (Object_thread.Store.Unsafe.get : (int -> Object_thread.t Lwt.t))

let query_member query =
  Lwt_log.ign_info_f "searching for member using query %s" query ;
  let open Ys_uid in
  lwt uids_name = try_lwt Object_member.Store.search_name query with _ -> return [] in
  let uids = Ys_uid.of_list uids_name in
  lwt uids =
    match_lwt Object_member.Store.find_by_email query with
      None -> return uids
    | Some uid -> return (UidSet.add uid uids)
  in
  Lwt_log.ign_info_f "found %d members using query %s" (UidSet.cardinal uids) query ;
  match UidSet.elements uids with
    [] -> return_none
  | uid :: _ -> return (Some uid)

let query_member = server_function ~name:"query-admin-member" Json.t<string> query_member

let query_thread query =
  return_none

let query_thread = server_function ~name:"query-admin-thread" Json.t<string> query_thread

(* setters ********************************************************************)

let apply_member_op =
   server_function ~name:"apply-admin-member" Json.t<int * member_op>
   (fun (uid, op) ->
      Vault.protected_admin_bool
        (fun session ->
           match op with
           | MemberUpdateName name ->
             lwt _ = $member(uid)<-name %% (fun _ -> name) in
             return_true
           | MemberUpdatePreferredEmail email ->
             lwt _ = $member(uid)<-preferred_email = email in
             $member(uid)<-emails %% (fun emails -> email :: List.filter (fun e -> e <> email) emails)
           | MemberAddEmail email ->
             $member(uid)<-emails %% (fun emails -> email :: List.filter (fun e -> e <> email) emails)
           | MemberRemoveEmail email ->
             $member(uid)<-emails %% (List.filter (fun e -> e <> email))
           | MemberUpdateRecoveryToken token ->
             $member(uid)<-recovery_token %% (fun _ -> token)
           | MemberResetPassword ->
             lwt _ = $member(uid)<-state = Object_member.Active in
             lwt token = Ys_random.random_string 48 in
             lwt _ = $member(uid)<-recovery_token %% (fun _ -> token) in
             lwt _ = Notify.send_recovery_message uid token in
             return_true
           | MemberMakeAdmin ->
             lwt _ = $member(uid)<-rights = Object_member.Admin in
             return_true
           | MemberMakeDefault ->
             lwt _ = $member(uid)<-rights = Object_member.Default in
             return_true
           | MemberMakeArchived ->
             lwt _ = $member(uid)<-state = Object_member.Archived in
             return_true
           | MemberMakeGhost ->
             lwt _ = $member(uid)<-state = Object_member.Ghost in
             return_true
           | MemberUpdateSettingsBatched choice ->
             lwt _ = $member(uid)<-settings %% (fun settings -> { settings with Object_member.email_notify_batch_emails = choice }) in
             return_true
           | MemberUpdateStatement statement ->
             lwt _ = $member(uid)<-statement = statement in
             return_true
           | _ ->
             Lwt_log.ign_info "the member op has NOT been applied" ;
             return_false
        ))

let apply_thread_op =
  server_function ~name:"apply-admin-thread" Json.t<int * thread_op>
  (fun (uid, op) ->
     Vault.protected_admin_bool
        (fun session ->
          match op with
          | ThreadUpdateOwner owner ->
            lwt _ = $thread(uid)<-owner = owner in
            return_true
          | ThreadUpdateSubject subject ->
            lwt _ = $thread(uid)<-subject = subject in
            return_true
          | ThreadMessagesReset ->
            lwt _ = $thread(uid)<-messages = [] in
            lwt _ = $thread(uid)<-number_of_messages = 0 in
            return_true
          | _ -> return_true)
  )

}}

{client{

open Ys_react

let wrap_apply f =
  fun arg ->
    try_lwt
      match_lwt f arg with
      | false -> Help.warning "Something went wrong" ; return_unit
      | true -> Help.warning "Done" ; return_unit
    with exn -> Help.error ("Error caught: " ^ (Printexc.to_string exn)); return_unit

let apply_member_op = wrap_apply %apply_member_op
let apply_thread_op = wrap_apply %apply_thread_op

let dom_graph ~print ~parse fetcher querier builder goto uid_option : Html5_types.div_content_fun elt React.signal =
  S.map
    (function
      | Anonymous ->
        div ~a:[ a_class [ "admin" ]] [
          div ~a:[ a_class [ "box" ]] [
            pcdata "Please connect first"
          ]
        ]
      | Connected session when not session.member_is_admin ->
        div ~a:[ a_class [ "admin" ]] [
          div ~a:[ a_class [ "box" ]] [
            pcdata "Are you lost?"
          ]
        ]
      | Connected session ->

        let uid_input = input
            ~a:[ a_input_type `Text ;
                 a_value (match uid_option with
                  None -> ""
                | Some text -> print text) ]
            () in
        let text_input = input
            ~a:[ a_input_type `Text ]
            ()
        in
        ignore (Lwt_js.yield () >>= fun _ -> Ys_dom.focus uid_input ; return_unit);
        Manip.Ev.onkeyup
          uid_input
          (fun ev ->
             if ev##keyCode = Keycode.return then
               begin
                 goto (try Some (parse (Ys_dom.get_value uid_input))
                       with _ -> None) ;
                 true
               end
             else
               true) ;
        Manip.Ev.onkeyup
          text_input
          (fun ev ->
             if ev##keyCode = Keycode.return then
               begin
                 let query = Ys_dom.get_value text_input in
                 if query <> "" then
                   begin
                     Lwt.ignore_result
                       (lwt result = querier query in goto result ; return_unit) ;
                     true
                   end
                 else
                   true
               end
             else
               true) ;

        match uid_option with
          None -> div ~a:[ a_class [ "admin" ]] [
            div ~a:[ a_class [ "box" ]] [
              div ~a:[ a_class [ "box-section" ]] [ uid_input ] ;
              div ~a:[ a_class [ "box-section" ]] [ text_input ] ;
            ]
          ]
       | Some uid ->
          let content, update_content = S.create None in
          Lwt.ignore_result
            (lwt content_option = fetcher uid in
             update_content content_option ; return_unit) ;
          div ~a:[ a_class [ "admin" ]] [
            div ~a:[ a_class [ "box" ]] [
              div ~a:[ a_class [ "box-section" ]] [ uid_input ] ;
              div ~a:[ a_class [ "box-section" ]] [ text_input ] ;
            ] ;
            R.node
              (S.map
                 (function
                   | None -> div ~a:[ a_class [ "box" ]] [ pcdata "Object not found" ]
                   | Some v -> builder uid v)
                 content)
          ])
    Sessions.session

module Helpers =
  struct

    let field label content =
      div ~a:[ a_class [ "clearfix" ; "field" ]] [
        div ~a:[ a_class [ "label"; "left" ]] [ pcdata label ] ;
        div ~a:[ a_class [ "content"; "left" ]] content
      ]

    let edit_string value update =
      let input = input ~a:[ a_input_type `Text ; a_value value ] () in
      [
        input ;
        button
          ~a:[ a_button_type `Button ;
               a_onclick (fun _ -> ignore_result (update (Ys_dom.get_value input))) ]
          [ pcdata "Update" ]
      ]

    let edit_string_list value add remove =
      let value = RList.init value in
      let op label callback =
        let input = input () in
        div [
          input ;
          button
            ~a:[ a_onclick (fun _ ->
                let s = Ys_dom.get_value input in
                if (s <> "") then
                  ignore_result (callback s)) ]

            [ pcdata label ]
        ]
      in
      [
        RList.map_in_div (fun s -> div [ pcdata s ]) value ;
        op "Add" (fun s -> RList.add value s ; add s) ;
        op "Remove" (fun s -> RList.remove value s ; remove s) ;
      ]

    let edit_int value update =
      let input = input ~a:[ a_value (string_of_int value) ] () in
      [
        input ;
        button
          ~a:[ a_button_type `Button ;
               a_onclick (fun _ -> ignore_result (update (int_of_string (Ys_dom.get_value input)))) ]
          [ pcdata "Update" ]
      ]

    let edit_uid value update =
      let input = input ~a:[ a_value (Ys_uid.to_string value) ] () in
      [
        input ;
        button
          ~a:[ a_button_type `Button ;
               a_onclick (fun _ -> ignore_result (update (int_of_string (Ys_dom.get_value input)))) ]
          [ pcdata "Update" ]
      ]

    let edit_uid_option value update =
      let input = input ~a:[ a_value (match value with None -> "none" | Some uid -> Ys_uid.to_string uid) ] () in
      [
        input ;
        button
          ~a:[ a_onclick (fun _ -> ignore_result (update (try Some (int_of_string (Ys_dom.get_value input)) with _ -> None ))) ]

          [ pcdata "Update" ]
      ]

    let edit_text value update =
      let textarea = Raw.textarea (pcdata value) in
      [
        textarea ;
        button
          ~a:[ a_onclick (fun _ ->
              ignore_result (update (Ys_dom.get_value_textarea textarea))) ]

          [ pcdata "Update" ]
      ]

    let edit_bool value update =
      let checkbox = input ~a:(match value with false -> [ a_input_type `Checkbox ] | true -> [ a_input_type `Checkbox ; a_checked `Checked ]) () in
      [
        checkbox ;
        button
          ~a:[ a_onclick (fun _ ->
              ignore_result (update (Ys_dom.is_checked checkbox))) ]

          [ pcdata "Update" ]
      ]

    let edit_timestamp value update =
      let input = input ~a:[ a_value (Ys_time.timestamp_to_isostring value) ] () in
      [
        input ;
          button
          ~a:[ a_onclick
                 (fun _ ->
                    ignore_result
                      (update
                         (Ys_time.isostring_to_timestamp (Ys_dom.get_value input))))
             ]

          [ pcdata "Update" ]
      ]

    let edit_authentication uid =
      [
        button
          ~a:[ a_button_type `Button ;
               a_onclick (fun _ -> ignore_result (apply_member_op (uid, MemberResetPassword))) ]
          [ pcdata "Reset Password (& send email)" ]
      ]

    let edit_rights uid right =
      let current_right, update_right = S.create right in
      [
        R.node
          (S.map
             (function
               | Object_member.Default ->
                 div [
                   span [ pcdata "Default" ] ;
                   button
                     ~a:[ a_button_type `Button ;
                          a_onclick
                            (fun _ -> ignore_result (apply_member_op (uid, MemberMakeAdmin)
                                                     >>= fun _ -> update_right Object_member.Admin ; return_unit)) ]

                     [ pcdata "Make Admin" ]
                 ]
               | Object_member.Admin ->
                 div [
                   span [ pcdata "Admin" ] ;
                   button
                     ~a:[ a_button_type `Button ;
                          a_onclick
                            (fun _ -> ignore_result (apply_member_op (uid, MemberMakeDefault)
                                                     >>= fun _ -> update_right Object_member.Default; return_unit)) ]
                     [ pcdata "Make Default" ]
                 ]
             )
             current_right)
      ]

    let edit_member_state uid state =
      let current_state, update_state = S.create state in

      let control_box but =
        let input_suspended = input  () in

        (if (but = `MemberMakeArchived) then pcdata "" else
            button
              ~a:[ a_onclick
                     (fun _ ->
                        ignore_result (apply_member_op (uid, MemberMakeArchived)
                                       >>= fun _ -> update_state Object_member.Archived; return_unit)
                     )
                 ]

              [ pcdata "Archive" ])

        :: (if (but = `MemberMakeGhost) then pcdata "" else
          button
            ~a:[ a_onclick
                   (fun _ ->
                      ignore_result (apply_member_op (uid, MemberMakeGhost)
                                     >>= fun _ -> update_state Object_member.Ghost; return_unit)
                   )
               ]

            [ pcdata "Ghostify" ])

        :: (if (but = `MemberMakeActive) then pcdata "" else
              button
                ~a:[ a_onclick
                       (fun _ ->
                          ignore_result (apply_member_op (uid, MemberMakeActive)
                                         >>= fun _ -> update_state Object_member.Active; return_unit)
                       )
                   ]

                [ pcdata "Activate" ])

        :: (if (but = `MemberMakeSuspended) then [] else
              [
                input_suspended ;
                button
                  ~a:[ a_onclick
                         (fun _ ->
                            let message = Ys_dom.get_value input_suspended in
                            ignore_result (apply_member_op (uid, MemberMakeSuspended message)
                                           >>= fun _ -> update_state (Object_member.Suspended message);
                                           return_unit)
                         )
                     ]

                  [ pcdata "Suspend" ] ;
              ])

        in

      [
        R.node
          (S.map
             (function
               | Object_member.Active ->
                 div (
                   span [ pcdata "Active" ]
                   :: control_box `MemberMakeActive
                 )
               | Object_member.Archived ->
                 div (
                   span [ pcdata "Archived" ]
                   :: control_box `MemberMakeArchived
                 )
               | Object_member.Ghost ->
                 div (
                   span [ pcdata "Ghost" ]
                   :: control_box `MemberMakeGhost
                 )
               | Object_member.Suspended message ->
                 div (
                   span [ pcdata "Suspended ("; pcdata message ; pcdata ") " ] ;
                   :: control_box `MemberMakeSuspended
                 )
             )
             current_state)
      ]

    (* edit edges *)

    let edit_edges tag_to_string string_to_tag redirect synchronize edges =
      let edges = RList.init edges in
       let format_edge (tag, uid) =
        let delete =
            button
            ~a:[ a_button_type `Button ;
                 a_onclick
                   (fun _ ->
                      RList.filter edges (function (_, uid') -> uid' <> uid)) ]

            [ pcdata "Unlink" ]
        in
        div ~a:[ a_class [ "left" ]] [
          span ~a:[ a_onclick (fun _ -> redirect (tag, uid)) ] [ pcdata (tag_to_string tag) ] ;
          span [ pcdata (Ys_uid.to_string uid) ] ;
          delete
        ]
      in
      let add_edges =
        let input_tag = input ~a:[ a_input_type `Text ; a_placeholder "tag" ]  () in
        let input_uid = input ~a:[ a_input_type `Text ; a_placeholder "uid" ]  () in
        let button =
          button
            ~a:[ a_onclick
                   (fun _ ->
                      try
                        Help.silent () ;
                        let tag = string_to_tag (Ys_dom.get_value input_tag) in
                        let uid = Ys_uid.of_string (Ys_dom.get_value input_uid) in
                        RList.append_unique (fun (t, e) -> t <> tag || e <> uid) edges (tag, uid) ;
                        Ys_dom.set_value input_tag "" ;
                        Ys_dom.set_value input_uid ""
                      with _ -> Help.warning "couldn't parse the tag or the uid")
               ]

            [ pcdata "Activate" ]
        in
        div ~a:[ a_class [ "edit-edges-add" ]] [
          input_tag ;
          input_uid ;
          button
        ]
      in

      let synchronize =
        div [
          button
            ~a:[ a_onclick
                   (fun _ ->
                      try
                        Help.warning "sync" ;
                        ignore_result (synchronize (RList.to_list edges))
                      with _ -> Help.warning "couldn't parse the tag or the uid")
               ]

            [ pcdata "Synchronize" ]
        ]
      in

      [
        RList.map_in_div ~a:[ a_class [ "edit-edges-list" ; "clearfix" ]] format_edge edges ;
        add_edges ;
        synchronize
      ]

    let view uid = field "uid" [ pcdata (Ys_uid.to_string uid) ]


  end

open Helpers

let builder_member uid v =

(*  let mask_to_string = function
    | `Cohort weight -> Printf.sprintf "cohort (%d)" weight
  in

  let string_to_mask _ = `Cohort 0 in

  let story_to_string = function
    | `Story -> "story"
  in

  let string_to_story _ = `Story in

  let member_to_string = function
    | `Member -> "member"
  in

  let string_to_member _ = `Member in
*)
  div ~a:[ a_class [ "box" ]] [

    view uid ;

    field  "created_on" [ pcdata (Int64.to_string v.Object_member.created_on) ] ;

    field "name"
      (edit_string
         v.Object_member.name
         (fun s -> apply_member_op (uid, (MemberUpdateName s)))) ;

    field "preferred_email"
      (edit_string
         v.Object_member.preferred_email
         (fun s -> apply_member_op (uid, (MemberUpdatePreferredEmail s)))) ;

    field "emails"
      (edit_string_list
         v.Object_member.emails
         (fun s -> apply_member_op (uid, (MemberAddEmail s)))
         (fun s -> apply_member_op (uid, (MemberRemoveEmail s)))) ;

    field "recovery_token"
      (edit_string
         v.Object_member.recovery_token
         (fun s -> apply_member_op (uid, MemberUpdateRecoveryToken s))) ;

    field "authentication" (edit_authentication uid) ;

    field "rights" (edit_rights uid v.Object_member.rights) ;

    field "state" (edit_member_state uid v.Object_member.state) ;

    field "batched" (edit_bool v.Object_member.settings.Object_member.email_notify_batch_emails (fun v -> apply_member_op (uid, MemberUpdateSettingsBatched v))) ;

    field "statement"
      (edit_string
        v.Object_member.statement
        (fun s -> apply_member_op (uid, (MemberUpdateStatement s)))) ;

   ]

let builder_thread uid v =
  let edit_messages messages =
    let reset =
      button

        ~a:[ a_onclick (fun _ -> ignore_result (apply_thread_op (uid, ThreadMessagesReset))) ]
        [ pcdata "Reset" ]
    in
    [
      reset
    ]
  in

  div ~a:[ a_class [ "box" ]] [

    view uid ;

    field "owner"
      (edit_uid
         v.Object_thread.owner
         (fun s -> apply_thread_op (uid, ThreadUpdateOwner s))) ;

    field "subject"
      (edit_string
         v.Object_thread.subject
         (fun s -> apply_thread_op (uid, (ThreadUpdateSubject s)))) ;

   field "messages" (edit_messages v.Object_thread.messages) ;

  ]


let dom_graph_member =
  dom_graph ~print:Ys_uid.to_string ~parse:Ys_uid.of_string
  %fetch_member
  %query_member
      builder_member

      (fun uid_option -> Service.goto (Service.AdminGraphMember uid_option))

let dom_graph_thread =
  dom_graph ~print:Ys_uid.to_string ~parse:Ys_uid.of_string
  %fetch_thread
  %query_thread
      builder_thread
      (fun uid_option -> Service.goto (Service.AdminGraphThread uid_option))


let dom_stats () =
  S.map
    (function
      | Anonymous ->
        div [ pcdata "Please connect first" ]
      | Connected session when not session.member_is_admin ->
        div [ pcdata "Are you lost?" ]
      | Connected session ->
        div [])
    Sessions.session

 let dom_i18n () =
   S.map
     (function
       | Anonymous ->
         div [ pcdata "Please connect first" ]
       | Connected session when not session.member_is_admin ->
         div [ pcdata "Are you lost?" ]
       | Connected session ->
         div [])
     Sessions.session

}}
