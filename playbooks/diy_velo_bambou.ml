(*
 * diy_velo_bamblou
 *
 * ce playbook permet à un groupe de personnes de construire leur propre vélo
 * en bambou
 *
 * william@accret.io
 *
 *)

open Lwt

open Printf
open CalendarLib
open Calendar.Precise

open Api

open Eliom_content.Html5
open Eliom_content.Html5.D

open Message_parsers
open Toolbox

let author = "william@accret.io"
let name = "Vélo en bambou"
let description = "Ce playbook permet à un groupe de personne de construire leur propre vélo en bambou"

let version = 0

(* the stages *****************************************************************)

let tag_leader = "leader"

let ask_leaders_for_interested_people context () =
  lwt leaders = context.search_members ~query:tag_leader () in
  let run_id = new_run_id () in
  lwt _ =
    Lwt_list.iter_s
      (fun member ->
         lwt salutations = salutations_fr member in
         lwt _ =
           context.message_member
             ~member
             ~subject:"Nouvelle série de vélos en bambous"
             ~data:(data_run_id run_id)
             ~content:[
               salutations ; br () ;
               br () ;
               pcdata "J'aimerais lancer la fabrication d'une nouvelle série de vélos en bambous. L'idée est de faire une commande groupée de matériaux puis d'organiser une série d'ateliers afin que les personnes intéressées puissent s'entraider pour le montage de leur vélo sur mesure." ; br () ;
               br () ;
               pcdata "Est-ce que vous connaîtriez des personnes intéressées? Vous pouvez m'envoyer leur adresses emails et je leur enverrai une présentation du projet, ou leur transmettre directement ce message!" ; br () ;
               br () ;
               pcdata "Merci d'avance," ; br () ;
             ]
             ()
         in
         return_unit)
      leaders
  in
  lwt _ =
    context.message_supervisor
      ~subject:"Nouvelle série de vélos en bambous"
      ~data:(data_run_id run_id)
      ~content:[
        pcdata "Bonjour," ; br () ;
        br () ;
        pcdata "J'ai contacté " ; pcdata (string_of_int (List.length leaders)) ; pcdata " personnes pour leur demander si elles connaissent des personnes intéressées. Voyons voir!" ; br () ;
        br () ;
      ]
      ()
  in
  return `None

let tag_candidate = sprintf "candidate%Ld"

let extract_all_emails context message =
  match_lwt run_id_from_message context message with
    None ->
    lwt _ =
      context.forward_to_supervisor
        ~message
        ~subject:"Can't extract candidates"
        ~content:[ pcdata "Couldn't find a run id" ]
        ()
    in
    return `None
  | Some run_id ->
    lwt members = extract_and_create_all_members_from_message context message in
    lwt members = Lwt_list.filter_s (fun member -> lwt is_member = context.is_member ~member in return (not is_member)) members in
    context.log_info "found %d new members" (List.length members) ;
    match members with
      [] -> return `None
    | _ as members ->
      lwt _ =
        Lwt_list.iter_s
          (fun member -> context.tag_member ~member ~tags:[ tag_candidate run_id ])
          members
      in
      lwt _ =
        context.reply_to
          ~message
          ~content:[
            pcdata "Merci, je vais prendre contact avec ces personnes!"
          ]
        ()
      in
      return (`AskCandidatesForTheirOpinion run_id)

let mark_sender_as_interested context message =
  match_lwt run_id_from_message context message with
    None ->
    lwt _ =
      context.forward_to_supervisor
        ~message
        ~subject:"Can't extract candidates"
        ~content:[ pcdata "Couldn't find a run id" ]
        ()
    in
    return `None
  | Some run_id ->
    lwt member = context.get_message_sender ~message in
    lwt _ = context.tag_member ~member ~tags:[ tag_candidate run_id ] in
    return (`AskCandidatesForTheirOpinion run_id)

let send_summary context message =
  match_lwt run_id_from_message context message with
    None -> return `None
  | Some run_id ->
    lwt candidates = context.search_members ~query:(tag_candidate run_id) () in
    lwt _ =
      context.reply_to
        ~message
        ~data:(data_run_id run_id)
        ~content:[
          pcdata "Pour l'instant il y a " ; pcdata (string_of_int (List.length candidates)) ; pcdata " personnes intéressées."
        ]
        ()
    in
    return `None


let tag_already_asked = sprintf "alreadyasked%Ld"

let ask_candidates_for_their_opinion context run_id =
  lwt candidates = context.search_members ~query:(sprintf "%s -%s" (tag_candidate run_id) (tag_already_asked run_id)) () in
  lwt _ =
    Lwt_list.iter_s
      (fun member ->
         lwt _ =
           context.message_member
             ~member
             ~subject:"Quel sujet mettre ici?"
             ~data:(data_run_id run_id)
             ~content:[
               pcdata "Comment veux tu présenter le projet aux personnes? Le but est de savoir si ils sont très intéressés (= prêt à s'engager sur un paiment) / un peu intéressés (= veulent avoir plus d'infos) / pas intéressés (veulent quitter la liste de diffusion)"
             ]
             ()
         in
         lwt _ = context.tag_member ~member ~tags:[ tag_already_asked run_id ] in
         return_unit)
      candidates
  in
  return `None

(* the flow *******************************************************************)

PLAYBOOK

*ask_leaders_for_interested_people ~> `ExtractAllEmails of email ~> extract_all_emails ~> `AskCandidatesForTheirOpinion of int64 ~> ask_candidates_for_their_opinion
 ask_leaders_for_interested_people ~> `MarkSenderAsInterested of email ~> mark_sender_as_interested ~> `AskCandidatesForTheirOpinion of int64 ~> ask_candidates_for_their_opinion
 ask_leaders_for_interested_people ~> `SendSummary of email ~> send_summary
