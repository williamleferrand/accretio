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

type t = {
  uid : uid ;
  name : string ;
  email : string ; (* <- do we want to leak that?? *)
  mutable love : int ;
  mutable is_loved : bool ;
  profile_picture : View_image.t option ;
}

let uid t = t.uid

}}

let to_view uid =
  lwt name, preferred_email, reviews, profile_picture = $member(uid)->(name, preferred_email, reviews, profile_picture) in
  let love =
    Ys_uid.Edges.cardinal_filtered
      (function _ -> true)
      reviews in
  lwt profile_picture =
    match profile_picture with
      None -> return_none
    | Some uid -> lwt view = View_image.to_view uid in return (Some view)
  in
  return { uid; email = preferred_email ; name ; love ; is_loved = false ; profile_picture }

let apply_is_loved_cohort member cohort view  =
  if view.uid = member then
    return { view with is_loved = true }
  else
    lwt reviews = $member(view.uid)->reviews in
    let cohorts = Edges.find_all member reviews in
    let is_loved = List.exists (function `ThanksForCohort cohort' when cohort' = cohort -> true | _ -> false) cohorts in
    return { view with is_loved }

{client{

open React
open Ys_react
open Eliom_content.Html5
open Eliom_content.Html5.D

let format member =
  pcdata member.name

}}
