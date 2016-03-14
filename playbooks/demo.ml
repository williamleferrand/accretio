open Lwt
open Api

let author = "william@accret.io"
let name = "Demo"
let description = "Demo playbook for the landing page"

let version = 0
let tags = ""

(* the individual stages ****************************************************)

let pick_up_a_restaurant _ _ =
  return (`YelpLink "")

let suggest_place_to_friends _ _ =
  match Random.bool () with
    true -> return `MajoritySayYes
  | false -> return `MajoritySayNo

let book_restaurant _ _ =
  return `None

(* the graph ****************************************************************)

PLAYBOOK

  pick_up_a_restaurant ~> `YelpLink of string ~> suggest_place_to_friends ~> `MajoritySayYes ~> book_restaurant
                                                 suggest_place_to_friends ~> `MajoritySayNo ~> pick_up_a_restaurant
