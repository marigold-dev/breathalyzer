(* MIT License

   Copyright (c) 2022 Marigold <contact@marigold.dev>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal in
   the Software without restriction, including without limitation the rights to
   use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
   the Software, and to permit persons to whom the Software is furnished to do so,
   subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

#import "util.mligo" "Util"

type actor = {
  name : string
; initial_amount: tez
; address: address
}

let default_actors = [
    ("Baker", 10000000000tez)
  ; ("Alice", 4000000tez)
  ; ("Bob", 2000000tez)
  ; ("Carol", 8000000tez)
  ; ("David", 8000000tez)
  ; ("Eve", 8000000tez)
  ; ("Frank", 8000000tez)
]

(** [init actors] initialize bootstrap accounts. *)
let init_with (actors: (string * tez) list) : actor list =
  let number_of_accounts = List.size actors in
  let default_amounts =
    List.map (fun (_, value: string * tez) -> value) actors
  in
  let () = Test.reset_state number_of_accounts default_amounts in
  let (_counter, actors) =
    List.fold_left (fun ((i, actors), (name, value) : (nat * actor list) * (string * tez)) ->
      let address = Test.nth_bootstrap_account (int i) in
      let actor = {
        name = name
      ; initial_amount = value
      ; address = address
      }
      in
      (i + 1n, actor :: actors)
    ) (0n, ([] : actor list)) actors
   in
   Util.rev actors

(** [init_with_default n] initializes a context with [n] participants,
    the first one is the baker and the other are regular participants. *)
let init_with_default (n : int) : actor list =
  let actors = Util.take n default_actors in
  init_with actors

(** [init_default ()] will initialize a context with four participants,
    the first one is the baker and the other are regular participants. *)
let init_default () : actor * (actor * actor * actor) =
  let actors = init_with_default 4 in
  match actors with
  | [baker; alice; bob; carol] ->
    let () = Test.set_baker baker.address in
    baker, (alice, bob, carol)
  | _ -> Test.failwith "unreachable case"

(** [init_with_baker ()] initializes a context with one baker and returns
    it. *)
let init_with_baker () : actor =
  match init_with_default 1 with
  | [baker] ->
    let () = Test.set_baker baker.address in
    baker
  | _ -> Test.failwith "unreachable case"

(** [init_1_actor ()] initializes a context with one baker and one participant,
    and returns this participant. *)
let init_1_actor () : actor =
  match init_with_default 2 with
  | [baker; alice] ->
    let () = Test.set_baker baker.address in
    alice
  | _ -> Test.failwith "unreachable case"

(** [init_2_actors ()] initializes a context with one baker and two
    participants, and returns these participants. *)
let init_2_actors () : (actor * actor) =
  match init_with_default 3 with
  | [baker; alice; bob] ->
    let () = Test.set_baker baker.address in
    (alice, bob)
  | _ -> Test.failwith "unreachable case"

(** [init_3_actors ()] initializes a context with one baker and three
    participants, and returns these participants. *)
let init_3_actors () : (actor * actor * actor) =
  match init_with_default 4 with
  | [baker; alice; bob; carol] ->
    let () = Test.set_baker baker.address in
    (alice, bob, carol)
  | _ -> Test.failwith "unreachable case"

(** [init_4_actors ()] initializes a context with one baker and four
    participants, and returns these participants. *)
let init_4_actors () : (actor * actor * actor * actor) =
  match init_with_default 5 with
  | [baker; alice; bob; carol; dave] ->
    let () = Test.set_baker baker.address in
    (alice, bob, carol, dave)
  | _ -> Test.failwith "unreachable case"

(** [init_5_actors ()] initializes a context with one baker and four
    participants, and returns these participants. *)
let init_5_actors () : (actor * actor * actor * actor * actor) =
  match init_with_default 6 with
  | [baker; alice; bob; carol; dave; eve] ->
    let () = Test.set_baker baker.address in
    (alice, bob, carol, dave, eve)
  | _ -> Test.failwith "unreachable case"

(** [init_6_actors ()] initializes a context with one baker and six
    participants, and returns these participants. *)
let init_6_actors () : (actor * actor * actor * actor * actor * actor) =
  match init_with_default 7 with
  | [baker; alice; bob; carol; dave; eve; frank] ->
    let () = Test.set_baker baker.address in
    (alice, bob, carol, dave, eve, frank)
  | _ -> Test.failwith "unreachable case"

(** [act_as actor f] will perform the operation [f] in the POV of the given [actor]. *)
let act_as (type a) (actor: actor) (handler : unit -> a) : a =
  let old_source = Tezos.get_source () in
  let address = actor.address in
  let () = Test.set_source address in
  let result = handler () in
  let () = Test.set_source old_source in
  result
