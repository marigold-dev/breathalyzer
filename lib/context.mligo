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
#import "contract.mligo" "Contract"
#import "result.mligo" "Result"

type actor = {
  name : string
; initial_amount: tez
; address: address
}

(** [init actors] initializes bootstrap accounts. *)
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

(** [init_default ()] initializes a context with four participant, the first one
    being the baker and the others regular accounts. *)
let init_default () : actor * (actor * actor * actor) =
  let actors = init_with [
    ("Baker", 10000000000tez)
  ; ("Alice", 4000000tez)
  ; ("Bob", 2000000tez)
  ; ("Carol", 8000000tez)
  ]
  in
  match actors with
  | [baker; alice; bob; carol] ->
    let () = Test.set_baker baker.address in
    baker, (alice, bob, carol)
  | _ -> Test.failwith "unreachable case"

(** [act_as actor f] performs the operation [f] as [actor]. *)
let act_as (type a) (actor: actor) (handler : unit -> a) : a =
  let old_source = Tezos.get_source () in
  let address = actor.address in
  let () = Test.set_source address in
  let result = handler () in
  let () = Test.set_source old_source in
  result

(** [call_as actor contract parameter] performs the [contract] call [f] as
    [actor]. *)
let call_as
  (type a b)
  (actor: actor)
  (originated: (a, b) Contract.originated)
  (parameter: a) : Result.result =
  act_as actor (fun () -> Contract.call originated parameter)
