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

type entrypoint =
  | Bid

type current_leader = {
  current_leader_address: address
; current_leader_amount: tez
}

type storage = current_leader option

type applied = operation list * storage

let bid (storage: storage) (quantity: tez) (user_address: address) : storage =
  if quantity <= 0tez then failwith "Amount cannot be null"
  else match storage with
  | None -> Some { current_leader_address = user_address; current_leader_amount = quantity }
  | Some { current_leader_address; current_leader_amount } ->
    let () = if current_leader_address = user_address then failwith "Same leader" in
    let () = if current_leader_amount >= quantity then failwith "Amount should be greater" in
    Some { current_leader_address = user_address; current_leader_amount = quantity }

let main (action: entrypoint) (storage: storage) : applied =
  match action with
  | Bid ->
    let quantity = Tezos.get_amount () in
    let user_address = Tezos.get_source () in
    let new_storage = bid storage quantity user_address in
    ([], new_storage)
