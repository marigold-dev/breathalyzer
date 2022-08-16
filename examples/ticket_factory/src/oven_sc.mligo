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

#import "common.mligo" "Common"

type entrypoint = Common.oven_entrypoint
type storage = Common.oven_storage
type applied = operation list * storage

let request_mint (mint_address: address) (qty: tez) : operation =
  if qty <= 0tez then failwith "oven_sc: amount should not null"
  else
    let callback : bytes ticket contract = Tezos.self "%oven_retreive_ticket" in
    let mint_sc : Common.mint_entrypoint contract =
      Tezos.get_contract_with_error
        mint_address
        "oven_sc: unable to find mint contract"
    in
    Tezos.transaction (Mint_process_mint callback) qty mint_sc

let retreive_ticket
    (counter: nat)
    (stored_ticket : bytes ticket option)
    (minted_ticket: bytes ticket) : bytes ticket option * nat =
  match stored_ticket with
  | None ->
     let (_, (_, n)), fresh_ticket = Tezos.read_ticket minted_ticket in
     (Some fresh_ticket, n)
  | Some ticket ->
      let (_, (_, n)), fresh_ticket = Tezos.read_ticket minted_ticket in
      (Tezos.join_tickets (ticket, fresh_ticket), counter + n)

let request_redeem (mint_address: address) (stored_ticket : bytes ticket option) : operation =
  match stored_ticket with
  | None -> failwith "oven_sc: no stored ticket"
  | Some ticket ->
    let (_, (_, qty)), ticket = Tezos.read_ticket ticket in
    if qty <= 0n then failwith "oven_sc: quantity is null"
    else
      let callback : unit contract = Tezos.self "%oven_retreive_tez" in
      let mint_sc : Common.mint_entrypoint contract =
      Tezos.get_contract_with_error
        mint_address
        "oven_sc: unable to find mint contract"
      in
      Tezos.transaction (Mint_process_redeem (ticket, callback)) 0tez mint_sc

let retreive_tez (owner_address : address) (retribution: tez) : operation =
  let beneficiary : unit contract =
     Tezos.get_contract_with_error owner_address "oven_sc: unable to find owner"
  in
  Tezos.transaction unit retribution beneficiary


let main (action, {owner_address; mint_address; stored_ticket; qty_ticket}: entrypoint * storage) : applied =
  if Tezos.get_source () = owner_address then
    let quantity = Tezos.get_amount () in
    match action with
    | Oven_request_mint ->
      let operation = request_mint mint_address quantity in
      ([operation], {
         owner_address = owner_address
       ; mint_address = mint_address
       ; stored_ticket = stored_ticket
       ; qty_ticket = qty_ticket} )
    | Oven_retreive_ticket minted_ticket ->
       let (new_ticket, counter) = retreive_ticket qty_ticket stored_ticket minted_ticket in
       ([], {
         owner_address = owner_address
       ; mint_address = mint_address
       ; stored_ticket = new_ticket
       ; qty_ticket = counter} )
    | Oven_request_redeem ->
      let operation = request_redeem mint_address stored_ticket in
      ([operation], {
         owner_address = owner_address
       ; mint_address = mint_address
       ; stored_ticket = (None : bytes ticket option)
       ; qty_ticket = 0n })
    | Oven_retreive_tez ->
      let operation = retreive_tez owner_address quantity in
      ([operation], {
         owner_address = owner_address
       ; mint_address = mint_address
       ; stored_ticket = (None : bytes ticket option)
       ; qty_ticket = 0n} )

  else failwith "oven_sc: not owner"
