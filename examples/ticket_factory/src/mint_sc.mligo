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
#import "helper.mligo" "Helper"
#import "../../../lib/util.mligo" "Util"

type entrypoint = Common.mint_entrypoint
type storage = Common.mint_storage
type applied = operation list * storage

let mint (caller_contract : bytes ticket contract) (amount : tez) (storage : storage) : operation =
    if amount < storage.minimal_amount then 
        failwith ("mint_sc: this value should be at least equal to "
                 ^ (Util.tez_to_string storage.minimal_amount))
    else
        let payload = storage.fixed_payload in
        let amount = Helper.tez_to_nat amount in
        let ticket = Tezos.create_ticket payload amount in
        Tezos.transaction ticket 0tez caller_contract

let redeem (ticket : bytes ticket) (caller_contract : unit contract) (storage : storage): operation =
    let (minter_address,(payload,qty)), _ = Tezos.read_ticket ticket in
    if qty > 0n && minter_address = Tezos.get_self_address () && payload = storage.fixed_payload then
        Tezos.transaction unit (Helper.nat_to_tez qty) caller_contract
    else
        Tezos.transaction unit 0tez caller_contract

let main (action, storage : entrypoint * storage) : applied = match action with
    | Mint_process_mint caller_contract ->
        let amount = Tezos.get_amount () in
        let operation = mint caller_contract amount storage in
        ([operation], storage)
    | Mint_process_redeem (ticket, caller_contract) -> 
        let operation = redeem ticket caller_contract storage in
        ([operation], storage)