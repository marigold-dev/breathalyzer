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
#import "../../../lib/lib.mligo" "Breath"
#import "../src/oven_sc.mligo" "Oven"
#import "../src/mint_sc.mligo" "Mint"

type originated = Breath.Contract.originated

let mint_storage = {
    fixed_payload = 0x00;
    minimal_amount = 5tez
}

let originate_mint (storage : Mint.storage ) =
    Breath.Contract.originate
      Void
      "Mint"
      Mint.main
      storage
      0tez

let oven_storage (owner : address) (mint : address) =
    {
     stored_ticket = (None : bytes ticket option);
     owner_address = owner;
     mint_address = mint
    }

let originate_oven (storage : Oven.storage ) =
    Breath.Contract.originate
      Void
      "oven"
      Oven.main
      storage
      0tez

let mint_ticket
    (previous: Breath.Result.result)
    (oven: (Oven.Common.oven_entrypoint, Oven.Common.oven_storage) originated)
    (amount: tez)
    () : Breath.Result.result =
    Breath.transfer_to_contract
      previous
      oven.originated_contract
      Oven_request_mint
      amount

let case_right_owner =
    (*Init actors*)
    let (_,(alice,_,_)) = Breath.Context.init_default () in

    (*Originate mint and oven*)
    let originated_mint = originate_mint mint_storage in
    let storage = oven_storage alice.address originated_mint.originated_address in 
    let originated_oven = originate_oven storage in

    (*Try to mint a ticket with alice*)
    let pre_mint = mint_ticket Breath.Result.succeed originated_oven 5tez in
    let result_of_mint = Breath.Context.act_as alice pre_mint in

    Breath.Model.case
      "Oven"
      "Alice try to mint with her wallet with a right amount of tez."
      (fun (_: Breath.Logger.level) -> 
        let res = Breath.Result.is_succeed result_of_mint in
        Breath.Assert.is_true "should be true" res)

let case_right_owner_but_wrong_amount =
    (*Init actors*)
    let (_,(alice,_,_)) = Breath.Context.init_default () in

    (*Originate mint and oven*)
    let originated_mint = originate_mint mint_storage in
    let storage = oven_storage alice.address originated_mint.originated_address in 
    let originated_oven = originate_oven storage in

    (*Try to mint a ticket with alice*)
    let pre_mint = mint_ticket Breath.Result.succeed originated_oven 4tez in
    let result_of_mint = Breath.Context.act_as alice pre_mint in

    Breath.Model.case
      "Oven"
      "Alice try to mint with her wallet with a amount of tez insufficient."
      (fun (_: Breath.Logger.level) -> 
        let res = Breath.Result.is_succeed result_of_mint in
        Breath.Assert.is_false "should be false" res)


let case_wrong_owner =
    (*Init actors*)
    let (_,(alice,bob,_)) = Breath.Context.init_default () in

    (*Originate mint and oven*)
    let originated_mint = originate_mint mint_storage in
    let storage = oven_storage alice.address originated_mint.originated_address in 
    let originated_oven = originate_oven storage in

    (*Try to mint a ticket with alice*)
    let pre_mint = mint_ticket Breath.Result.succeed originated_oven 5tez in
    let result_of_mint = Breath.Context.act_as bob pre_mint in

    Breath.Model.case
      "Oven"
      "Bob try to mint with the Alice's wallet with a right amount of tez."
      (fun (_: Breath.Logger.level) -> 
        let res = Breath.Result.is_succeed result_of_mint in
        Breath.Assert.is_false "should be false" res)


let suite =
    Breath.Model.suite "A suite about oven" [
    case_right_owner;
    case_right_owner_but_wrong_amount;
    case_wrong_owner
    ]


