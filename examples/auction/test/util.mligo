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


#import "../src/auction_sc.mligo" "Auction"
#import "../../../lib/lib.mligo" "Breath"

type originated = Breath.Contract.originated

let originate (level: Breath.Logger.level) =
  Breath.Contract.originate
    level
    "auction_sc"
    Auction.main
    (None: Auction.storage)
    (0tez)

let bid (contract : (Auction.entrypoint, Auction.storage) originated) (qty: tez) () =
  Breath.Contract.transfert_to contract Bid qty

let expect_storage
    (storage : Auction.storage)
    (actor: Breath.Context.actor)
    (expected_amount: tez) : Breath.Result.result =
  Breath.Assert.is_some_and
    "The storage should be filled"
    (fun ({ current_leader_address; current_leader_amount} : Auction.current_leader) ->
        let expected_leader =
           Breath.Assert.is_equal "leader" current_leader_address actor.address in
        let expected_amount =
           Breath.Assert.is_equal "amount" current_leader_amount expected_amount in
        Breath.Result.and_then expected_leader expected_amount)
    storage
