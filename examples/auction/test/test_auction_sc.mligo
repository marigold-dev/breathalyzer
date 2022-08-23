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
#import "util.mligo" "Util"

let case_happy_path =
  Breath.Model.case
    "bid"
    "when everything is ok, it should upgrade the leader"
    (fun (level: Breath.Logger.level) ->
      let alice, bob, carol = Breath.Context.init_3_actors () in
      let contract = Util.originate level in

      let alice_action = Breath.Context.act_as alice (Util.bid contract 1tez) in
      let bob_action = Breath.Context.act_as bob (Util.bid contract 2tez) in
      let carol_action = Breath.Context.act_as carol (Util.bid contract 3tez) in

      let storage = Breath.Contract.storage_of contract in
      let balance = Breath.Contract.balance_of contract in

      Breath.Result.reduce [
        alice_action
      ; bob_action
      ; carol_action
      ; Breath.Assert.is_equal "balance" balance 6tez
      ; Util.expect_storage storage carol 3tez
      ])

let case_leader_try_to_be_upgraded_twice =
  Breath.Model.case
    "bid"
    "when the leader try to reupgrade the storage it should raise an error"
    (fun (level: Breath.Logger.level) ->
      let alice = Breath.Context.init_1_actor () in
      let contract = Util.originate level in

      let alice_fst_action = Breath.Context.act_as alice (Util.bid contract 1tez) in
      let alice_snd_action = Breath.Context.act_as alice (Util.bid contract 2tez) in

      let storage = Breath.Contract.storage_of contract in
      let balance = Breath.Contract.balance_of contract in

      Breath.Result.reduce [
        alice_fst_action
      ; Breath.Expect.fail_with_message "Same leader" alice_snd_action
      ; Breath.Assert.is_equal "balance" balance 1tez
      ; Util.expect_storage storage alice 1tez
      ])

let case_try_to_upgrade_with_a_lower_amount =
  Breath.Model.case
    "bid"
    "when a challenger try to upgrade the storage with a lower amount it should raise an error"
    (fun (level: Breath.Logger.level) ->
      let (alice, bob) = Breath.Context.init_2_actors () in
      let contract = Util.originate level in

      let alice_action = Breath.Context.act_as alice (Util.bid contract 2tez) in
      let bob_action = Breath.Context.act_as bob (Util.bid contract 1tez) in

      let storage = Breath.Contract.storage_of contract in
      let balance = Breath.Contract.balance_of contract in

      Breath.Result.reduce [
        alice_action
      ; Breath.Expect.fail_with_message "Amount should be greater" bob_action
      ; Breath.Assert.is_equal "balance" balance 2tez
      ; Util.expect_storage storage alice 2tez
      ])

let case_try_to_upgrade_with_a_null_amount =
  Breath.Model.case
    "bid"
    "when a challenger try to upgrade the storage without tez, it should raise an error"
    (fun (level: Breath.Logger.level) ->
      let alice = Breath.Context.init_1_actor () in
      let contract = Util.originate level in

      let alice_action = Breath.Context.act_as alice (Util.bid contract 0tez) in
      let storage = Breath.Contract.storage_of contract in
      let balance = Breath.Contract.balance_of contract in

      Breath.Result.reduce [
        Breath.Expect.fail_with_message "Amount cannot be null" alice_action
      ; Breath.Assert.is_equal "balance" balance 0tez
      ; Breath.Assert.is_none "The storage should be filled" storage
      ])


let () =
  Breath.Model.run_suites Trace [
    Breath.Model.suite "Suite for [auction_sc]" [
      case_happy_path
    ; case_leader_try_to_be_upgraded_twice
    ; case_try_to_upgrade_with_a_lower_amount
    ; case_try_to_upgrade_with_a_null_amount
    ]
  ]
