(* MIT License

   Copyright (c) 2023 Marigold <contact@marigold.dev>

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

#import "../lib/lib.mligo" "B"
#import "./simple_contract.jsligo" "Simple"

module Views = struct
  type storage = {
    simple: address;
    x: int
  }

  [@entry]
  let default () (storage: storage): operation list * storage =
    match (Tezos.call_view "get_value" () storage.simple: int option) with
      | None -> failwith "Should not happen"
      | Some x -> [], { storage with x = x }

  [@view]
  let view () (storage: storage): int =
    storage.x
end

let (_, (alice, bob, _)) = B.Context.init_default ()

let originated level =
  B.Contract.originate level "Simple" (contract_of Simple) 0 0tez

let case_views_1 =
  B.Model.case
    "call_view"
    "succeeds on an originated contract"
    (fun (level: B.Logger.level) ->
      let contract = originated level in
      B.Result.reduce [
        B.Context.call_as alice
          contract (Decrement 5);
        B.Context.call_as alice
          contract (Increment 3);
        B.Assert.is_equal
          "should be -2"
          (Tezos.call_view "get_value" () contract.originated_address)
          (Some (-2))
      ])

let case_views_2 =
  B.Model.case
    "call_view"
    "succeeds when a contract uses it on another contract"
    (fun (level: B.Logger.level) ->
      let contract = originated level in
      let initial_storage = { simple = contract.originated_address; x = 0 } in
      let view_contract =
        B.Contract.originate level "Views" (contract_of Views) initial_storage 0tez
      in
      B.Result.reduce [
        B.Context.call_as alice
          contract (Increment 10);
        B.Context.call_as alice
          view_contract (Default ());
        B.Assert.is_equal
          "should be 10"
          (Tezos.call_view "view" () view_contract.originated_address)
          (Some 10)
      ])

(* Simple signatures check to ensure Breathalyzer generates and stores the
correct public and private keys *)

module Signatures = struct
  type storage = unit

  [@entry]
  let default (key, signature: key * signature) (_: storage): operation list * storage =
    if Crypto.check key signature 0x1234 then
      ([], ())
    else
      failwith "Invalid signature"
end

let key_1 =
  B.Model.case
    "signature"
    "can be checked for the right key"
    (fun (level: B.Logger.level) ->
      let contract = B.Contract.originate level "Signatures" (contract_of Signatures) () 0tez in
      let sign = Test.sign alice.secret 0x1234 in
      B.Result.reduce [
        B.Context.call_as alice
          contract (Default (alice.key, sign))
      ])

let key_2 =
  B.Model.case
    "signature"
    "cannot be checked for the wrong key"
    (fun (level: B.Logger.level) ->
      let contract = B.Contract.originate level "Signatures" (contract_of Signatures) () 0tez in
      let sign = Test.sign alice.secret 0x1234 in
      B.Result.reduce [
        B.Expect.fail_with_message "Invalid signature"
          (B.Context.call_as alice
           contract (Default (bob.key, sign)))
      ])

let keys_suite =
  B.Model.suite
    "Test suite for keys and signatures"
    [ key_1
    ; key_2 ]

let views_suite =
  B.Model.suite
    "Test suite for Tezos views"
    [ case_views_1
    ; case_views_2
    ]
