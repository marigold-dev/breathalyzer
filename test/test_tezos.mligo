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

let (_, (alice, _, _)) = B.Context.init_default ()

let originated level =
  B.Contract.originate_module level "Simple" (contract_of Simple) 0 0tez

let case_views_1 =
  B.Model.case
    "call_view"
    "succeeds on a contract originated with originate_module"
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

let suite =
  B.Model.suite
    "Test suite for Tezos operations"
    [ case_views_1
    ]
