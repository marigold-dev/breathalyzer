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

(* Simple contract that expects you to wait one minute before two pings *)
module Ping = struct
  type storage = timestamp

  [@entry]
  let ping (_, last: unit * storage) : operation list * storage =
    let now = Tezos.get_now () in
    if now > last + 60 then
      [], now
    else
      failwith "come back later"
end

let (_, (alice, _, _)) = B.Context.init_default ()

let originated level initial_time =
  B.Contract.originate_module level "ping" (contract_of Ping) initial_time 0tez

let suite =
  B.Model.suite
    "Test suite for time-related operations"
    [
      B.Model.case
        "ping"
        "succeeds when you call it after waiting for more than one minute"
        (fun (level: B.Logger.level) ->
          let contract = originated level (0: timestamp) in

          B.Result.reduce [
            B.Context.call_as alice contract Ping;
            B.Context.wait_for 60n;
            B.Context.call_as alice contract Ping;
            B.Context.wait_for 60n;
            B.Context.call_as alice contract Ping;
          ]);

      B.Model.case
        "ping"
        "fails when you call it before waiting"
        (fun (level: B.Logger.level) ->
          let contract = originated level (0: timestamp) in

          B.Result.reduce [
            B.Context.call_as alice contract Ping;
            B.Context.wait_for 30n;
            B.Expect.fail_with_message
              "come back later"
              (B.Context.call_as alice contract Ping);
          ])

    ]
