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

#import "logger.mligo" "Logger"

type ('a, 'b) originated = {
  originated_typed_address : ('a, 'b) typed_address
; originated_contract : 'a contract
; originated_address : address
}

(** [originate level name f storage quantity] will originate the smart-contract
    [f] (which is a main entry point) and will provision it using [quantity] and
    with [storage] as a default storage value. *)
let originate
    (type a b)
    (level: Logger.level)
    (name: string)
    (main: (a * b -> (operation list * b)))
    (storage: b)
    (quantity: tez) : (a, b) originated =
  let typed_address, _, _ = Test.originate main storage quantity in
  let contract = Test.to_contract typed_address in
  let address = Tezos.address contract in
  let () =
    Logger.log level ("originated smart contract", name, address, storage, quantity)
  in
  { originated_typed_address = typed_address
  ; originated_contract = contract
  ; originated_address = address }