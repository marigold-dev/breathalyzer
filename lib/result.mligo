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

(** The core of the test framework. *)

(** An alias for returing the gas consumption of a function *)
type gas = nat

(** A reason can be :
    - a Message, it is an [user-error]
    - an Execution error (ie: the balance is too low).
*)
type reason =
  | Message of string
  | Execution of test_exec_error

(** A test can be succeed, so the result is [Passed n] where [n] is the gas
   consumption of the test. Or [Failed] with a reason (see: [reason]). *)
type result =
  | Passed of gas
  | Failed of reason list

(** Build a [passed] result. *)
let succeed_with (n: gas)  = Passed n

(** Build a [passed] result with a null gas consumption. *)
let succeed = succeed_with 0n

(** Build a [failed] with a given message. *)
let fail_with (m: string) = Failed ([Message m])

(** [concat a b] concat a and b. *)
let concat (type a) (left: a list) (right: a list) : a list =
  List.fold_right (fun (x, xs: a * a list) -> x :: xs) left right

(** [and_then_lazy a f] will collapse a and the [f] performing if [a] is [passed].
    If both are passed, it will returns passed with the sum of the gas consumption,
    if there is a failure, it will returns the first one. *)
let and_then_lazy (left: result) (right: unit -> result) : result =
  match left with
  | Failed x -> Failed x
  | Passed x -> begin
    match right () with
    | Passed y -> Passed (x + y)
    | failure -> failure
  end

(** [and_then a b] will collapse two results. It produce an "early-return".
    If [a] and [b] are both succeed, the result will be the sum of the
    gas consumption of [a] and [b], otherwise, it will returns the concat of
    failing result. *)
let and_then (left: result) (right: result) =
  match (left, right) with
  | Passed x, Passed y -> Passed (x + y)
  | Failed x, Failed y -> Failed (concat x y)
  | _, Failed x -> Failed x
  | Failed x, _ -> Failed x

(** [reduce list] will apply [and_then] on each element of a list of result. *)
let reduce (list: result list) : result =
  List.fold_left
    (fun (x, y : result * result) -> and_then x y)
    succeed
    list
