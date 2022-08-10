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

#import "../lib/result.mligo" "R"

let test_result_and_then_1 =
  let expected = R.succeed_with 10n in
  let computed = R.and_then (R.succeed_with 3n) (R.succeed_with 7n) in
  assert (expected = computed)

let test_result_and_then_2 =
  let expected = R.fail_with "kaboom" in
  let computed = R.and_then (R.succeed_with 3n) (expected) in
  assert (expected = computed)

let test_result_and_then_3 =
  let expected = R.fail_with "kaboom" in
  let computed = R.and_then (expected) (R.succeed_with 3n) in
  assert (expected = computed)

let test_result_and_then_4 =
  let expected = Failed [Message "1"; Message "2"] in
  let computed = R.and_then (R.fail_with "1") (R.fail_with "2") in
  assert (expected = computed)

let test_result_and_then_lazy_1 =
  let expected = R.fail_with "kaboom" in
  let computed = R.and_then_lazy
     (expected)
     (fun () ->
        let () = failwith "boum" in
        R.succeed_with 3n)
  in
  assert (expected = computed)

let test_result_and_then_lazy_2 =
  let expected = Failed [Message "1"] in
  let computed = R.and_then_lazy (R.fail_with "1") (fun () -> (R.fail_with "2")) in
  assert (expected = computed)


let test_result_reduce_1 =
  let expected = R.succeed in
  let computed = R.reduce ([] : R.result list) in
  assert (expected = computed)

let test_result_reduce_2 =
  let expected = R.succeed_with 10n in
  let computed =
    R.reduce [
      R.succeed; R.succeed; R.succeed;
      R.succeed_with 6n; R.succeed;
      R.succeed_with 4n]
  in
  assert (expected = computed)

let test_result_reduce_2 =
  let expected = Failed [Message "kabooum"; Message "aha"] in
  let computed =
    R.reduce [
      R.succeed; R.succeed; R.fail_with "kabooum";
      R.succeed_with 6n; R.succeed;
      R.fail_with "aha"]
  in
  assert (expected = computed)
