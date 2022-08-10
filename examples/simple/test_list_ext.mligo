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

#import "list_ext.mligo" "L"
#import "../../lib/lib.mligo" "T"

let empty : int list = []

let test_cons_1 =
  let expected = [1] in
  let computed = L.cons 1 ([] : int list) in
  assert (expected = computed)

let test_cons_2 =
  let expected = [1; 2; 3; 4] in
  let computed = L.cons 1 [2; 3; 4] in
  assert (expected = computed)

let test_rev_1 =
  let expected = empty in
  let computed = L.rev expected in
  assert (expected = computed)

let test_rev_2 =
  let expected = [1; 2; 3] in
  let computed = L.rev [3; 2; 1] in
  assert (expected = computed)

let test_concat_1 =
  let expected = empty in
  let computed = L.concat expected expected in
  assert (expected = computed)

let test_concat_2 =
  let expected = [1] in
  let computed = L.concat empty [1] in
  assert (expected = computed)

let test_concat_3 =
  let expected = [1] in
  let computed = L.concat [1] empty in
  assert (expected = computed)

let test_concat_4 =
  let expected = [1; 2; 3; 4; 5; 6] in
  let computed = L.concat [1; 2; 3] [4; 5; 6] in
  assert (expected = computed)
