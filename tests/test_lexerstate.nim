import lexim
import unittest

type State = object
  pos: int

type Token = object
  x: int

genStringMatcher makeLexIter[State, Token]:
  r"[a-zA-Z_]\w+":
    yield Token(x: 1)
  r"\s":
    discard

genStringMatcher empLex[State, Token]:
  r"":
    yield Token(x: 1)

genStringMatcher helloLex[State, Token]:
  r"hello":
    yield Token(x: 1)

test "pos is 0 when accepting empty string":
  var input = ""
  var state = State(pos: -9999)
  let tokenIter = empLex(input)
  for thing in tokenIter(state):
    break
  check state.pos == 0

test "pos is 0 when no match at all":
  var input = "hell"
  var state = State(pos: -9999)
  let tokenIter = helloLex(input)
  try:
    for thing in tokenIter(state):
      break
  except JammedException:
    discard
  check state.pos == 0

test "pos is input.len when matched all":
  var input = "hello"
  var state = State(pos: -9999)
  let tokenIter = helloLex(input)
  for thing in tokenIter(state):
    break
  check state.pos == input.len

test "pos is position following token when match":
  var input = "hello     world"
  var state = State(pos: 0)
  let tokenIter = makeLexIter(input)
  for thing in tokenIter(state):
    break
  check state.pos == 5
  for thing in tokenIter(state):
    break
  check state.pos == input.len

test "pos is at last match if mismatch in middle":
  var input = "hello  .   world"
  var state = State(pos: 0)
  let tokenIter = makeLexIter(input)
  for thing in tokenIter(state):
    break
  try:
    for thing in tokenIter(state):
      break
  except JammedException:
    discard
  # "hello  ." so mismatch is at the 7th pos
  check state.pos == 7
