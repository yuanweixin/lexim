import lexim
import unittest

type State = object
  startPos: int
  endPosExcl: int

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

test "startPos, endPosExcl is 0,0 when accepting empty string":
  var input = ""
  var state = State(startPos: -9999, endPosExcl: -9999)
  let tokenIter = empLex(input)
  for thing in tokenIter(state):
    break
  check state.startPos == 0
  check state.endPosExcl == 0

test "startPos, endPosExcl is 0,0 when no match at all":
  var input = "hell"
  var state = State(startPos: -9999, endPosExcl: -9999)
  let tokenIter = helloLex(input)
  try:
    for thing in tokenIter(state):
      break
  except JammedException:
    discard
  check state.startPos == 0
  check state.endPosExcl == 0

test "startPos, endPosExcl is 0, input.len when matched all":
  var input = "hello"
  var state = State(startPos: -9999, endPosExcl: -9999)
  let tokenIter = helloLex(input)
  for thing in tokenIter(state):
    break
  check state.startPos == 0
  check state.endPosExcl == input.len

test "startPos, endPosExcl normal match":
  var input = "hello     world"
  var state = State(startPos: -9999, endPosExcl: -9999)
  let tokenIter = makeLexIter(input)
  for thing in tokenIter(state):
    break
  check state.startPos == 0
  check state.endPosExcl == 5

  for thing in tokenIter(state):
    break
  check state.startPos == 10
  check state.endPosExcl == 15

test "startPos, endPosExcl with mismatch":
  var input = "hello  .   world"
  var state = State(startPos: -9999, endPosExcl: -9999)
  let tokenIter = makeLexIter(input)
  for thing in tokenIter(state):
    break
  check state.startPos == 0
  check state.endPosExcl == 5
  try:
    for thing in tokenIter(state):
      break
  except JammedException:
    discard
  # "hello  ." so mismatch is at the 7th pos
  # the startPos, endPosExcl get updated for the discarded blanks (which are matches)
  # but once startPos is incremented before detection of the lexical error so it
  # ends up being identical to the endPosExcl.
  check state.startPos == 7
  check state.endPosExcl == 7
