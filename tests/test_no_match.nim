import lexim
import unittest

type Token = object
  x: int
type State = object
  startPos: int
  endPosExcl: int

genStringMatcher helloLex[State, Token]:
  r"hello":
    yield Token(x: 1)

test "throws jammed exception":
  var state: State
  let tokenIter = helloLex("hell")

  expect(JammedException):
    discard tokenIter(state)

  # I am not sure why it isn't considered finished
  # if it throws, but adding the check to pin behavior.
  # In this case it's fine since if it's jammed it should
  # either keep jamming, or be finished and stop returning
  # tokens. Either way is correct. In fact, keep jamming
  # makes it less error prone for user because they don't
  # have to remember it was jammed before.
  #
  # just throw it in a for loop and check it a few times.
  for i in 0..10:
    check not finished(tokenIter)
    expect(JammedException):
      discard tokenIter(state)
