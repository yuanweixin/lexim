import lexim
import patty
import strutils
import unittest

variant MyToken:
  PLUS
  MULTI
  NUM(val: int)
  DOT
  LPAREN
  RPAREN
  IGNORE

# mostly revolves the escape patterns
genStringMatcher makeLex[int, MyToken]:
  r"\(":
    yield LPAREN()
  r"\)":
    yield RPAREN()
  r"\+":
    yield PLUS()
  r"\*":
    yield MULTI()
  r"\d+":
    yield NUM(parseInt(input.substr(oldPos, pos-1)))
  r"\s":
    discard

test "test calculator tokenize":
  var
    lexIter = makeLex("(20 + 1) * 2")
    state = 1
    res: seq[MyToken] = @[]
  for t in lexIter(state):
    res.add t
  doAssert res == @[LPAREN(), NUM(20), PLUS(), NUM(1), RPAREN(), MULTI(), NUM(2)]
