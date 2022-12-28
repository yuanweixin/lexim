import strutils
import unittest
import lexim
import std/sugar

type
  TokenType = enum
    OP
    INT
    IGNORE

  Token = object
    typ: TokenType
    val: string
    line: int
    col: int

  State = object
    lineStartPos: int
    lineNumber: int

# example of how you can implement line and col tracking.
genStringMatcher testLex[State, Token]:
  r"\n":
    inc lexState.lineNumber
    lexState.lineStartPos = pos
  r"\+|-|\*|/":
    yield Token(typ: OP, val: input.substr(oldPos, pos-1),
                 line: lexState.lineNumber, col: oldPos - lexState.lineStartPos)
  r"\d+":
    yield Token(typ: INT, val: input.substr(oldPos, pos-1),
                 line: lexState.lineNumber, col: oldPos - lexState.lineStartPos)
  r"\s":
    discard

test "test lexer counting newline":
  let
    str = "22 / \n 11 +\n40"
    expected = @[
      Token(typ: INT, val: "22", line: 1, col: 0),
      Token(typ: OP, val: "/", line: 1, col: 3),
      Token(typ: INT, val: "11", line: 2, col: 1),
      Token(typ: OP, val: "+", line: 2, col: 4),
      Token(typ: INT, val: "40", line: 3, col: 0)
    ]

  var state = State(lineStartPos: 0, lineNumber: 1)
  var ret = collect(newSeq):
    for token in testLex(str, state): token


  check expected == ret
