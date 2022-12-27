discard """
  output: '''
String( hello world )
an identifier the##
something else  ##
an integer 0909##
something else  ##
an ELSE
something else  ##
an identifier input##
something else  ##
an ELIF
something else  ##
an identifier elseo##
something else  ##
an END'''
"""

import lexim

type State = object
  strBody: string
  commentDepth: int

type Token = object
  thing: int

genStringMatcher lex[State, Token]:
  "begin string":
    beginState(string)
  r"\d+":
    echo "an integer ", input.substr(oldPos, pos-1), "##"
  "else":
    echo "an ELSE"
  "elif":
    echo "an ELIF"
  "end": echo "an END"
  r"[a-zA-Z_]\w+":
    echo "an identifier ", input.substr(oldPos, pos-1), "##"
  r".":
    echo "something else ", input.substr(oldPos, pos-1), "##"
  string:
    "end string":
      echo "String(" & lexState.strBody & ")"
      lexState.strBody = ""
      beginState(initial)
    r".":
      lexState.strBody.add input.substr(oldPos, pos-1)

proc main =
  var input = "begin string hello world end stringthe 0909 else input elif elseo end"
  var state = State(strBody: "", commentDepth: 0)
  for thing in lex(input, state):
    discard

main()
