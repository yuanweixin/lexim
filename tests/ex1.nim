discard """
  output: '''
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

type LexState = object 
  strBody : string 
  commentDepth : int 

type Token = object 
  thing : int 

genStringMatcher lex[LexState, Token]:
  r"\d+": echo "an integer ", input.substr(oldPos, pos-1), "##"
  "else": echo "an ELSE"
  "elif": echo "an ELIF"
  "end": echo "an END"
  r"[a-zA-Z_]\w+": echo "an identifier ", input.substr(oldPos, pos-1), "##"
  r".": echo "something else ", input.substr(oldPos, pos-1), "##"

proc main =
  var input = "the 0909 else input elif elseo end"
  var lexState = LexState(strBody:"", commentDepth:0)
  for thing in lex(input, lexState):
    discard

main()

