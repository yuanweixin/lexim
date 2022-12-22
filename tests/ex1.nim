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

proc main =
  var input = "the 0909 else input elif elseo end"
  match input:
    r"\d+": echo "an integer ", input.substr(oldPos, pos-1), "##"
    "else": echo "an ELSE"
    "elif": echo "an ELIF"
    "end": echo "an END"
    r"[a-zA-Z_]\w+": echo "an identifier ", input.substr(oldPos, pos-1), "##"
    r".": echo "something else ", input.substr(oldPos, pos-1), "##"
main()
