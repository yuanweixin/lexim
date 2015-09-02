
import lexim

proc main =
  var input = "the 0909 else input elif elseo end"
  var pos = 0
  while pos < input.len:
    let oldPos = pos
    match input, pos:
    of r"\d+": echo "an integer ", input.substr(oldPos, pos-1), "##"
    of "else": echo "an ELSE"
    of "elif": echo "an ELIF"
    of "end": echo "an END"
    of r"[a-zA-Z_]\w+": echo "an identifier ", input.substr(oldPos, pos-1), "##"
    of r".": echo "something else ", input.substr(oldPos, pos-1), "##"

main()
