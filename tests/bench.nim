from strutils import find

const
  asRegex = ".*[Pp]leasuring"

from times import cpuTime

import lexim/vm
var bc = vm.re(asRegex)
echo "code ", bc.code.len, " data: ", bc.data.len

template bench(text, doWork: untyped) =
  var t0 = cpuTime()
  doWork
  echo text, " took [s] ", cpuTime() - t0

import re

let thaRe = re.re("[Pp]leasuring", {reDotAll, reStudy})

import lexim

lexim.genStringMatcher lex[int, int]:
    r"[Pp]leasuring":
      yield pos
    r".":
      discard
  
import std/strscans
proc scan(input: string): int =
  var pos = 0
  while pos < input.len:
    if scanp(input, pos, {'P', 'p'}, "leasuring"):
      return pos
    inc pos
  return -1

import npeg
proc pegs(input: string): int =
  let p = peg search:
    search <- @({'P', 'p'} * "leasuring")
  let r = p.match(input)
  return if r.ok: r.matchLen else: -1

proc main =
  let inp = readFile("tests/benchdata.txt")
  when true:
    bench "vm 1":
      for i in 1..100:
        discard vm.matchLen(inp, bc)

    bench "re A":
      for i in 1..100:
        discard re.find(inp, thaRe)

    bench "find":
      for i in 1..100:
        discard find(inp, "pleasuring")

    bench "lexer":
      var res = -1 
      for i in 1..100:
        for nextp in lex(inp, res):
          break 

    bench "scanp":
      for i in 1..100:
        discard scan(inp)

    bench "npeg":
      for i in 1..100:
        discard pegs(inp)

    echo matchLen(inp, bc)
    echo re.find(inp, thaRe)+len"pleasuring"
    echo find(inp, "pleasuring")+len"pleasuring"
    var pos = -1 
    for nextp in lex(inp, pos):
      echo $nextp
      break
    echo scan(inp) # +len"pleasuring"
    echo pegs(inp) # +len"pleasuring"

main()
