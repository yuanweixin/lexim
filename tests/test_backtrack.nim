import lexim
import unittest
import std/sugar

type LexState = object
    pos: int
genStringMatcher makeLex[LexState, string]:
    "a": yield "a"
    "aaa": yield "aaa"
    "b": yield "b"

template areEq(a, b) =
    if a != b:
        echo "expected " & $a & " but got " & $b
        doAssert false


test "consumes all input during recognition":
    let input = "aab"
    var lexState = LexState(pos: 0)
    var res: seq[string]
    let lexIter = makeLex(input)
    for s in lexIter(lexState):
        res.add s
    areEq ["a", "a", "b"], res
