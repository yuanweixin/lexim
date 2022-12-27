import lexim
import unittest
import std/sugar

genStringMatcher lex[int, string]:
    "a": yield "a"
    "aaa": yield "aaa"
    "b": yield "b"

template areEq(a, b) =
    if a != b:
        echo "expected " & $a & " but got " & $b
        doAssert false

test "consumes all input during recognition":
    let input = "aab"
    var lexState: int
    var res: seq[string]
    for s in lex(input, lexState):
        res.add s
    areEq ["a", "a", "b"], res
