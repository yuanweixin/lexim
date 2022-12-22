#
#
#    Lexim - The Lexer Generator for Nim
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

import
  regexprs, nfa, macros, marshal
import std/tables
{.experimental: "caseStmtMacros".}
import fusion/matching
import std/sugar

proc findMacro(name: string): PRegExpr {.used.} = nil

proc newRange(a, b: NimNode): NimNode {.compileTime.} =
  newCall(bindSym"..", a, b)

proc charSetLit(cc: set[char]): NimNode {.compileTime.} =
  const
    MaxChar = '\xFF'
  result = newNimNode(nnkCurly)
  var c1 = '\0'
  while true:
    if c1 in cc:
      var c2 = c1
      while c2 < MaxChar and succ(c2) in cc: c2 = succ(c2)
      if c1 == c2:
        result.add newLit(c1)
      elif c2 == succ(c1):
        result.add newLit(c1)
        result.add newLit(c2)
      else:
        result.add newRange(newLit(c1), newLit(c2))
      c1 = c2
    if c1 >= MaxChar: break
    inc c1

proc charAt(s: string; i: int): char {.inline.} =
  result = if i < s.len: s[i] else: '\0'

proc currChar(s, i: NimNode; isCString: bool): NimNode {.compileTime.} =
  result =
    if isCString:
      newTree(nnkBracketExpr, s, i)
    else:
      newCall(bindSym"charAt", s, i)

proc getCmp(s, i: NimNode; x: set[char]; isCString: bool): NimNode {.compileTime.} =
  result = newCall(bindSym"contains",  charSetLit(x), currChar(s, i, isCString))

proc getSpecial(s, i: NimNode; x: Alphabet; isCString: bool): NimNode {.compileTime.} =
  result = newCall(bindSym"==", currChar(s, i, isCString), newLit(x.val))

proc newVarStmt(name, typ, value: NimNode): NimNode {.compiletime.} =
  return newTree(nnkVarSection, newTree(nnkIdentDefs, name, typ, value))

proc nextState(i, state: NimNode; dest: int): NimNode {.compileTime.} =
  newStmtList(newCall(bindSym"inc", i), newAssignment(state, newLit(dest)))


const initialStartCondition = "initial"
type Rule = object 
    startCondition : string 
    regex : string 
    actions : NimNode 

proc replaceBeginState(scToIdx: var Table[string,int], curSc, n:NimNode) : NimNode = 
    case n 
    of StmtList[all @stmts]:
        let nodes = collect(newSeq):
            for stmt in stmts: replaceBeginState(scToIdx, curSc, stmt)
        return newStmtList(nodes)
    of ElifBranch([@cond, @stmtList]):
        return newTree(nnkElifBranch, cond, replaceBeginState(scToIdx, curSc, stmtList))
    of Else([@stmtList]):
        return newTree(nnkElse, replaceBeginState(scToIdx, curSc, stmtList))
    of IfStmt[all @branches]:
        # newIfStmt requires tuples, very annoying to use, so I will just do a nested match here. 
        let newBranches = collect(newSeq):
            for branch in branches: replaceBeginState(scToIdx, curSc, branch)
        return newTree(nnkIfStmt, newBranches)            
    of Call([Ident(strVal: "beginState"), @sc is (kind: in nnkStrKinds)]):
        if sc.strVal notin scToIdx:
          raise newException(Exception, "Unknown state: " & sc.strVal)
        result = nnkAsgn.newTree(
                curSc,
                newLit(scToIdx[sc.strVal])
            )
    of Call([Ident(strVal: "beginState"), Ident(strVal: @statename)]):
        if statename notin scToIdx:
          raise newException(Exception, "Unknown state: " & statename)
        result = nnkAsgn.newTree(
                curSc,
                newLit(scToIdx[statename])
            )
    else:
        return n 

proc newGenMatcher(scToIdx : var Table[string, int], curSc: NimNode, a: DFA; s, i: NimNode, rules: seq[Rule]; isCString: bool, labelOffset: var int): NimNode {.compileTime.} =
  ## curSc: the genSym'ed variable we use to track current start condition
  let state = genSym(nskVar, "state")
  result = newStmtList()
  result.add newVarStmt(newTree(nnkPragmaExpr, state,
                          newTree(nnkPragma, ident"goto")),
                        newTree(nnkBracketExpr, bindSym"range",
                          newRange(newLit(1+labelOffset), newLit(a.stateCount+labelOffset))),
                        newLit(a.startState+labelOffset))
  var caseStmt = newNimNode(nnkCaseStmt)
  caseStmt.add state
  result.add newTree(nnkWhileStmt, bindSym"true", caseStmt)
  for src in countup(1, a.stateCount):
    let rule = getRule(a, src)
    var ifStmt = newNimNode(nnkIfStmt)
    for dest in allDests(a, src):
      let (others, cs) = allTransitions(a, src, dest)
      if cs != {}:
        ifStmt.add newTree(nnkElifBranch,
                           getCmp(s, i, cs, isCString),
                           nextState(i, state, dest+labelOffset))
      for ot in others:
        if ot.kind == reChar:
          ifStmt.add newTree(nnkElifBranch,
                             getSpecial(s, i, ot, isCString),
                             nextState(i, state, dest+labelOffset))
        else:
          doAssert false, "not supported " & $ot.kind
    let actions = 
      if rule >= 1:
        let actions = replaceBeginState(scToIdx, curSc, rules[rule-1].actions)
        newStmtList(actions, newTree(nnkBreakStmt, newNimNode(nnkEmpty)))
      else:
        newTree(nnkBreakStmt, newNimNode(nnkEmpty))
    if ifStmt.len == 0:
      caseStmt.add newTree(nnkOfBranch, newLit(src+labelOffset), actions)
    else:
      ifStmt.add newTree(nnkElse, actions)
      caseStmt.add newTree(nnkOfBranch, newLit(src+labelOffset), ifStmt)
  # need to insert a break statement for when we break out of this inner while loop to break
  # out of the outer while loop, because we should have recognized the longest match on this 
  # dfa to be out of its loop, and have not switched the start condition. 
  result.add nnkBreakStmt.newTree(newEmptyNode())
  labelOffset += a.stateCount 
  
template `/.`(x: string): string =
  (when defined(posix): "./" & x else: x)

proc tryGetRules(scToIdx: var Table[string,int], scToRules: var OrderedTable[int,seq[Rule]], n : NimNode) = 
    case n 
    of Call([Ident(strVal: @startCondition), StmtList([all @callList])]):
        for call in callList:
            case call
            of Call([@lit is (kind: in nnkStrKinds), @stmt is StmtList()]):
                if startCondition notin scToIdx:
                    scToRules[scToIdx.len] = @[]
                    scToIdx[startCondition] = scToIdx.len 
                let scIdx = scToIdx[startCondition]
                scToRules[scIdx].add Rule(startCondition: startCondition, regex:lit.strVal, actions: stmt)
            else:
                raise newException(Exception, "Unable to understand the start condition call with this AST repr " & astGenRepr call)
    of Call([@lit is (kind: in nnkStrKinds), @stmt is StmtList()]):
        let scIdx = scToIdx[initialStartCondition]
        scToRules[scIdx].add Rule(startCondition: initialStartCondition, regex:lit.strVal, actions:stmt)
    else:
        raise newException(Exception, "I do not understand the syntax of " & astGenRepr n)

macro match*(s: cstring|string; sections: varargs[untyped]): untyped =
  # dsl parsing 
  # insertion order matters for generating the goto
  var scToRules = initOrderedTable[int, seq[Rule]]() 
  var scToIdx = initTable[string,int]() 
  scToIdx[initialStartCondition] = 0
  scToRules[0] = @[]
  case sections 
  of ArgList([StmtList([all @calls])]):
      for call in calls: 
          tryGetRules(scToIdx, scToRules, call)
  else:
      raise newException(Exception, "I do not understand the syntax " & astGenRepr sections)

  when defined(leximVerbose):
    for sc, idx in scToIdx:
      echo "sc=" & sc & ", idx=" & $idx
    for sc, rules in scToRules:
      echo "sc=" & $sc & ",rules=" & repr rules

  # dfa generation 
  var scToDfa : seq[tuple[sc: int, dfa: DFA]] 
  let isCString = s.getType.typeKind == ntyCString
  for scIdx, rules in scToRules.pairs():
    when defined(leximSkipLexe):
      var bigRe: PRegExpr = nil
      var ruleIdx = 1
      for rule in rules:
        let rex = parseRegExpr(rule.regex, findMacro,
                                {reNoCaptures, reNoBackrefs})
        rex.rule = ruleIdx
        if bigRe.isNil: bigRe = rex
        else: bigRe = altExpr(bigRe, rex)
      var n: NFA
      var d, o: DFA
      regExprToNFA(bigRe, n)
      let alph = fullAlphabet(n)
      NFA_to_DFA(n, d, alph)
      optimizeDFA(d, o, alph)
      scToDfa.add (scIdx, o)
    else:
      # use 'lexe.exe' helper program in order to speedup lexer generation
      var res: seq[string] = @[]
      for rule in rules:
        res.add rule.regex
      let data = $$res
      writeFile("lexe.input", data)
      let o = to[DFA](staticExec(/."lexe", input="", cache=data))
      scToDfa.add (scIdx, o)


  # generate the nested goto's.
  # because states for individual DFA start at 1, are used for jump labels, 
  # but we have multiple DFA, so we have to track a global label offset. 
  var labelOffset = scToIdx.len
  # pos intentionally exposed to user action code 
  var pos = ident("pos")
  # oldPos intentionally exposed to user action code 
  var oldPos = ident("oldPos")
  
  var curSc = genSym(nskVar, "curSc") 
  var caseStmt = newNimNode(nnkCaseStmt)
  caseStmt.add curSc
  for _, (sc, dfa) in scToDfa:
    let g : NimNode = newGenMatcher(scToIdx, curSc, dfa, s, pos, scToRules[sc], isCString, labelOffset)
    caseStmt.add newTree(nnkOfBranch, newLit(sc), g)
  let curScUpperBoundInclusive = scToIdx.len-1
  # far less brain damaging to write than the eqv in AST 
  result = quote do:
      var `pos` = 0 
      var `curSc` : range[0..`curScUpperBoundInclusive`] = 0
      while `pos` < `s`.len:
        let `oldPos` = `pos` 
        while true:
          {.computedGoto.}
          `caseStmt`
  echo repr result 
