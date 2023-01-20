#
#
#    Lexim - The Lexer Generator for Nim
#        (c) Copyright 2022 Wei Xin Yuan
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#
import
  lexim/regexprs, lexim/nfa, macros, marshal
import std/tables
{.experimental: "caseStmtMacros".}
import fusion/matching
import std/sugar
import os

export nfa
export regexprs

type
  JammedException* = object of IOError
  BadDslInputException* = object of ValueError

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

proc getCmp(s, i: NimNode; x: set[char];
    isCString: bool): NimNode {.compileTime.} =
  result = newCall(bindSym"contains", charSetLit(x), currChar(s, i, isCString))

proc getSpecial(s, i: NimNode; x: Alphabet;
    isCString: bool): NimNode {.compileTime.} =
  result = newCall(bindSym"==", currChar(s, i, isCString), newLit(x.val))

proc newVarStmt(name, typ, value: NimNode): NimNode {.compiletime.} =
  return newTree(nnkVarSection, newTree(nnkIdentDefs, name, typ, value))

proc nextState(i, state: NimNode; dest: int): NimNode {.compileTime.} =
  newStmtList(newCall(bindSym"inc", i), newAssignment(state, newLit(dest)))


const initialStartCondition = "initial"
type
  Rule = object
    startCondition: string
    regex: string
    actions: NimNode

  CaseLabel = distinct int
  StartCond = distinct int
  RuleIdx = distinct int
  CodegenCtx = object
    scStartOffsets: seq[int] # for each start condition (dfa), what is the state number of its starting state.
    scToIdx: Table[string, int] # start condition label to its assigned index.
    scToRules: seq[seq[Rule]] # think of it as a start condition idx to the list of regexes it implements
    dfas: seq[DFA]              # each dfa is for a start condition
    labelOffset: int # used during codegen to track the offset to add to a state number, accounting for previous dfa's and jammed state.

    # table because multiple accept states can exist for a single rule.
    # The caseLabel needs to be tracked because we need that for jumps from accept states with outgoing transitions.
    scRuleToAdditionalCaseBranches: OrderedTable[(StartCond, RuleIdx), (
        CaseLabel, NimNode)]

proc `==` (a, b: StartCond): bool {.borrow.}
proc `==` (a, b: RuleIdx): bool {.borrow.}

proc replaceBeginState(curSc: NimNode; ctx: var CodegenCtx; state,
  # goes through the AST and replace "beginState(someStr)"
  # with `curSc` = `startState` where startState is the index of
  # the start state we look up using mapping in the ctx object.
    n: NimNode): NimNode =
  case n
  of StmtList[all @stmts]:
    let nodes = collect(newSeq):
      for stmt in stmts: replaceBeginState(curSc, ctx, state, stmt)
    return newStmtList(nodes)
  of ElifBranch([@cond, @stmtList]):
    return newTree(nnkElifBranch, cond, replaceBeginState(curSc, ctx, state, stmtList))
  of Else([@stmtList]):
    return newTree(nnkElse, replaceBeginState(curSc, ctx, state, stmtList))
  of IfStmt[all @branches]:
    # newIfStmt requires tuples, very annoying to use, so I will just do a nested match here.
    let newBranches = collect(newSeq):
      for branch in branches: replaceBeginState(curSc, ctx, state, branch)
    return newTree(nnkIfStmt, newBranches)
  of Call([Ident(strVal: "beginState"), @sc is (kind: in nnkStrKinds)]):
    if sc.strVal notin ctx.scToIdx:
      raise newException(BadDslInputException,
          "Unknown state label used in beginState. state=" & sc.strVal)
    let startState = ctx.scStartOffsets[ctx.scToIdx[sc.strVal]]
    result = quote do:
      `curSc` = `startState`
  of Call([Ident(strVal: "beginState"), Ident(strVal: @statename)]):
    if statename notin ctx.scToIdx:
      raise newException(BadDslInputException,
          "Unknown state label used in beginState. state=" & statename)
    let startState = ctx.scStartOffsets[ctx.scToIdx[statename]]
    result = quote do:
      `curSc` = `startState`
  else:
    return n

proc genMatcher(caseStmt: NimNode; ctx: var CodegenCtx; sc: StartCond;
    state: NimNode; curSc: NimNode; a: DFA; s, pos: NimNode; rules: seq[Rule];
    isCString: bool; stateCnt: int; lastAccPos, lastAccStateAction,
    lexState: NimNode) {.compileTime.} =
  ## curSc: the genSym'ed variable track cur start cond's start state.
  ## state: the genSym'ed variable for cur state inside the while loop.
  for src in countup(1, a.stateCount):
    let rule = getRule(a, src)
    var ifStmt = newNimNode(nnkIfStmt)
    for dest in allDests(a, src):
      let (others, cs) = allTransitions(a, src, dest)
      if cs != {}:
        ifStmt.add newTree(nnkElifBranch,
                           getCmp(s, pos, cs, isCString),
                           nextState(pos, state, dest+ctx.labelOffset))
      for ot in others:
        if ot.kind == reChar:
          ifStmt.add newTree(nnkElifBranch,
                             getSpecial(s, pos, ot, isCString),
                             nextState(pos, state, dest+ctx.labelOffset))
        else:
          doAssert false, "not supported " & $ot.kind
    var caseLabel: CaseLabel
    let actions =
      if rule >= 1:
        let key = (sc, rule.RuleIdx)
        if ifStmt.len > 0:
          if key in ctx.scRuleToAdditionalCaseBranches:
            caseLabel = ctx.scRuleToAdditionalCaseBranches[key][0]
          else:
            let actions = replaceBeginState(curSc, ctx, state, rules[
                rule-1].actions)
            # the actions need to go into its own, separate case branch
            caseLabel = (stateCnt + 1 + ctx.scRuleToAdditionalCaseBranches.len).CaseLabel
            # don't forget to set the lexState.pos in the action block.
            let setLexState = quote do:
              `lexState`.pos = `pos`
            ctx.scRuleToAdditionalCaseBranches[key] = (caseLabel, newStmtList(
                setLexState, actions))
          newStmtList(quote do:
            `state` = `caseLabel`)
        else:
          # set the lexState pos before taking action.
          let setLexState = quote do:
            `lexState`.pos = `pos`
          let actions = replaceBeginState(curSc, ctx, state, rules[
              rule-1].actions)
          # it is already its own case branch for the accept actions
          newStmtList(setLexState, actions, newTree(nnkBreakStmt, newNimNode(nnkEmpty)))
      else:
        # no possible transition, go to last accepted state and pos
        newStmtList(quote do:
            `pos` = `lastAccPos`
            `state` = `lastAccStateAction`)
    if ifStmt.len == 0:
      caseStmt.add newTree(nnkOfBranch, newLit(src + ctx.labelOffset), actions)
    else:
      ifStmt.add newTree(nnkElse, actions)
      if rule >= 1:
        let updates = quote do:
          `lastAccPos` = `pos`
          `lastAccStateAction` = `caseLabel`
        let stmt = newStmtList(updates, ifStmt)
        caseStmt.add newTree(nnkOfBranch, newLit(src + ctx.labelOffset), stmt)
      else:
        caseStmt.add newTree(nnkOfBranch, newLit(src + ctx.labelOffset), ifStmt)
  ctx.labelOffset += a.stateCount

proc tryGetRules(c: var CodegenCtx; n: NimNode) =
  case n
  of Call([Ident(strVal: @startCondition), StmtList([all @callList])]):
    for call in callList:
      case call
      of Call([@lit is (kind: in nnkStrKinds), @stmt is StmtList()]):
        if startCondition notin c.scToIdx:
          c.scToRules.add @[]
          c.scToIdx[startCondition] = c.scToIdx.len
        let scIdx = c.scToIdx[startCondition]
        c.scToRules[scIdx].add Rule(startCondition: startCondition,
            regex: lit.strVal, actions: stmt)
      of CommentStmt():
        discard
      else:
        raise newException(BadDslInputException,
            "Unable to understand the start condition call with this AST repr " &
            astGenRepr call)
  of Call([@lit is (kind: in nnkStrKinds), @stmt is StmtList()]):
    let scIdx = c.scToIdx[initialStartCondition]
    c.scToRules[scIdx].add Rule(startCondition: initialStartCondition,
        regex: lit.strVal, actions: stmt)
  of CommentStmt():
    discard
  else:
    raise newException(BadDslInputException,
        "I do not understand the syntax of " & astGenRepr n)

template parseDsl(ctx: var CodegenCtx) =
  # for sanity, initial start condition always gets the lower ordinal.
  ctx.scToIdx[initialStartCondition] = 0
  ctx.scToRules.add @[]

  case sections
  of StmtList([all @calls]):
    for call in calls:
      tryGetRules(ctx, call)
  else:
    raise newException(BadDslInputException, "I do not understand the syntax " &
        astGenRepr sections)

  when defined(leximVerbose):
    for sc, idx in ctx.scToIdx:
      echo "sc=", sc, ", idx=", idx
    for sc, rules in ctx.scToRules:
      echo "sc=", sc, ",rules=", repr rules

template genDfa(ctx: var CodegenCtx) =
  for scIdx, rules in ctx.scToRules:
    block:
      # do the parsing here (duplicated in lexe) so we at least get
      # decent error message. doesn't work if I try to marshal a PRegExpr
      # to lexe. marshal feels very buggy and honestly I rather not
      # waste more brain cells and life dealing with it never working
      # the way you expect.
      var bigRe: PRegExpr = nil
      for i, rule in rules:
        let rex = parseRegExpr(rule.regex, findMacro,
                              {reNoCaptures, reNoBackrefs})
        rex.rule = i+1
        if bigRe.isNil: bigRe = rex
        else: bigRe = altExpr(bigRe, rex)

    # use 'lexe.exe' helper program in order to speedup lexer generation
    var res: seq[string] = @[]
    for rule in rules:
      res.add rule.regex
    let data = $$res
    let lexeOut = staticExec("lexe", input = data, cache = data)
    when defined(leximVerbose):
      echo "===lexe output (from regex construction)==="
      echo lexeOut
      echo "===end lexe output==="
    # for reasons unknown, can't catch exceptions from the call. it would
    # just crash the compiler. so, just rely on the previous lines to get
    # the hint that something went wrong.
    ctx.dfas.add to[DFA](lexeOut)
  when defined(leximVerbose):
    echo "===dfas==="
    echo repr ctx.dfas
    echo "===end dfas==="

template genCode(ctx: var CodegenCtx) {.dirty.} =
  # because states for individual DFA fall in the range(1..numStates)
  # but we have multiple DFA due to start conditions, so keep running count
  # of states added so far, incremented once for each dfa processed.
  ctx.labelOffset = 1 # 1 is the jammed state 
  # pos, oldPos, input, lexState intentionally exposed to user action code
  let pos = ident("pos")
  let oldPos = ident("oldPos")
  let input = ident("input")
  let lexState = ident("lexState")

  let curSc = genSym(nskVar, "curSc")
  let state = genSym(nskVar, "state")

  var stateCnt = 1 # account for jammed state
  for dfa in ctx.dfas:
    ctx.scStartOffsets.add stateCnt + dfa.startState
    stateCnt += dfa.stateCount
  when defined(leximVerbose):
    echo "===scStartOffsets==="
    echo $ctx.scStartOffsets
    echo "===end scStartOffsets==="

  let initScStartState = ctx.scStartOffsets[0]
  let caseStmt = newNimNode(nnkCaseStmt)
  caseStmt.add state
  # generate the action for the jammed state.
  caseStmt.add newTree(nnkOfBranch, newLit(1), quote do:
    # this is the default value of lastAccStateAction when there is no last accepted position.
    raise newException(JammedException, "state machine is jammed! there is no more possible match"))

  let isCstr = newLit(true) == isCString
  let lastAccStateAction = genSym(nskVar, "lastAccStateAction")
  let lastAccPos = genSym(nskVar, "lastAccPos")
  for sc, dfa in ctx.dfas:
    caseStmt.genMatcher(ctx, sc.StartCond, state, curSc, dfa, input, pos,
        ctx.scToRules[sc], isCstr, stateCnt, lastAccPos, lastAccStateAction, lexState)

  # generate the additional case branches for the actions
  # of accept states that have outgoing transitions.
  for caseLabel, actions in ctx.scRuleToAdditionalCaseBranches.values():
    caseStmt.add newTree(nnkOfBranch, newLit(caseLabel.int), newStmtList(
        actions, newTree(nnkBreakStmt, newNimNode(nnkEmpty))))

  let inputStrType = if isCstr: ident("cstring") else: ident("string")
  # far less brain damaging to write than the eqv in AST
  # this needs to be a closure iterator because otherwise it would inline, try
  # to generate identical goto labels in the same compilation unit and break
  # compilation
  let caseLabelUpper = stateCnt+ctx.scRuleToAdditionalCaseBranches.len
  # the syntax for iterator type is dumb, it is unhappy if you don't give the parameter a name
  # definitely one of the rough edges of the language.
  result = quote do:
    proc `procName`*(`input`: `inputStrType`): iterator(
        _: var `lexerStateTName`): `tokenTName` =
      result = iterator(`lexState`: var `lexerStateTName`): `tokenTName` {.closure.} =
        var `state` {.goto.}: range[1..`caseLabelUpper`]
        var `curSc`: range[1..`caseLabelUpper`] = `initScStartState`
        var `pos` = 0
        `lexState`.pos = 0
        while `pos` < `input`.len:
          let `oldPos` = `pos`
          var `lastAccPos` = `pos`
          var `lastAccStateAction` = 1
          `state` = `curSc`
          while true:
            `caseStmt`
  when defined(leximVerbose):
    echo repr result

macro match(isCString: bool; lexerStateTName, tokenTName, procName,
    sections: untyped): untyped =
  var ctx: CodegenCtx
  parseDsl(ctx)
  genDfa(ctx)
  genCode(ctx)

macro genStringMatcher*(name, body: untyped): untyped =
  case name:
  of BracketExpr([@procName is Ident(), @lexerStateT is Ident(),
      @tokenT is Ident()]):
    result = quote do:
      match(false, `lexerStateT`, `tokenT`, `procName`, `body`)
  else:
    raise newException(BadDslInputException,
        "Expected procName[<LexerStateType>, <TokenType>] but got " & repr name)

macro genCStringMatcher*(name, body: untyped): untyped =
  case name:
  of BracketExpr([@procName is Ident(), @lexerStateT is Ident(),
      @tokenT is Ident()]):
    result = quote do:
      match(true, `lexerStateT`, `tokenT`, `procName`, `body`)
  else:
    raise newException(BadDslInputException,
        "Expected procName[<LexerStateType>, <TokenType>] but got " & repr name)
