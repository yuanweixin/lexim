# lexim
Lexer generation and regex implementation for Nim. A fork of https://github.com/Araq/lexim. Intended to be used in my toy compiler project. 


# details 

Lexim implements fsa based scanner, using the classic Regex -> NFA -> DFA -> minimization. Regex parsing uses recursive descent. The NFA -> DFA with subset construction, DFA minimization using Hopcroft's algorithm. The code generated is a computed goto that is embedded in the caller code, which is more efficient than simulating the DFA separately. 

No buffering scheme is given for the scanner. Since the toy compiler I write with this runs on modern desktops, memory is abundant and it is just added complexity to throw in buffering for a non-problem. No longer have to deal with multi-line tokens (if using a line-oriented buffer), or dealing with the possibility that a single token can exceed your buffer size. 

Lexim requires a 'lexe' helper exe that is used by 'lexim'.
Compile via ``nim c lexe`` and then you can run the example
via ``nim c ex1.nim``. It isn't strictly required, but does speed up the compilation process by offloading the expensive DFA construction to a separate process instead of doing it in the VM (for more complicated regex it generates the `interpretation requires too many iterations` error; fwiw it could be mitigated with compiler option `maxLoopIterationsVM:N` but that's ugly). 

# usage 

Something like this: 
```nim
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
```

