# awesome-low-level-programming-languages

A curated list of **low level** programming languages primarily aimed and OS and game
programming.

**Excluded are languages relying on managed run-times and without manual memory management.**
(For less exclusionary lists check [ChessMax](https://github.com/ChessMax/awesome-programming-languages) or
[Wikipedia](https://en.wikipedia.org/wiki/List_of_programming_languages).)

Feel free to send pull-requests with additions and corrections.


Table of content

- [awesome-low-level-programming-languages](#awesome-low-level-programming-languages)
  - [ATS](#ats)
  - [Ada](#ada)
  - [Beef](#beef)
  - [C](#c)
  - [C++](#c-1)
  - [C2](#c2)
  - [C3](#c3)
  - [Carp](#carp)
  - [Cone](#cone)
  - [Crystal](#crystal)
  - [CSpydr](#cspydr)
  - [D](#d)
  - [Forth](#forth)
  - [Hare](#hare)
  - [Jai](#jai)
  - [Kit](#kit)
  - [Lobster](#lobster)
  - [Modula-2](#modula-2)
  - [Nim](#nim)
  - [Oberon](#oberon)
  - [Odin](#odin)
  - [Pascal (FreePascal)](#pascal-freepascal)
  - [Rust](#rust)
  - [Scopes](#scopes)
  - [V](#v)
  - [Val](#val)
  - [Vale](#vale)
  - [Vox](#vox)
  - [Zig](#zig)


Not yet summarized (pull requests welcome):

[Alumina](https://github.com/tibordp/alumina),
[Austral](https://github.com/austral/austral),
[Cakelisp](https://cakelisp.handmade.network/),
[Carbon](https://github.com/carbon-language/carbon-lang),
[eC](https://ec-lang.org/),
[Jiyu](https://jiyu.handmade.network/), 
[LitaC](https://github.com/tonysparks/litac-lang),
[Modula-3](https://en.wikipedia.org/wiki/Modula-3),
[Move](https://move-language.github.io/move/)
[Myr](https://myrlang.org/),
[Oak](https://github.com/adam-mcdaniel/oakc/)
[Roc](https://www.roc-lang.org/),
[Seed7](http://seed7.sourceforge.net/),
[Silk](https://github.com/AjayMT/silk),
[Sparrow](https://github.com/Sparrow-lang/sparrow),
[Swift](https://www.swift.org/),
[Terra](https://terralang.org/),
[Vala](https://wiki.gnome.org/Projects/Vala),


## ATS

* main: http://www.ats-lang.org/
* repo: https://github.com/ats-lang
* documentation:
  - http://ats-lang.sourceforge.net/DOCUMENT/INT2PROGINATS/HTML/book1.html
  - http://www.ats-lang.org/Documents.html
* discussion:
  - https://news.ycombinator.com/item?id=28214665
  - https://news.ycombinator.com/from?site=ats-lang.org
* implementation-language: ATS
* meta-programming: N/A
* backends: C
* major projects using the language: N/A
* syntax: functional style
* highlights:
  - proofs
  - dependent types
  - C code can be specified inline
* [https://pldb.com/languages/ats.html](pldb)

```
#include "share/atspre_staload.hats"
#include "share/atspre_staload_libats_ML.hats"

implement
main0() = println! ("Hello, world!")
```

```
fun fibc (n: int) : int = let
  fun loop(n: int, f0: int, f1: int): int =
    if n > 0 then loop(n-1, f1, f0+f1) else f0
  end of [loop]
in
  loop(n, 0, 1)
end // end of [fibc]
```

## Ada

* main: N/A
* repo: N/A
* documentation:
  - awesome-ada https://github.com/ohenley/awesome-ada
  - https://learn.adacore.com/
  - http://groups.umd.umich.edu/cis/course.des/cis400/ada/ada.html
* meta-programming: generics
* error-handlig: exceptions
* backends: gcc (gnat), several commerical implementations
* major projects using the language: numerous
* syntax: begin/end, type to the right of identifier
* highlights:
  - design by contract
* [https://pldb.com/languages/ada.html](pldb)
```
with Ada.Text_IO;

procedure Hello_World is
begin
   Ada.Text_IO.Put_Line ("Hello World");
end Hello_World;
```

```
function fibonacci(n : in integer) return integer is
 f1, f2, fib : integer;
 begin
    f1 := 0;
    f2 := 1;
    for i in 1..n loop
       fib := f1 + f2;
       f1 := f2;
       f2 := fib;
    end loop;
    return fib;
 end fibonacci;
```

## Beef

* main: https://www.beeflang.org/
* repo: https://github.com/beefytech/Beef/
* documentation:
  - awesome-beef https://github.com/Jonathan-Racaud/awesome-beef
* discussion:
  - https://news.ycombinator.com/item?id=21991382
* implementation-language: C++
* meta-programming: generics
* backends: LLVM
* major projects using the language: N/A
* syntax: curly braces, type to the left of identifier
* highlights:
  - inspired by C#
  - co-designed with IDE
  - windows centric development
* [https://pldb.com/languages/beef.html](pldb)

```
using System;

namespace Hello
{
    class Program
    {
        static void Main()
        {
            Console.WriteLine("Hello, world!");
        }
    }
}
```

```
N/A
```

## C

* main: N/A
* repo: N/A
* documentation:
  - https://github.com/inputsh/awesome-c
  - https://github.com/uhub/awesome-c
* meta-programming: pre-processor
* error-handling: magic return values by covention
* backends: LLVM, gcc, numerous others
* major projects using the language: numerous
* syntax: curly braces, type to the left of identifier
* highlights:
  - ubiquitous
  - often used as a backend
  - no namespaces
  - array to pointer auto conversion
  - no defer (or RAII) mechanism
  - lots of undefined / implementation defined behavior
* [https://pldb.com/languages/c.html](pldb)

```
#include <stdio.h>

int main() {
  printf("Hello World!");
}
```

```
int fib(int n) {
    int a = 0;
    int b = 1;
    for (int i = 0; i < n; i++) {
       int c = a + b;
       a = b;
       b = c;
    }
    return a;
}
```


## C++
* repo:
* documentation:
  - standard https://isocpp.org/std/the-standard
  - reference https://en.cppreference.com/w/
  - awesome-cpp https://github.com/fffaraz/awesome-cpp
  - AwesomePerfCpp https://github.com/fenbf/AwesomePerfCpp
* meta-programming: 
  - template meta programming
  - generics (types, functions)
  - comptime
  - macros
* backends: LLVM, gcc, numerous others
* major projects using the language: numerous
* syntax: curly braces, type to the left of identifier
* highlights:
  - large user base
  - several compilers 
  - large language (evolving)
  - slow compiles
* [https://pldb.com/languages/cpp.html](pldb)
  
```
#include <iostream>

int main() {
  std::cout << "Hello World!" << std::endl;
}

```

```
int fib(int n) {
    int a = 0;
    int b = 1;
    for (int i = 0; i < n; i++) {
       const int c = a + b;
       a = b;
       b = c;
    }
    return a;
}
```

## C2

* main: http://www.c2lang.org/
* repo: https://github.com/c2lang/
* documentation:
  - http://c2lang.org/site/
* discussion:
  - https://news.ycombinator.com/from?site=c2lang.org
* implementation-language: C++
* hello-world: http://www.c3-lang.org/firstproject/
* meta-programming: generics, N/A
* backends: LLVM
* major projects using the language: N/A
* syntax: curly braces, type to the left of identifier
* highlights:
  - modernized C
* [https://pldb.com/languages/c2.html](pldb)

```
module hello_world;

import stdio local;

public func i32 main(i32 argc, i8** argv) {
    printf("Hello World!\n");
    return 0;
}

```

```
func i32 fib(int n) {
    i32 a = 0;
    i32 b = 1;
    for (i32 i = 0; i < n; i++) {
       i32 c = a + b;
       a = b;
       b = c;
    }
    return a;
}
```

## C3

* main: http://www.c3-lang.org/
* repo: https://github.com/c3lang/c3c
* documentation:
  - http://www.c3-lang.org/compare/
* discussion:
  - https://news.ycombinator.com/item?id=27876570
* implementation-language: C
* hello-world: http://www.c3-lang.org/firstproject/
* meta-programming: generics, semantic macros
* backends: LLVM
* major projects using the language: N/A
* syntax: curly braces, type to the left of identifier
* highlights:
  - evolution of C
  - contracts
* [https://pldb.com/languages/c3.html](pldb)

```
module hello_world;

import std::io;

fn int main(int argc, char** argv) {
    io::println("Hello World!");
    return 0;
}

```

```
fn int fib(int n) {
    int a = 0;
    int b = 1;
    for (int i = 0; i < n; i++) {
       int c = a + b;
       a = b;
       b = c;
    }
    return a;
}
```

## Carp

* main: https://github.com/carp-lang/Carp
* repo: https://github.com/carp-lang/Carp
* documentation:
  - language guide: https://github.com/carp-lang/Carp/blob/master/docs/LanguageGuide.md
* discussion:
  - https://news.ycombinator.com/item?id=28875051
  - https://news.ycombinator.com/item?id=20368969
* implementation-language: Haskell
* meta-programming: generics
* backends: N/A
* major projects using the language:
* syntax: Lisp like
* highlights:
  - repl
  - ownership tracking
* [https://pldb.com/languages/carp.html](pldb)

```
import stdio

fn main():
  print <- "Hello world!"
```

```
N/A
```

## Cone

* main: https://cone.jondgoodwin.com/
* repo: https://github.com/jondgoodwin/cone
* documentation:
  - reference: https://cone.jondgoodwin.com/coneref/index.html
* discussion:
  - https://news.ycombinator.com/item?id=19565824
* implementation-language: C
* meta-programming: macros, generics (types, function, modules)
* error-handling: execptions + special syntax for same line rethrowing/default values
* backends: LLVM
* major projects using the language:
* syntax: optionally indentation sensitive, type to the right of identifier
* highlights:
  - [Regions and Lifetimes](https://cone.jondgoodwin.com/memory.html)
  - [Permissions and Actors](https://cone.jondgoodwin.com/concurrency.html)
  - [Variants, Structural Traits & Delegated Inheritance](https://cone.jondgoodwin.com/types.html)
  - [<-, with, and match](https://cone.jondgoodwin.com/complexval.html)

```
import stdio

fn main():
  print <- "Hello world!"
```

```
fn fib(n i64) i64:
  mut prior i64 = 0
  mut result i64 = 1
  while n-- > 0:
    prior, result = result, prior + result
  result

```

## Crystal

* main: https://crystal-lang.org/
* repo: https://github.com/crystal-lang/crystal
* note: use garbage collection but it is possible to strip out runtime (see [Lilith Kernel](https://github.com/ffwff/lilith))
* documentation:
  - https://crystal-lang.org/reference/
* discussion:
  - https://news.ycombinator.com/from?site=kitlang.org
* implementation-language: Crystal
* meta-programming: [Sophisticated macro system](https://crystal-lang.org/reference/1.5/syntax_and_semantics/macros/) 
* backends: LLVM
* major projects using the language: N/A
* syntax: 
* highlights:
  - similar to ruby 
* [https://pldb.com/languages/crystal.html](pldb)

```
puts "Hello World"
```

```
def fib(n)
  a = 0
  b = 1
  n.times do
    a += b
    a, b = b, a
  end
  a
end
```

## CSpydr

* main: https://github.com/Spydr06/CSpydr
* repo: https://github.com/Spydr06/CSpydr
* documentation: https://github.com/Spydr06/CSpydr/wiki
* implementation-language: C
* meta-programming: macros
* backends: x86_64 GNU Assembly, C (deprecated), LLVM (in progress)
* major projects using the language: CSpydr's standard library
* syntax: curly braces, type to the right of identifier with colon inbetween
* highlights:
  - namespaces
  - closures
  - automatic type inferrence
  - custom std lib
* [https://pldb.com/languages/cspydr.html](pldb)
```
import "std.csp";

fn main(): i32 {
    std::io::puts("Hello, World!");
    <- 0;
}
```

```
fn fib(n: i32): i32 {
    let a = 0;
    let b = 1;
    for 0 .. n {
        a + b |> (a = b, b = $);
    }
    <- a;
}
```

## D

* main: https://dlang.org/
* repo: https://github.com/dlang, https://github.com/ldc-developers/ldc, https://gdcproject.org/
* documentation:
  - spec https://dlang.org/spec/spec.html
  - overview https://dlang.org/comparison.html
* implementation-language: D
* meta-programming: generics
* backends: Custom (X86-64), LLVM, gcc
* major projects using the language: numerous
* syntax: curly braces, type to the left of identifier
* highlights:
  - large language
  - optional GC
* [https://pldb.com/languages/d.html](pldb)

```
import std.stdio;

void main() {
    writeln("Hello, World!");
}
```

```
N/A
```

## Forth

* main: N/A
* repo: N/A
* documentation:
  - http://www.forth.org/
* implementation-language: C, assembler, 
* meta-programming: N/A
* backends: Custom
* major projects using the language: N/A
* syntax: unique
* highlights:
 - concatenative programming style
 - many different flavors
 - very easy to implement
* [https://pldb.com/languages/forth.html](pldb)
* 
```
: HELLO ."Hello World " ;

```

```
: FIB ( x -- y ) RECURSIVE
	DUP 2 > IF DUP  1- RECURSE 
		   SWAP 2- RECURSE +  EXIT 
	     ENDIF 
	DROP 1 ;
```

## Hare

* main: https://harelang.org/
* repo: https://sr.ht/~sircmpwn/hare/
* documentation:
  - https://harelang.org/documentation/
* implementation-language: C
* error-handling: via tagged unions
* meta-programming: ???
* backends: QBE (restricted to 64bit: X86-64, AArch64)
* major projects using the language: 
  - https://sr.ht/~sircmpwn/himitsu/
  - https://sr.ht/~sircmpwn/helios/
* syntax: curly braces, type to the right of identifier
* [https://pldb.com/languages/hare.html](pldb)

```
use fmt;

export fn main() void = {
    fmt::println("Hello, world!")!;
};
```

```
N/A
```

## Jai

* main: N/A
* repo: N/A
* documentation:
  - inofficial https://inductive.no/jai/
  - inofffical https://github.com/BSVino/JaiPrimer/blob/master/JaiPrimer.md
  - Jonathan Blow YT channel https://www.youtube.com/user/jblow888/videos
  - Community Library https://github.com/Jai-Community/Jai-Community-Library
* implementation-language: C++
* meta-programming: macros
* backends: LLVM (?), Custom
* major projects using the language: N/A
* syntax: N/A 
* highlights:
  - compile time execution

```
N/A
```

```
N/A
```

## Kit

* main: https://www.kitlang.org/
* repo: https://github.com/kitlang/kit
* documentation:
* discussion:
  - https://news.ycombinator.com/from?site=kitlang.org
* implementation-language: Haskell
* meta-programming:
* backends: C
* major projects using the language: N/A
* syntax: curly braces, type to the left of identifier
* highlights:

```
function main() {
    printf("%s\n", "hello world!");
}
```

```
N/A
```

## Lobster

* main: http://aardappel.github.io/lobster/README_FIRST.html
* repo: https://github.com/aardappel/lobster
* discussion:
  - https://news.ycombinator.com/item?id=19567160
* implementation-language: C(++)
* meta-programming: generics, comptime evals 
* backends: bytecode VM, C++, WASM
* major projects using the language: N/A
* syntax: python inspired, white space sensitive
* memory management: compile time reference counting, lifetime analysis, borrow checker, cycle detection at program exit.
* highlights:
  - (flow sensitive) type inference

```
print "hello world"
```

```
def fibonacci(n: int):
  a = 0
  b = 1
  for (n) i:
    t = b
    b += a
    a = t
  return a  
```

## Modula-2

* main: https://en.wikipedia.org/wiki/Modula-2
* repo:
  - Gnu Modula 2 Frontend (gm2) https://gcc.gnu.org/git/?p=gcc.git;a=shortlog;h=refs/heads/devel/modula-2
* documentation:
  - https://www.nongnu.org/gm2/homepage.html
  - https://freepages.modula2.org/
  - https://www.modula-2.net/
* implementation-language: C (gm2)
* meta-programming: None
* backends: gcc (gm2)
* major projects using the language: [Medos-2](https://en.wikipedia.org/wiki/Lilith_(computer)#Operating_system)
* syntax: begin/end, type to the right of identifier
* highlights:
  - Evolution of Pascal
  - Modules
  - Co-routines
* [https://pldb.com/languages/modula-2.html](pldb)

```
MODULE Hello;
FROM STextIO IMPORT WriteString;
BEGIN
  WriteString("Hello World!")
END Hello.
```

```
PROCEDURE fib(n : INTEGER) : INTEGER;
VAR
    a, b, c : INTEGER;
BEGIN
    a := 0;
    b := 1;
 
    WHILE n > 0 DO
        c := a + b;
        a := b;
        b := c;
        DEC(n)
    END;
    
    RETURN a
END fib;

```

## Nim

* main: https://nim-lang.org/
* repo:
  - https://github.com/nim-lang/Nim
* documentation:
  - https://nim-lang.org/documentation.html
* discussions:
  - https://news.ycombinator.com/item?id=24800161
  - https://news.ycombinator.com/item?id=27165366
  - https://news.ycombinator.com/item?id=28916172
* implementation-language: Nim (self hosting)
* error-handing: exceptions
* meta-programming: macros manipulating the AST, generics, templates
* backends: JS, C
* memory management: ARC
* major projects using the language:
  - Nim (self hosting)
  - https://github.com/dom96/jester
  - https://github.com/karaxnim/karax
  - https://github.com/planety/prologue
* syntax: python inspired, white space sensitive
* highlights:
  - c interop
  - async implemented as library
  - case insensitve identifiers
* [https://pldb.com/languages/nim.html](pldb)

```
echo "Hello World"
```

```
proc fib(n: uint64): uint64 =
    if n <= 1: return n
    return fib(n - 1) + fib(n - 2)
```

## Oberon

* main: http://www.projectoberon.com
* note:
  - Oberon DOES use garbage collection but it has been used to implememt an influential OS of the same name
    so we include here.
  - There are serveral flavors of Oberon (
      Oberon-2 (1992),
      [Obereon-7 (2007)](http://people.inf.ethz.ch/wirth/Oberon/Oberon07.Report.pdf),
      [Active Oberon](http://cas.inf.ethz.ch/projects/a2/repository/raw/trunk/LanguageReport/OberonLanguageReport.pdf),
      [Oberon+](https://github.com/oberon-lang) )
* repo:
  - Oberon+ https://github.com/oberon-lang/
* documentation:
  - http://www.projectoberon.com
* discussion:
  - https://news.ycombinator.com/item?id=21557057
* implementation-language: Oberon
* meta-programming: None
* backends: Custom
* major projects using the language: Oberon-OS
* syntax: begin/end, type to the right of identifier
* highlights:
  - evolution of Pascal and Modula-2
  - deliberate small language
* [https://pldb.com/languages/oberon.html](pldb)

```
MODULE Hello;
         IMPORT Oberon, Texts;
  VAR W: Texts.Writer;
  
  PROCEDURE World*;
  BEGIN
    Texts.WriteString(W, "Hello World!");
    Texts.WriteLn(W);
    Texts.Append(Oberon.Log, W.buf);
  END World;

BEGIN
  Texts.OpenWriter(W);
END Hello.
```

```
PROCEDURE fib(VAR n: INTEGER) : INTEGER;
  VAR
    a, b, c, i : INTEGER;
  BEGIN 
    a := 0;
    b := 1;
    FOR i := 1 TO n DO
      c := a + b;
      a := b
      b := c
    END;
    RETURN a;
END fib;
```

## Odin

* main: https://odin-lang.org/
* repo: https://github.com/odin-lang/Odin
* documentation:
  - spec https://odin-lang.org/docs/spec/
  - https://www.youtube.com/channel/UCUSck1dOH7VKmG4lRW7tZXg
* discussion:
  - https://news.ycombinator.com/item?id=22199942
* implementation-language: C++
* meta-programming: generics
* error-handling: "go-style" via multiple return values
* backends: LLVM
* major projects using the language: [EmberGen](https://jangafx.com/software/embergen/)
* syntax: curly braces, type to the right of identifier
* highlights:
  - implcit context parameter
* [https://pldb.com/languages/odin.html](pldb)

```
package main

import "core:fmt"

main :: proc() {
  fmt.println("Hello World!")
}

```

```
fibonacci :: proc(n: int) -> int {
  switch {
  case n < 1:
    return 0
  case n == 1:
    return 1
  }
  return fibonacci(n-1) + fibonacci(n-2)
}

```

## Pascal (FreePascal)


* main: https://www.freepascal.org/
* repo: https://github.com/fpc
* documentation: https://www.freepascal.org/docs.html
* note: includes support for Delphi language extensions (classes, etc.)
* discussion:
  - https://news.ycombinator.com/from?site=freepascal.org
* implementation-language: Pascal
* meta-programming: generics
* backends: Custom(X86 (32+64), PowerPC (32+64), Sparc, ARM (32+64))
* major projects using the language
* syntax: begin/end, type to the right of identifier
* highlights:
* [https://pldb.com/languages/pascal.html](pldb)

```
program Hello;
begin
  writeln ('Hello, world.');
  readln;
end.
```

```
function fibonacci(const n: integer): integer;
var
  a, b, c, i: integer;
begin
  a := 0;
  b := 1;
  for i := 1 to n do
  begin
    c := a + b;
    a := b;
    b := c;
  end;
  fibonacci := a
end;
```

## Rust

* main: https://www.rust-lang.org/
* repo: https://github.com/rust-lang
* documentation:
  - https://doc.rust-lang.org/book/
* implementation-language: Rust 
* meta-programming: 
  - hygienic macros
  - generics/traits
  - comptime
* backends: LLVM
* major projects using the language: numerous (including large parts of [Firefox](https://www.mozilla.org/en-US/firefox/new/))
* syntax: curly braces, type to the right of identifier
* highlights:
  - memory safety focus (ownership semantics)
  - immutable by default
  - bare metal programming via `no_std` environment
  - steep learning curve
  - large language
  - slow compiles
* [https://pldb.com/languages/rust.html](pldb)

```
fn main() {
    println!("Hello World!");
}
```

```
fn fib(n: u8) -> u64 {
  let mut prev: u64 = 0;
  let mut curr: u64 = 1;
  for _ in 1..n {
      let next = prev + curr;
      prev = curr;
      curr = next;
  }
  curr
}
```  

## Scopes

* main: https://scopes.readthedocs.io/en/latest/
* repo: https://hg.sr.ht/~duangle/scopes
* documentation:
* discussion:
  - https://news.ycombinator.com/item?id=19830860
  - https://news.ycombinator.com/item?id=16603134
* implementation-language: C++
* meta-programming:
* backends: LLVM
* major projects using the language
* syntax: indentation sensitive
* highlights:
  - on-line compiler

```
print "hello world"

```

```
fn fib (n)
  loop (a b = 0 1)
    if (b < n)
      repeat b (a + b)
    else
      break b

```

## V

* main: https://vlang.io/
* repo: https://github.com/vlang
* documentation:
  - https://github.com/vlang/v/blob/master/doc/docs.md
* discussion:
  - https://news.ycombinator.com/from?site=vlang.io
* implementation-language: V
* error-handling: special case of optional types, dedicated syntax 
* meta-programming: generics
* backends: C, LLVM
* major projects using the language: N/A
* syntax: curly braces, type to the right of identifier
* highlights:
  - go derived syntax
  - immutable by default
  - some confusion around memory-allocators and GC ("autofree")
* [https://pldb.com/languages/v.html](pldb)
```
fn main() {
  println('Hello, World!')
}
```

```
fn fn(n int) int {
  mut a := 0
  mut b := 1
  for _ in 0 .. n {
    c := a + b
    a = b
    b = c
  }
  return a
}
```

## Val

(Needs more work - pull requests welcome)

* main: https://github.com/val-lang
* repo: https://github.com/val-lang/val
* documentation: https://github.com/val-lang/val/wiki/Val's-Language-Guide
* discussion: https://news.ycombinator.com/item?id=31788527
* implementation-language: Swift
* meta-programming:
* backends:
* major projects using the language
* syntax: curly braces, type to the right of identifier
* highlights: value semantics

## Vale

* main: https://vale.dev/
* repo: https://github.com/ValeLang/Vale
* documentation:
  - introduction https://vale.dev/guide/introduction
  - https://www.reddit.com/r/vale/
* discussion:
  - https://news.ycombinator.com/item?id=16603134
  - https://news.ycombinator.com/from?site=vale.dev
* implementation-language: Vale, Scala
* meta-programming: generics
* backends: LLVM
* major projects using the language: N/A
* syntax: curly braces, type to the right of identifier
* highlights:
  - immutable by default
  - ownership semantics

```
fn main() export {
  println("Hello world!");
}
```

```
N/A
```

## Vox

* main: https://github.com/MrSmith33/vox
* repo: https://github.com/MrSmith33/vox
* documentation: N/A
* discussion: N/A
* implementation-language: D
* meta-programming: generics
* backends: Custom (X86-64)
* major projects using the language: N/A
* syntax: curly braces, type to the left of identifier
* highlights:
  - AOT + JIT

```
enum u32 stdin  = 0;
enum u32 stdout = 1;
enum u32 stderr = 2;

@extern(syscall, 60)
void exit(i32 error_code);

@extern(syscall, 1)
void sys_write(u32 fd, u8* buf, u64 count);

void write(u32 fd, u8[] data) { sys_write(fd, data.ptr, data.length); }

void main(u8* lpCmdLine, i32 nShowCmd) {
	write(stdout, msg);
	exit(0);
}
```

```
N/A
```
  
## Zig

* main: https://ziglang.org
* repo: https://github.com/ziglang/zig
* documentation:
  - https://ziglang.org/documentation/master/
* discussion:
  - https://news.ycombinator.com/item?id=25797025
  - https://news.ycombinator.com/item?id=28458713
  - https://news.ycombinator.com/item?id=27399876
  - https://news.ycombinator.com/from?site=ziglang.org
* implementation-language: C++, Zig
* meta-programming: comptime (including types)
* error-handling: compiler built-in type, dedicated syntax 
* backends: LLVM, custom
* major projects using the language: N/A
* syntax: curly braces, type to the right of identifier
* highlights:
  - small language
  - testing built into the language
  - variables must be declared via `const` (immutable) or `var` (mutable)
  - no invisible control-flow
  - defer/errdefer
* [https://pldb.com/languages/zig.html](pldb)

```
const std = @import("std");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("Hello, {s}!\n", .{"world"});
}

```

```
fn fibonacci(n: u32) u32 {
   var a : u32 = 0;
   var b : u32 = 1;
   var i : u32 = 0
   while (i < n) : (i += 1) {
      const c : u32 = a + b;
      a = b;
      b = c;
   }
   return a;
}
```

<!--
## Template

* main:
* repo:
* documentation:
* discussion:
* implementation-language:
* meta-programming:
* backends:
* major projects using the language
* syntax:
* highlights:

```
HELLO WORLD SNIPPET
```

```
FIBONACCI SNIPPET
```
-->
