Below I list tools and libraries that should be built as part of
"FITTEST distribution". Note that they are needed to support
automation in ITE, but you cannot just literally plug them it. We will
have to discuss ITE integration separately.


-----------------------
abci related stuffs
-----------------------

Where: eu.fittest.uu\tools\abci\VERSION

* It contains README, with build instruction.
* Written in Haskell, so you need Haskell platform

Which stuffs you should build:

(a) abciLib

    VERSION\lib

    How to build: see the README above. But if you are impatient, do
    this in VERSION\lib:

    cabal configure
    cabal install

    
(b) abciLog

    VERSION\derivedtools\abciLog

    How to build: see the README above. But if you are impatient, do
    this in  VERSION\derivedtools\abciLog:

    cabal configure
    cabal install

-----------------------
haslog
-----------------------

Where: eu.fittest.uu\tools\Hashlog\VERSION
 
* It contains README
* Written in Haskell, so you need Haskell platform

* How to build: see README. If you are impatient, do this in the above
  directory:

    cabal configure
    cabal install


-----------------------
daikon-utitility
-----------------------

(a) declmerge

Where: eu.fittest.uu\tools\daikonUtils\haskell\declmerge\VERSION

* No README yet
* Written in Haskell, so you need Haskell platform
* To build, do this in the above directory:

    cabal configure
    cabal install

(b) dtracesplit

Where eu.fittest.uu\tools\daikonUtils\haskell\dtraceSplit\VERSION

* No README yet
* Written in Haskell, so you need Haskell platform
* To build, do this in the above directory:

    cabal configure
    cabal install

-----------------------
Pattern inferencing related stuffs
-----------------------

(a) lopi

Where: eu.fittest.uu\tools\PatternInferencer\lopi

* No README yet
* Written in Haskell, so you need Haskell platform
* To build, do this in the above directory:

    cabal configure
    cabal install


-----------------------
Daikon
-----------------------

Well, not our tool. You get it here:  http://groups.csail.mit.edu/pag/daikon/

It has compiled java jars; so no need to build.