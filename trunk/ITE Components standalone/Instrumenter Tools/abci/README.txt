-------------
How to build   
-------------

These are what you need:

(1) Glasgow Haskell Compiler (GHC). The easiest way to install GHC is
by instally the Haskell Platform, see:

    http://hackage.haskell.org/platform/

(2) Utrecht University's Attribute Grammar system. It can be obtained
from Haskell's packages distribution center called Hackage:

   http://hackage.haskell.org/packages/hackage.html

(3) Various Haskell libraries on which ABCI depends on (see also below)

You need to do at least 1 and 2 first. Then, first install the "lib"
of ABCI, and then the tools.
 
1. For the lib, go to the directory 'lib', then do:
        cabal configure
        cabal install

Note: the install requires various Haskell packages to be installed
first. You can peek in the .cabal file there to see the
dependency. Else, the install program will indicate to you each time
it misses a package, so that you can just install the packages one by
one.

2. For the tool abciExample, go to derivedtools/abciExample, then do:
        cabal configure
        cabal install

3. For the tool abciLog, go to derivedtools/abciLog, then do as above

KNOWN ISSUES:

- On Linux there might be a problem with the chunk package related to
the conflict of the Prelude versions in the haskell98 and base
packages. The workaround is to grab the package chunk (cabal unpack)
and remove the dependency on haskell98 from the chunks.cabal and then
run cabal install on the updated file.

- Instrumenting an swf which has compiled with the option:

    static-link-runtime-shared-libraries=true

gives problem. Contrary to what Adobe decomentation, true is not the
default. The resulting code (even after empty instrumentation) throws
an index out of bound exception during the initial building of its GUI
components.
