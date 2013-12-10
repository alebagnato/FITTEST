To build this you need to install Haskell Platform. It is here :

    http://hackage.haskell.org/platform/
    
It supports Windows, Mac, and Linux. The platform comes with a build utility called Cabal. To build just go to the directory where you have unzipped or downloaded this package. This should be the same directory as this README. Then just do:

   cabal install
   
It should then produce an executable called haslog.exe (in windows). It is probably placed in a diretory "dist" it creates. Then do:

   haslog.exe --help
   
To compress a raw log do:  haslog.exe --compress file.log
To export a compressed log to XML do: hashlog.exe --toxml file.log