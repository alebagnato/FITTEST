******************Building the FITTEST ITE Update Site*********************************

Requirements:
JDK 1.6 (not a JDK 7 because of issues when signing the Eclipse plugins)
Maven 3.0.3 in the environment path (http://maven.apache.org/download.html)
Ant 1.8.3 in the environment path (http://ant.apache.org/bindownload.cgi)
Haskell platform (http://hackage.haskell.org/platform/)
dk.brics.automaton (check thirdpartytools/dk.brics.automaton)
nt.flotsam.xeger (check thirdpartytools/nl.flotsam.xeger)
netx (check thirdpartytools/net.sourceforge.netx)
PHP interpreter 5.4.0 (for Windows http://windows.php.net/)

------------------------------------------------------------------
To prevent from OutofMemory exception when building and assuming you have at least 2GB of RAM, run setenv.bat
You may need to increase the value of the -Xmx option and also consider to add something like '-Xms512m -XX:MaxPermSize=512m'
-------------------------------------------------------------------
Then, on the command prompt, execute the following command in eu.fittest.eclipse/eu.fittest.eclipse.updatesite:
>mvn package
If you are not connected to the Internet, try:
>mvn package -o
------------------------------------------------------------------
If you get an error regarding 'profiling' when building the eu.fittest.uu.asic and eu.fittest.uu.hashlog projects (written in Haskell), you should resinstall the required Haskell libraries (specified in the error message) with the profiling opton (-p), e.g for the 'deepseq' library:
>cabal install -p --reinstall deepseq
------------------------------------------------------------------
To sign the eclipse update site, run
>mvn package -Psign-release
------------------------------------------------------------------
To skip test add the following argument
 -Dmaven.test.skip=true
-----------------------------------------------------------
If the FITTEST certificate is out of date, generate a new keystore by executing conf/keystore.bat
Make sure that keytool provided by Java JDK is in the environment path

*****************Integration in Eclipse 3.6.2*********************************************
Install:
-m2e connector for jaxb2 from http://bitstrings.github.com/m2e-connectors-p2/releases
-m2e - Maven Integration for Eclipse from http://download.eclipse.org/technology/m2e/releases