ITE Release refactored by urko.rueda@UPVLC

******************Building the FITTEST ITE Update Site*********************************
Requirements:
	- JDK 1.6 (not a JDK 7 because of issues when signing the Eclipse plugins)
	- Maven 3.0.3 in the environment path (http://maven.apache.org/download.html)
	- Ant 1.8.3 in the environment path (http://ant.apache.org/bindownload.cgi)
	- Haskell platform (http://hackage.haskell.org/platform/)
	- PHP interpreter 5.4.0 (for Windows http://windows.php.net/)
	Third party tools:
		- dk.brics.automaton 	(check requirements/thirdpartytools/dk.brics.automaton)		
		- nt.flotsam.xeger 		(check requirements/thirdpartytools/nl.flotsam.xeger)
		- netx 					(check requirements/thirdpartytools/netx)
		- flashselenium 		(check requirements/thirdpartytools/flashselenium)
		- daikon 				(check requirements/thirdpartytools/daikon)
		- haslog				(check requirements/thirdpartytools/haslog)
		- evosuite-fittest 		(check requirements/thirdpartytools/evosuite-fittest)
		- weka 					(check requirements/thirdpartytools/nz.ac.waikato.cms.weka)
		- cluster-weka 			(check requirements/thirdpartytools/cluster-weka)
	Dependencies:
		- iteagent_1.0.1		(check requirements/dependencies/iteagent_1.0.1)
		- IMU 					(check requirements/dependencies/IMU)
		- xinputminer 1.0.0		(check requirements/dependencies/xinputminer_1.0.0)
					  1.0.1		(check requirements/dependencies/xinputminer_1.0.1)
		- fbkflex 1.0.0			(check requirements/dependencies/fbkflex_1.0.0)
		- log2efsm_1.0.1		(check requirements/dependencies/log2efsm_1.0.1)
		- PHP Maven plugin		(copy  requirements/dependencies/phpmaven)
------------------------------------------------------------------
To prevent from OutofMemory exception when building and assuming you have at least 2GB of RAM, run setenv.bat
You may need to increase the value of the -Xmx option and also consider to add something like '-Xms512m -XX:MaxPermSize=512m'
-------------------------------------------------------------------
Then, on the command prompt, execute the following command in the ITE Release root:
	>mvn install -DskipTests -Psign-release
If you are not connected to the Internet, try:
	>mvn install -o
You should obtain the ITE release at: eu.fittest.softeam/eu.fittest.eclipse/eu.fittest.eclipse.updatesite/eu.fittest.eclipse.updatesite-x.x.x.zip
------------------------------------------------------------------
If you get an error regarding 'profiling' when building the eu.fittest.uu.asic and eu.fittest.uu.hashlog projects (written in Haskell), you should resinstall the required Haskell libraries (specified in the error message) with the profiling opton (-p), e.g for the 'deepseq' library:
	>cabal install -p --reinstall deepseq
------------------------------------------------------------------
If you get an error of unresolvable php plugins, go into eu.fittest.phplogger/dependencies and copy the contents to your local Maven repository
------------------------------------------------------------------
To sign the eclipse update site, run
	>mvn install -Psign-release
------------------------------------------------------------------
To skip test add the following argument
	-Dmaven.test.skip=true
-----------------------------------------------------------
If the FITTEST certificate is out of date, generate a new keystore by executing eu.fittest.softeam/conf/keystore.bat
Make sure that keytool provided by Java JDK is in the environment path

*****************Integration in Eclipse 3.6.2*********************************************
Install:
	-m2e connector for jaxb2 from http://bitstrings.github.com/m2e-connectors-p2/releases
	-m2e - Maven Integration for Eclipse from http://download.eclipse.org/technology/m2e/releases