package eu.fittest.logging.trtransf;

// Used to test the main-tool TrT
public class TrTTest {

	public static void main(String[] args) {
		String[] args_ =
			   {   "--src", "D:/workshop/projects/fittest/fittestRepos/Software/UtrechtUniv/stdprojects/sturala/v1/transftest/test1",
				   "--class",
				   "D:/workshop/eclipseWorkspace/tlogTransTest_v1/bin/;D:/workshop/projects/fittest/fittestRepos/Software/UtrechtUniv/stdprojects/sturala/v1/build/loglib.jar",	
				   "--stat",
				   "--tag",
				   "--show", "*"
			   } ;
		TrT.main(args_) ;
	}
}
