/**
 * @author Kiran Lakhotia (k.lakhotia@cs.ucl.ac.uk)
 */

package eu.fittest.ucl.eventinf.api;

import java.io.File;

/**
 * Utility class for parsing a CSV file (which denotes an execution trace)
 * and converting each line into a ConcreteState states
 *
 */
public class Utils {
		
	public static void emptyDir(File dir, boolean delete) {
		if(dir != null){
			if(dir.exists()) {
				if(dir.isDirectory()) {
					File[] files = dir.listFiles();
					for(File f : files) {
						emptyDir(f, true);
					}
				}
				if(delete)
					dir.delete();
			}
		}
	}
	public static void emptyDir(File dir) {
		emptyDir(dir, true);
	}
	
}
