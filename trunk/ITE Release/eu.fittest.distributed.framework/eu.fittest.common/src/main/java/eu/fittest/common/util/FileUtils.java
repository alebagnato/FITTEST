package eu.fittest.common.util;

import java.io.File;

public class FileUtils {
	
	public static boolean deleteDir(File dir) {
	    if (dir.isDirectory()) {
	        for (File file: dir.listFiles()) {
	            boolean success = deleteDir(file);
	            if (!success) {
	                return false;
	            }
	        }
	    }

	    // The directory is now empty so delete it
	    return dir.delete();
	}
}
