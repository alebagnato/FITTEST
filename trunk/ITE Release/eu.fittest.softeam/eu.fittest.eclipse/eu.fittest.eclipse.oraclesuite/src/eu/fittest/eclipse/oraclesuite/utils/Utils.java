package eu.fittest.eclipse.oraclesuite.utils;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;

public class Utils {
	
	private static String fieldDelimiter = ";";

	public static  boolean isPrepped(File logFolder) {
		List<File> logfiles;
		try {
			logfiles = collectFiles(logFolder,".log", "");

			for (File logFile : logfiles) {

				File  flox = new File (logFolder + File.separator + getBaseName (logFile.getName()) + ".lox") ;
				File  fdic = new File (logFolder + File.separator + getBaseName (logFile.getName()) + ".dic") ;

				if(!(flox.exists()) || !(fdic.exists())) {		      
					return false ;
				}
			}
			return true ;

		} catch (CoreException e) {
			e.printStackTrace();
			return false;
		}
	}
	
	public static String getFieldDelimiter(){
		return fieldDelimiter;
	}
	
	public static void copyFile(String source, String dest) throws IOException {
		copyFile(new File(source), new File(dest));	
	}
	
	public static void copyFile(File source, File dest) throws IOException {
	    InputStream is = null;
	    OutputStream os = null;
	    try {
	        is = new FileInputStream(source);
	        os = new FileOutputStream(dest);
	        byte[] buffer = new byte[1024];
	        int length;
	        while ((length = is.read(buffer)) > 0) {
	            os.write(buffer, 0, length);
	        }
	    } finally {
	        is.close();
	        os.close();
	    }
	}
	
	public static void moveFile(File source, File dest) throws IOException {
		copyFile(source, dest);
		source.delete();
	}
	
	public static void moveFile(String source, String dest) throws IOException {
		moveFile(new File(source), new File(dest));	
	}

	public static void copyFiles(List<File> sources, File destFolder) throws IOException {
		for(File source : sources){
			copyFile(source, new File(destFolder.getAbsolutePath() + File.separator + source.getName()));
		}
	}
	
	public static void copyFiles(List<File> sources, String destFolder) throws IOException {
		for(File source : sources){
			copyFile(source, new File(destFolder + File.separator + source.getName()));
		}
	}
	
	public static List<String> getFileNames(File folder, String fileExtension){
		List<String> result = new ArrayList<String>();
		List<File> collectFiles;
		try {
			collectFiles = collectFiles(folder, fileExtension, "");

			for(File collectFile : collectFiles){
				result.add(collectFile.getName());
			}


		} catch (CoreException e) {
			e.printStackTrace();
		}
		return result ;
	}
	
	public static List<String> getFileNames(String folder, String fileExtension){
		return getFileNames(new File(folder), fileExtension) ;
	}
	
	public static List<File> collectFiles(File folder, String fileExtension) throws CoreException{
		return collectFiles(folder, fileExtension, "");
	}
	
	public static List<File> collectFiles(String folder, String fileExtension) throws CoreException{
		return collectFiles(new File(folder), fileExtension, "");
	}

	public static String getBaseName(String fileName) {
		int i = fileName.lastIndexOf('.');
		return fileName.substring(0, i);
	}
	
	public static String getExtName(String fileName) {
		return fileName.substring(fileName.indexOf("."));
	}



	/**
	 * List recursively all files that have the extension and prefix
	 * 
	 * @param folder, extension, prefix
	 * @return List<File> of files
	 * @throws CoreException 
	 * 
	 * @author ebrosse
	 * 
	 */
	public static List<File> collectFiles(File folder, String fileExtension, String fileNamePrefix) throws CoreException{
		List<File> fileList = new ArrayList<File>();
		if (folder != null && fileExtension != null){
			for (File r : folder.listFiles()){
//				if (r.isDirectory()){
//					fileList.addAll(collectFiles(r, fileExtension, fileNamePrefix));					
//				} else{
					boolean checkExtension = false;
					if (fileExtension != null && r.getName().endsWith(fileExtension)){
						checkExtension = true;
					} else if (fileExtension == null){
						checkExtension = true;
					}
					boolean checkPrefix = false;
					if (fileNamePrefix != null && r.getName().startsWith(fileNamePrefix)){
						checkPrefix = true;
					} else if (fileNamePrefix == null){
						checkPrefix = true;
					}
					if (checkExtension && checkPrefix){
						fileList.add((File) r);
					}
//				}
			}
		}
		return fileList;
	}

}
