package eu.fittest.tloglib;

import java.util.*;
import java.io.*;
import java.nio.file.*;
import java.util.regex.*;

/**
 * Provide some file-related utility.
 *
 */

public class FileUtils {

	/**
	 * To write a string to a file. NO BUFFERING is done. The file is immediately
	 * closed after the writing.
	 * If the echo is set to true, the string will be echoed t the error console. 
	 */
	static public void fnbwrite(String fname, boolean echo, String s) { 
		   try{
			   // Create a handle for the file
			   FileWriter f = new FileWriter(fname,true);
			   f.append(s) ;
			   f.close() ;
			   if (echo) System.out.print(s) ; 
			   }
		   catch (Exception e){
			   System.err.println("Logger error: " + e.getMessage());
		    }
	}

	/**
	 * Just an -ln variant of fnbwrite.
	 */
	static public void fnbwriteln(String fname, boolean echo, String s) { 
		   fnbwrite(fname, echo, s + "\n") ;
	}
	
	/**
	 * Mapping file-names that are currently still opne to their buffers.
	 */
	static protected Map<String,BufferedWriter> fileBufferMap = new HashMap<String,BufferedWriter>() ;
	
	static public void fwrite(String fname, boolean echo, String s) { 
		
		BufferedWriter buf = fileBufferMap.get(fname) ;
		if (buf==null) {
			try {
  			  File file = new File(fname);
	          FileWriter f = new FileWriter(file) ;
			  buf = new BufferedWriter(f);
			  fileBufferMap.put(fname, buf) ; 
			} 
			catch (IOException e) {
			  System.err.println("Logger error: " + e.getMessage()) ;
			  e.printStackTrace();
			}	
		}
		try{ buf.write(s) ; 
		     if (echo) System.out.print(s) ; 
		}
	    catch (Exception e){
			   System.err.println("Logger error: " + e.getMessage());
	    }
    }
	
	static public void fwriteln(String fname, boolean echo, String s) { 
		   fwrite(fname, echo, s + "\n") ;
	}
	
	static public void fclose(String fname) {
		BufferedWriter buf = fileBufferMap.get(fname) ;
		if (buf==null) return ;
		try { buf.close() ; 
		      fileBufferMap.remove(fname) ;
		}
		catch (Exception e){
			   System.err.println("Logger error: " + e.getMessage());
	    }
	}
	
	
	static public String readFileToString(String fname) throws IOException {
		FileReader inputFile = new FileReader(fname);
	    BufferedReader bufferReader = new BufferedReader(inputFile);
	    String s = "" ;
	    String line = bufferReader.readLine() ;
	    if (line==null) return s ;
	    s += line ;
	    while ((line = bufferReader.readLine()) != null) s += "\n" + line ;
	    return s ;
	}
	
	/**
	 * Delete a file. 
	 */
	static public void removeFile(String fname) {
		try {
		 Files.delete(Paths.get(fname)) ;
		}
		catch (Exception e) { }
	}
	
	
	/**
	 * Return a list of file-names in a given directory that match the given
	 * reg-exp.
	 */
	static public List<String> getMatchingFileNames(String path, String regexp){
		// NOTE": when matching reg-exp on a string, do NOT use String's standard
		// s.matches(rexp) method. It stupidly tries to match the entire string s.
		// Known issue.. Sun leaves is as it is.
		Pattern pattern = Pattern.compile(regexp) ;
		Path dir = Paths.get(path) ;
		List<String> matches = new LinkedList<String>() ;
		try {
			DirectoryStream<Path> stream = Files.newDirectoryStream(dir) ;
		    for (Path entry: stream) {	
		    	String fn = entry.getFileName().toString() ;
		    	Matcher M = pattern.matcher(fn) ;
		        if (M.find()) 
		        	matches.add(fn) ;
		    }
		}
		catch(Exception e) { }
		return matches ;
	}

	/**
	 * Return ONE file-name in a given directory that matches the given
	 * reg-exp. If there are multiple matching candidates, the lexicograpghically
	 * maximum will be chosen. If there is no matching candidate, a null
	 * will  be returned.
	 */
	static public String getMatchingFileName1(String path, String regexp){
		List<String> files = getMatchingFileNames(path,regexp) ;
		if (files.size()==0) return null ;
		String max = files.get(0) ;
		for (String f : files) {
			if (max.compareTo(f) < 0) max = f ;
		}
		return max ;
	}
	
	public static void main(String[] args) {
		System.out.println("Content = " + getMatchingFileNames("d:/tmp","c(.*)txt")) ;
	}

}
