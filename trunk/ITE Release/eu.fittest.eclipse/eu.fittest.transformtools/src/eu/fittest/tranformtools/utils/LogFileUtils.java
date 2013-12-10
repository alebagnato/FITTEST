package eu.fittest.tranformtools.utils;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;

import eu.fittest.modelInference.logConverter.Converter;
import eu.fittest.modelInference.logConverter.Utility;
import eu.fittest.tranformtools.Activator;

public class LogFileUtils {
	
	/**
	 * Check if log files are in XML
	 * @param inputFolder
	 * @return
	 */
	public static boolean isXMLFormat(String inputFolder){
		Utility utils=new Utility();
		File[] fileList=utils.getFileList(inputFolder);
		List<File> fileToBeChecked = new ArrayList<File>();
		
		if (fileList.length == 0) return false;
		
		if (fileList.length > 3){
			Random ranSelector = new Random();
			File f = fileList[ranSelector.nextInt(fileList.length)];
			if (f != null)
				fileToBeChecked.add(f);
			f = fileList[ranSelector.nextInt(fileList.length)];
			if (f != null)
				fileToBeChecked.add(f);
		} else {
			for (File f : fileList){
				if (f!=null)
					fileToBeChecked.add(f);
			}
		}
		
		for (File f : fileToBeChecked){
			if (f.isFile()){
				try {
					BufferedReader reader = new BufferedReader(new FileReader(f));
					String firstLine = reader.readLine();
					reader.close();
					if (!firstLine.startsWith("<?xml")){
						return false;
					}
					
				} catch (FileNotFoundException e) {
					return false;
				} catch (IOException e) {
					return false;
				}
			}
		}
		
		return true;
	}
	
	/**
	 * Convert XML format log to FBK legacy format
	 * 
	 * @param inputFolder
	 * @return tempFolder
	 */
	public static String convert2FBKFormat(String inputFolder, IPath outputFolder){
		Converter uu2fbkLogs=new Converter();
		Utility utils=new Utility();
		File[] fileList=utils.getFileList(inputFolder);
		for (int i = 0; i < fileList.length; i++) {
			if (fileList[i] != null) {
				String outputFileName = outputFolder.append(fileList[i].getName()).toOSString();
				if (fileList[i].isFile()) uu2fbkLogs.convert(fileList[i],outputFileName);
			}
		}
		return outputFolder.toOSString();
	}
	
	/**
	 * Convert ITE XML format to FBK legacy format
	 * 
	 * @author cdnguyen
	 * @param sourceFolder
	 * @param targetFolder
	 * @return
	 */
	public static String convert2FBKFormat(IPath sourceFolder, IPath targetFolder) {
		return convert2FBKFormat(sourceFolder.toFile().getAbsolutePath(), targetFolder);
	}

	
	
	/**
	 * Get a temporary from the plugin temprary folder or from system temp dir
	 * 
	 * @author cdnguyen
	 * @return
	 */
	public static IPath prepareTempFolder(){
		IPath tmpFolder = Activator.getDefault().getStateLocation().append("tmp"); //
		
		File f = new File(tmpFolder.toOSString());
		try {
			if (f.exists()){
				deleteDir(f); // empty directory
			}
			f.mkdirs();
			
			return tmpFolder;
		} catch (Exception e) {
			
			String sysTempDir = System.getProperty("java.io.tmpdir") + File.separator + "ite_tmp";
			f = new File(sysTempDir);
			if (f.exists()){
				deleteDir(f); // empty directory
			}
			f.mkdirs();
			
			return new Path(sysTempDir);
		}
	}

	/**
	 * Empty a directory
	 * @param dir
	 * @return
	 */
	public static boolean deleteDir(File dir) {
	    if (dir.isDirectory()) {
	        String[] children = dir.list();
	        for (int i=0; i<children.length; i++) {
	            boolean success = deleteDir(new File(dir, children[i]));
	            if (!success) {
	                return false;
	            }
	        }
	    }

	    // The directory is now empty so delete it
	    return dir.delete();
	}
	
	
	/**
	 * Copy a folder to a target folder
	 * @param fin
	 * @param fout
	 * @throws Exception
	 */
	public static void copyFolder(File fin, File fout) throws Exception {
		fout.mkdir();
		String[] children = fin.list();
		if (children == null) {
			// Either dir does not exist or is not a directory
		} else {
			for (int p = 0; p < children.length; p++) {
				File f = new File(fin + File.separator + children[p]);
				File f1 = new File(fout + File.separator + children[p]);
				if (f.isDirectory())
					copyFolder(f, f1);
				else
					copyFile(f, f1);
			}
		}
	}
	 
	/**
	 * Copy a file to a target file
	 * 
	 * @param inputFile
	 * @param outputFile
	 */
	 public static void copyFile(File inputFile, File outputFile) {
		int bufferSize = 4 * 1024;
		try {
			FileInputStream in = new FileInputStream(inputFile);
			FileOutputStream out = new FileOutputStream(outputFile);
			int readSize;
			byte buff[] = new byte[bufferSize];
			while ((readSize = in.read(buff)) != -1)
				out.write(buff, 0, readSize);
			in.close();
			out.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	 
	/**
	 * copy a IFile to a target folder
	 * 
	 * @author cdnguyen
	 */
	public static void copyFile(IFile file, IPath targetFolder){
		File inputFile = file.getLocation().toFile();
		File outputFile = new File(targetFolder.toOSString() + File.separator + file.getName());
		copyFile(inputFile, outputFile);
	}

}
