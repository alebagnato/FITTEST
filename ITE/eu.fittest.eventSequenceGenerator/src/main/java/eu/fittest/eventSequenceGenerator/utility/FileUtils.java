package eu.fittest.eventSequenceGenerator.utility;

import java.io.*;
import java.util.Vector;

/**
*
* @author Alessandro
*
*/
public class FileUtils {

		BufferedWriter outF=null;

		public void createFile(String fileName){
			try{
				outF = new BufferedWriter(new FileWriter(fileName));
				outF.flush();
			}catch(Exception e){
				try{
				outF = new BufferedWriter(new FileWriter(fileName));
				outF.flush();
					
				}catch(Exception e2){ e2.printStackTrace();}
			}
		}
		
		public int createORappend_File_(String fileName){
			try{
				File file =new File(fileName);
				if(!file.exists()){
					createFile(fileName);
					return 1;
	    		}else {
	    			outF = new BufferedWriter(new FileWriter(fileName,true));
	    			outF.flush();
	    			return 0;
	    		}				
			}catch(Exception e){
				try{
					createFile(fileName);
					return 2;
				}catch(Exception e2){ 
					e2.printStackTrace();
					return -1;
					}
			}
		}
		
		/**
		 * 
		 */
		public void writeInFile(String line){
			try{
				outF.write(line+"\r\n");
				outF.flush();
			}catch(Exception e){
					e.printStackTrace();
			}
		}
		/**
		 * 
		 */
		public void writeInFile_no_NL(String line){
			try{
				outF.write(line);
				outF.flush();
			}catch(Exception e){
					e.printStackTrace();
			}
		}
		/**
		 * 
		 */
		public void closeFile(){
			try{
				outF.close();
			}catch(Exception e){
					e.printStackTrace();
			}
		}
		

		/*
		 * It returns the list of files
		 */
		public File[] getFileList(String folderPath){
			
			File folder = new File(folderPath);
			File[] listOfFiles = folder.listFiles();
			
			File[] listOfFilesOedered=new File[listOfFiles.length];
			String fname="";
			int index=0;
				
			for (int i = 0; i < listOfFiles.length; i++) {
				if (listOfFiles[i].isFile()) {
					fname=listOfFiles[i].getName();
					//if ((fname.startsWith("log_"))&&(fname.endsWith(".txt"))) {
					if (fname.startsWith("log_")) {
						listOfFilesOedered[index]=listOfFiles[i];
						index++;
					}
				}
			}
			
			Vector<File> tmpFiles=new Vector<File>();
			for (int i = 0; i < listOfFilesOedered.length; i++) {
				if (listOfFilesOedered[i]!=null) tmpFiles.add(listOfFilesOedered[i]);
			}
			
			listOfFilesOedered=new File[tmpFiles.size()];
			for (int i = 0; i < listOfFilesOedered.length; i++) {
				listOfFilesOedered[i]=tmpFiles.get(i);
			}

			return listOfFilesOedered;
		}
		
		/*
		 * It returns the list of files
		 */
		public File[] getFileList_WithoutPrefix(String folderPath){
			
			File folder = new File(folderPath);
			File[] listOfFiles = folder.listFiles();
			
			File[] listOfFilesOedered=new File[listOfFiles.length];
			String fname="";
			int index=0;
				
			for (int i = 0; i < listOfFiles.length; i++) {
				if (listOfFiles[i].isFile()) {
					fname=listOfFiles[i].getName();
					listOfFilesOedered[index]=listOfFiles[i];
					index++;
				}
			}
			
			Vector<File> tmpFiles=new Vector<File>();
			for (int i = 0; i < listOfFilesOedered.length; i++) {
				if (listOfFilesOedered[i]!=null) tmpFiles.add(listOfFilesOedered[i]);
			}
			
			listOfFilesOedered=new File[tmpFiles.size()];
			for (int i = 0; i < listOfFilesOedered.length; i++) {
				listOfFilesOedered[i]=tmpFiles.get(i);
			}

			return listOfFilesOedered;
		}
}
