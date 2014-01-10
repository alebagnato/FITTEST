package eu.fittest.modelInference.imu.utility;

import java.io.BufferedReader;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Vector;

/**
 * Some generic utilities 
 * 
 * @author Alessandro Marchetto
 *
 */
public class Utility {
	BufferedWriter outF=null;
	java.util.Random rand = new java.util.Random();
	
	 public String[] copy(String[] st){
		
		   String[] cp=new String[st.length];
		   for (int i = 0; i < st.length; i++) {
			   cp[i]=st[i];
		   }
		   return cp;
	   }
	 
	 public Vector<String[]> copy(Vector<String[]> st){
		 Vector<String[]> vst=new Vector<String[]>();
		 String[] cp;
		 for (int i = 0; i < st.size(); i++) {
			 cp=copy(st.get(i));
			 vst.add(cp);
		}
		   
		   return vst;
	   }
	
	public boolean equal(String[] object1, String[] object2){
		if (object1.length!=object2.length) {return false;}
		boolean equal=false;
		int i=0;
		while (i<object1.length){
			if (object1[i].trim().equals(object2[i].trim())) equal=true;
			else return false;
			i++;	
			}
		return equal;
		}
	
	public boolean equalContent(String[] object1, String[] object2){
		
		if (object1.length!=object2.length) {
			return false;}
		if (object1.length==0) return false;
		
		boolean iscontaned=true;
		
		for (int i = 0; i < object2.length; i++) {
			if (!iscontained(object1,object2[i])) iscontaned=false;
		}
		if (iscontaned==false){ return false; }
		
		for (int i = 0; i < object1.length; i++) {
			if (!iscontained(object2,object1[i])) iscontaned=false;
		}
		if (iscontaned==false){ return false; }
		
		return true; 
		
		
		}

	public boolean iscontained(String[] object1, String str){
		int i=0;
		while (i<object1.length){
			if (object1[i].trim().equals(str.trim())) return true;
			i++;
		}
		return false;
	}
	
	
	public BufferedWriter createFile(String dir,String fileName,boolean append){
		try{
			BufferedWriter outF = new BufferedWriter(new FileWriter(dir+System.getProperty("file.separator")+fileName,append));
			outF.flush();
			return outF;
		}catch(Exception e){
			e.printStackTrace();return null;
		}
	}
	
	public BufferedWriter createANewFile(String dir,String fileName){
		try{
			BufferedWriter outF = new BufferedWriter(new FileWriter(dir+System.getProperty("file.separator")+fileName));
			outF.flush();
			return outF;
		}catch(Exception e){
			try{
			BufferedWriter outF = new BufferedWriter(new FileWriter(dir+fileName));
			outF.flush();
			return outF;
			}catch(Exception e2){ e2.printStackTrace(); return null;}
		}
	}
	

		public void createFile(String dir,String fileName){
			try{
				outF = new BufferedWriter(new FileWriter(dir+System.getProperty("file.separator")+fileName));
				outF.flush();
			}catch(Exception e){
				try{
				outF = new BufferedWriter(new FileWriter(dir+fileName));
				outF.flush();
					
				}catch(Exception e2){ e2.printStackTrace();}
			}
		}
		
		
		public void writeInFile(BufferedWriter outF, String line){
			try{
				outF.write(line+"\r\n");
				outF.flush();
			}catch(Exception e){
					e.printStackTrace();
			}
		}
		
		public void closeFile(BufferedWriter outF){
			try{
				outF.close();
			}catch(Exception e){
					e.printStackTrace();
			}
		}
		
	
		public void writeInFile(String line){
			try{
				outF.write(line+"\r\n");
				outF.flush();
			}catch(Exception e){
					e.printStackTrace();
			}
		}
	
		public void closeFile(){
			try{
				outF.close();
			}catch(Exception e){
					e.printStackTrace();
			}
		}
		
		
		public int getNumOfLines(String fileName){
			try{
				int counterOfLines=0;
				String str;
				BufferedReader inputFile = new BufferedReader(new FileReader(fileName));
				while ((str = inputFile.readLine()) != null) {
					if (!str.equals("")) {
						counterOfLines++;			
					}
				}
				inputFile.close();
				return counterOfLines;
			}catch(Exception e){
				e.printStackTrace();
				return -1;
			}
				
		
		}
		
		
		public long randomLong(){
			long value=rand.nextLong();
			if (value < 0)
				value = -value;
			return value;
		}
		
		public double randomDouble(boolean nozero,boolean noone){
			double value=rand.nextDouble();
			if (nozero) while(value==0){value=rand.nextDouble();}
			if (noone) while(value==1){value=rand.nextDouble();}
			if (value < 0)
				value = -value;
			return value;
		}
		public double randomDouble(boolean withMax,double maxValue){
			double value=rand.nextDouble();
			if (withMax) while(value>maxValue){value=rand.nextDouble();}
			if (value < 0)
				value = -value;
			return value;
		}
		public double randomDouble(double toSkip,boolean nozero,boolean noone){
			double value=rand.nextDouble();
			if (nozero) while(value==0){value=rand.nextDouble();}
			if (noone) while(value==1){value=rand.nextDouble();}
			if (toSkip>-1.0) while(value==toSkip){value=rand.nextDouble();}
			if (value < 0)
				value = -value;
			return value;
		}
		public double randomDouble(String type,double max,double min){
			
			double value=0;
			
			if (type.equals("H")){
				
					while((value<=max)||(value<=0)||(value>=1)) {
						
						value=rand.nextDouble();
						
					}
			}
			else { 
					while((value>=min)||(value<=0)||(value>=1)) {
						
						value=rand.nextDouble();
						
					}
			}
			
			return value;
			
		}
		
		public double getNextDouble(){
			return rand.nextDouble();
		}
		
		public int randomInt(int max,boolean nozero){
			
			int value=rand.nextInt(max);
			if (nozero) while(value==0){value=rand.nextInt(max);}
			if (value < 0)
				value = -value;
			return value;
			
			
		 }
		
		public int randomInt(int max,int toskip){
			
			int value=rand.nextInt(max);
			while(value==toskip){value=rand.nextInt(max);}
			if (value < 0)
				value = -value;
			return value;
			
			
		 }
		
		public int randomInt(int min,int max,boolean nozero){
			
			int value=rand.nextInt(max);
			if (min>-1)  while(value<min){value=rand.nextInt(max);}
			if (nozero) while(value==0){value=rand.nextInt(max);}
			if (value < 0)
				value = -value;
			return value;
			
			
		 }
		
		
	  public Vector<File[]> getItems(Vector<File[]> listOfFilesSequences, int fromIndex, int toIndex){
		  Vector<File[]> items=new Vector<File[]>();
		  for (int i = 0; i < listOfFilesSequences.size(); i++) {
			  if((i>=fromIndex)&&(i<toIndex)) {
				  items.add(listOfFilesSequences.get(i));
			  }
		}
		  return items;
	  }

	  public File[] getArraySubset(File[] tobesplitted, int startindex, int stopindex){
		  
		  
		  Vector<File> files=new Vector<File>();
		  for (int i = 0; i < tobesplitted.length; i++) {
			if ((i>=startindex)&&(i<stopindex)) {
				files.add(tobesplitted[i]);
			}
		}
		  
		  File[] splitted=new File[files.size()];
		  for (int i = 0; i < files.size(); i++) {
			  splitted[i]=files.get(i);
		  }
		  
		  return splitted;
	  }

		
	    public int fact(int n) {
	    	
	    	
	    	if (n <= 1) {
	    	    return 1;
	    	}
	    	
	    	else {
	    	    return n * fact(n-1);
	    	}
	        }


		public double min(double[] vec){
			double min=0;
			
			int zeros=countZeros(vec);
			int countNewIndex=0;
			double[] newVec=new double[vec.length-zeros];
			
			for (int i = 0; i < vec.length; i++) {
				if(vec[i]>0) {
					newVec[countNewIndex]=vec[i];
					countNewIndex++;
				}
			}

			for (int i = 0; i < newVec.length; i++) {
				if (i==0) min=newVec[i];
	
				else {
					if (newVec[i]<min) min=newVec[i];
				}
			}
			return min;
		}
		public double max(double[] vec){
			double max=0;
			
			int zeros=countZeros(vec);
			int countNewIndex=0;
			double[] newVec=new double[vec.length-zeros];
			
			for (int i = 0; i < vec.length; i++) {
				if(vec[i]>0) {
					newVec[countNewIndex]=vec[i];
					countNewIndex++;
				}
			}
			
			for (int i = 0; i < newVec.length; i++) {
				if (i==0) {
					max=newVec[i];
				}
				else {
					if (newVec[i]>max) max=newVec[i];
				}
			}
			return max;
		}
		
		public int countZeros(double[] vec){
			int numOfZeros=0;
			for (int i = 0; i < vec.length; i++) {
				
				if (vec[i]==0) {
					numOfZeros++;
				}
				
			}
			return numOfZeros;
		}
		
		public int countOnes(double[] vec){
			int numOfOnes=0;
			for (int i = 0; i < vec.length; i++) {
				
				if (vec[i]==1) {
					numOfOnes++;
				}
				
			}
			return numOfOnes;
		}
		
		
		public boolean createDirectory(String dirName) {
		    try {
		    	boolean success=false;
		    	File d=new File(dirName);
		    	if (d.exists()==false){
		    		success = (d).mkdirs();
		    		if (!success) {
		    			return true; 
		    		}
		    	}
			return false;    
		    } catch (Exception e) {return false;}
		  }
		
		public void moveFilesInSubdir(String dirSource,String dirTarget,int amountOfFilesToMove_Perc){
			File folderSource = new File(dirSource);
			File[] listOfFiles = folderSource.listFiles();
			
			createDirectory(folderSource+System.getProperty("file.separator")+dirTarget);
			BufferedWriter wrb;
			BufferedReader inb;
			String fname="";
			String s;
			
			int countFiles=0;			
			for (int i = 0; i < listOfFiles.length; i++) {
				if (listOfFiles[i].isFile()) {
					fname=listOfFiles[i].getName();
					if ((fname.startsWith("log_s"))&&(fname.endsWith(".txt"))) countFiles++;
				}
			}
			
			if (amountOfFilesToMove_Perc==-1) amountOfFilesToMove_Perc=30;
			double perc= ((new Double(amountOfFilesToMove_Perc).doubleValue()) * (countFiles)) / (100);
			
			for (int i = 0; i < (int)perc; i++) {
				if (listOfFiles[i].isFile()) {
						try{
							fname=listOfFiles[i].getName();
							if ((fname.startsWith("log_s"))&&(fname.endsWith(".txt"))) {
								wrb=createFile(folderSource+System.getProperty("file.separator")+dirTarget,fname,false);
								inb = new BufferedReader(new FileReader(folderSource+System.getProperty("file.separator")+fname));
								while ((s = inb.readLine()) != null) {
									writeInFile(wrb,s);
								}
								inb.close();
								wrb.close();
								listOfFiles[i].delete();
							}
						} catch (IOException e) {
							System.err.println("IO error while reading: " + fname);
						}
				}
			}
			
		
			
		}
		
		
		public void emptyDirectory(String folderDirPath){
			
			File dirfile=new File(folderDirPath);
			File[] listOfFiles = dirfile.listFiles(); 
			if(listOfFiles!=null) {         
				 for (int i = 0; i < listOfFiles.length; i++)    {
					 File f=listOfFiles[i];
					 if(f.isFile()) {
							f.delete();
					 }else if (f.isDirectory()){
						emptyDirectory(folderDirPath+System.getProperty("file.separator")+f.getName());
					 }
				 }
			}
			
			
		}
		
		
	public String[] removeEmptyItems(String[] toclean){
		
		Vector<String> tmp=new Vector<String>();
		
		for (int i = 0; i < toclean.length; i++) {
			if (!toclean[i].equals("")) tmp.add(toclean[i]);
		}
		String[] toarray=new String[tmp.size()];
		for (int i = 0; i < tmp.size(); i++) {
			toarray[i]=tmp.get(i);
		}
		
		return toarray;
	}
	
}
