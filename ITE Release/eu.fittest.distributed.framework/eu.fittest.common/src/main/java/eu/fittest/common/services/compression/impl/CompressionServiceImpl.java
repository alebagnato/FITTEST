package eu.fittest.common.services.compression.impl;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URI;
import java.util.Vector;
import java.util.logging.Level;
import eu.fittest.common.util.ITELogger;
import java.util.zip.Adler32;
import java.util.zip.CheckedInputStream;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.AbstractService;
import eu.fittest.common.services.compression.spec.ICompressionService;

public class CompressionServiceImpl extends AbstractService implements ICompressionService{
	private static final int BUFFER = 2048;
	private static final File _TMP_FOLDER = new File(System.getProperty("java.io.tmpdir"));
	
	public long checksum(File f) throws FITTESTException{
		try {
			CheckedInputStream cis = new CheckedInputStream(new FileInputStream(f), new Adler32());
			BufferedInputStream bis = new BufferedInputStream(cis,BUFFER);
			byte data[] = new byte[BUFFER];
            while(bis.read(data, 0, BUFFER) != -1);
            bis.close();
			return cis.getChecksum().getValue();
		} catch (FileNotFoundException e) {
			throw new FITTESTException(e.getMessage());
		} catch (IOException e) {
			throw new FITTESTException(e.getMessage());
		}
	}
	
	public void isChecksumValid(File file, long checksum) throws FITTESTException{
		if(checksum != 0L){
			if(checksum(file)==checksum){
				ITELogger.log(Level.INFO, "CRC OK for "+file);
			}
			else{
				 throw new FITTESTException("CRC does not match");
			}
		}
	}
	
	private Vector<File> collectFiles(File file){
		Vector<File> files = new Vector<File>();
		files.add(file);
		
		if(file.isDirectory()){
			for(File subFile: file.listFiles()){
				files.addAll(collectFiles(subFile));
			}
		}	

		return files;
	}
	
	public File zipAndGzip(File from) throws FITTESTException{
    	File tmpzip = new File(_TMP_FOLDER, from.getName()+".zip");
    	File tmpgzip = new File(_TMP_FOLDER, from.getName()+".gzip");
    	ITELogger.log(Level.INFO, "zipping in "+tmpzip);
    	zip(from, tmpzip.getAbsolutePath());
    	ITELogger.log(Level.INFO, "Gzipping in "+tmpgzip);
    	gzip(tmpzip, tmpgzip.getAbsolutePath());
    	return tmpgzip;
	}
	
	public File gunzipAndUnzip(File from) throws FITTESTException{
		if(from.getName().endsWith(".gzip")){
	    	File tmpzip = new File(_TMP_FOLDER, from.getName()+".zip");
	    	ITELogger.log(Level.INFO, "GUnzipping in "+tmpzip);
	    	gunzip(from, tmpzip.getAbsolutePath());
	    	ITELogger.log(Level.INFO, "Unzipping in "+from);
	    	unzip(tmpzip, from.getParentFile().getAbsolutePath());
	    	return new File(from.getParentFile(), from.getName().substring(0, from.getName().lastIndexOf(".")));
		}
		else{
			throw new FITTESTException(from.getAbsolutePath()+" is not a file gzipped by FITTEST");
		}
	}
	
	public void zip(File from, String to) throws FITTESTException{
		File toFile = new File(to);
	      try {
	          FileOutputStream fos = new FileOutputStream(toFile);
	          ZipOutputStream zos = new ZipOutputStream(new BufferedOutputStream(fos));
	          byte data[] = new byte[BUFFER];
	          
	          Vector<File> files = collectFiles(from);
	          URI base = null;
	          if(from.isDirectory()){
	        	  base = from.toURI();
	          }
	          
	          for (File f: files) {	           	 
	             String name = null;
	             if(base!=null){
	            	 name = from.getName()+"/"+base.relativize(f.toURI()).getPath();
	             }
	             else{
	            	 name = f.getName();
	             }
	             
	             if(f.isDirectory()){
	            	 name = name.endsWith("/") ? name : name + "/";
	             }

	             ZipEntry entry = new ZipEntry(name);
	             entry.setTime(f.lastModified());
	             zos.putNextEntry(entry);
	             
	             if(f.isFile()){
	            	 BufferedInputStream bis = new BufferedInputStream(new FileInputStream(f), BUFFER);
		             int count;
		             while((count = bis.read(data, 0, BUFFER)) != -1) {
		                zos.write(data, 0, count);
		             }
		             bis.close();
	             }
	             zos.closeEntry();
	          }
	          zos.close();
	       } catch(Exception e) {
	          throw new FITTESTException(e.getMessage());
	       }
	}
	
	public void unzip(File from, String to) throws FITTESTException{		
		try {
	         FileInputStream fis = new FileInputStream(from);
	         ZipInputStream zis = new ZipInputStream(new BufferedInputStream(fis, BUFFER));
	         ZipEntry entry = zis.getNextEntry();
	         while(entry != null) {
	            int count;
	            byte data[] = new byte[BUFFER];
	         
	            File outFile = null;
	            if(entry.isDirectory()){
	            	outFile = new File(to,entry.getName());
	            	outFile.mkdirs();
	            }
	            else{
	            	outFile = new File(to, entry.getName());
	            	
	            	FileOutputStream fos = new FileOutputStream(outFile);
	            	BufferedOutputStream bos = new BufferedOutputStream(fos, BUFFER);
		            while ((count = zis.read(data, 0, BUFFER)) != -1) {
		               bos.write(data, 0, count);
		            }
		            bos.flush();
		            bos.close();
	            }
	            zis.closeEntry();
	            
	            entry = zis.getNextEntry();
	         }
	         zis.close();
	      } catch(Exception e) {
	         throw new FITTESTException(e.getMessage());
	      }
	}
	
	public void gzip(File from, String to) throws FITTESTException{
		try {
			GZIPOutputStream gos = new GZIPOutputStream(new BufferedOutputStream(new FileOutputStream(new File(to)),BUFFER));
			BufferedInputStream bis = new BufferedInputStream(new FileInputStream(from));
			int count;
			byte data[] = new byte[BUFFER];
            while((count = bis.read(data, 0, BUFFER)) != -1) {
               gos.write(data, 0, count);
            }
            bis.close();
            gos.close();					
		} catch (FileNotFoundException e) {
			throw new FITTESTException(e.getMessage());
		} catch (IOException e) {
			throw new FITTESTException(e.getMessage());
		}
	}
	
	public void gunzip(File from, String to)  throws FITTESTException{
		try {
			GZIPInputStream gis = new GZIPInputStream(new BufferedInputStream(new FileInputStream(from), BUFFER));		
			BufferedOutputStream bos = new BufferedOutputStream(new FileOutputStream(new File(to)), BUFFER);
			int count;
			byte data[] = new byte[BUFFER];
            while((count = gis.read(data, 0, BUFFER)) != -1) {
               bos.write(data, 0, count);
            }
            bos.close();
            gis.close();
		} catch (FileNotFoundException e) {
			throw new FITTESTException(e.getMessage());
		} catch (IOException e) {
			throw new FITTESTException(e.getMessage());
		}
	}

	@Override
	public String getName() {
		return ICompressionService.class.getName();
	}
}
