package eu.fittest.common.services.filetransfer.impl;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader; // by urueda
import java.io.FileWriter; // by urueda
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.Vector; // by ureda
import java.util.logging.Level;
import eu.fittest.common.util.ITELogger;
import java.util.regex.Matcher; // by urueda
import java.util.regex.Pattern; // by urueda

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.exception.FITTESTServiceConfigurationException;
import eu.fittest.common.core.service.AbstractService;
import eu.fittest.common.services.compression.spec.ICompressionService;
import eu.fittest.common.services.filetransfer.spec.FileEvent;
import eu.fittest.common.services.filetransfer.spec.FileEventKind;
import eu.fittest.common.services.filetransfer.spec.IFileTransferService;
import eu.fittest.common.services.filetransfer.spec.IFileTransferServiceListener;


public class FileTransferServiceImpl extends AbstractService implements IFileTransferService {
	private String _basePath;//file URI
	
    
    
    public String getName() {
        return IFileTransferService.class.getName();
    }
    
    // by urueda
    private static void filesJoiner(String base, String target, Object[] sources) throws IOException {
    	Arrays.sort(sources); // ensure we join log chunks in order
        ITELogger.log(Level.INFO,"Joining periodic logs to: " + base + "/" + target);
        FileWriter fw = new FileWriter(base + "/" + target, true); // true => append mode (keep contents of the file if already exists)
        FileReader fr;
        File chunkF;
        int rn;
        String logC;
        char[] cb = new char[1024];
        for (Object src : sources) {
        	logC = base + "/" + (String)src;
        	ITELogger.log(Level.INFO,"joining log chunk: " + logC);
        	fr = new FileReader(logC);
        	while ((rn = fr.read(cb)) > 0) {
        		fw.write(cb,0,rn);
        	}
        	fr.close();
        	// delete chunk file
        	chunkF = new File(logC);
        	chunkF.delete();
        }
        fw.close();
    }    
    
    // by urueda
    private static File joinPeriodicLogs(String logChunkPath) {
    	String logChunksRegex = "(log_[0-9]+)_[0-9]+.log";
		File fchunk = new File(logChunkPath);
		logChunkPath = fchunk.getName();
    	if (Pattern.matches(logChunksRegex,logChunkPath)) {
    		Matcher m = Pattern.compile(logChunksRegex).matcher(logChunkPath);
    		String logFname = m.replaceAll("$1.log"); // log target for all log chunks		
    		String baseD = fchunk.getParent();
    		fchunk = new File(baseD);
    		Vector<String> files2join = new Vector<String>();
    		for (File f : fchunk.listFiles()) {
				if (f.isFile() && Pattern.matches(logChunksRegex,f.getName())) {
					files2join.addElement(f.getName());
				}
    		}
    		try {
        		if (files2join.size() > 0) {
    				filesJoiner(baseD,logFname,files2join.toArray());
        		}
        		else {
        			ITELogger.log(Level.WARNING,"Log chunks should be >0 but found none?");
        		}
    			return new File(baseD + "/" + logFname);
        	}    	
        	catch (IOException e) {
        		ITELogger.log(Level.SEVERE,"IOException joining periodic log chunks");
        		return null;
        	}
    	}
    	else {
    		return fchunk;
    	}
    }
    
    public void upload(InputStream data, String relativeURI, long fileLength, boolean inflate, long checksum) throws FITTESTException {
    	if(_basePath==null) throw new FITTESTServiceConfigurationException("File Transfer Service base path has not been configured");
    	ITELogger.log(Level.INFO, "Uploading: "+relativeURI);
        try {
        	File file = null;
        	if (relativeURI.startsWith(System.getProperty("java.io.tmpdir"))) {
        		file = new File(new URI(relativeURI)); // it is actually absolute path
			} 
			else {
	        	file = new File(new URI(_basePath+relativeURI));
	        }
        	file.getParentFile().mkdirs();
        	ITELogger.log(Level.INFO, "upload into "+file.getCanonicalPath());
            BufferedOutputStream bos = new BufferedOutputStream(new FileOutputStream(file));
            for(long i = 0; i<fileLength;i++){
                bos.write(data.read());
            }
            bos.close();
            ITELogger.log(Level.INFO, "upload completed "+file.getCanonicalPath());
            
            if(inflate){
            	_registry.findService(ICompressionService.class).isChecksumValid(file, checksum);
            	File newfile = _registry.findService(ICompressionService.class).gunzipAndUnzip(file);
            	file.delete();
            	file = newfile;
            }
            File joinedF = joinPeriodicLogs(file.getAbsolutePath()); // by urueda
            file = joinedF == null ? file : joinedF; // by urueda
            fireEvent(new FileEvent(this, FileEventKind.upload, file.getAbsolutePath()));

        } catch (FileNotFoundException e) {
            throw new FITTESTException(e.getMessage());
        } catch (URISyntaxException e) {
            throw new FITTESTException(e.getMessage());
        } catch (IOException e) {
            throw new FITTESTException(e.getMessage());
        }
    }
    
    public void upload(InputStream data, String relativeURI, long fileLength) throws FITTESTException {
    	upload(data, relativeURI, fileLength, false, 0L);
    }

    
    
    public void download(OutputStream data, String absoluteURI) throws FITTESTException {
        try {
        	File file = new File(new URI(absoluteURI));
        	ITELogger.log(Level.INFO, "download of "+file.getCanonicalPath());
        	
            BufferedInputStream bis = new BufferedInputStream(new FileInputStream(file));            
            int i = bis.read();
            while(i!=-1){
                data.write(i);
                i = bis.read();
            }
            data.flush();
            bis.close();
            fireEvent(new FileEvent(this, FileEventKind.download, file.getAbsolutePath()));
        } catch (FileNotFoundException e) {
            throw new FITTESTException(e.getMessage());
        } catch (URISyntaxException e) {
            throw new FITTESTException(e.getMessage());
        } catch (IOException e) {
            throw new FITTESTException(e.getMessage());
        }
    }

    
    
    public void delete(String relativeURI) throws FITTESTException {
        try {
            boolean result = new File(new URI(relativeURI)).delete();
            if(!result) throw new FITTESTException(relativeURI + " not deleted");
        } catch (URISyntaxException e) {
            throw new FITTESTException(e.getMessage());
        }
    }

    

    
    public FileTransferServiceImpl() {
        super();
        _handlers.add(new DeleteMH(this));
        _handlers.add(new DownloadMH(this));
        _handlers.add(new UploadMH(this));
        _basePath = null;
    }

	
	public void setBasePath(String path) throws FITTESTException{
		try {
			new URI(path);//checking path syntax, check trailing forward slash?
			_basePath = path;
			//makeDir(new File(_basePath));
		} catch (URISyntaxException e) {
			throw new FITTESTException(e.getMessage());
		}
	}

	/*private void makeDir(File folder) {
		if (!folder.exists()) {
		    ITELogger.log(Level.INFO, "Making folder for: "+folder.getPath());
			//folder.mkdirs();
			try {
				folder.mkdir();
			} catch (Exception e) {
			    ITELogger.log(Level.INFO, "Exception during folder creation: "+folder.getPath());
			}
		}
	    ITELogger.log(Level.INFO, "Setting folder rights for: "+folder.getPath());
		// set folder rights
	    boolean result = folder.setReadable(true, false); 
	    ITELogger.log(Level.INFO, "Set readable succeded?: "+result);
	    result = folder.setWritable(true, false);
	    ITELogger.log(Level.INFO, "Set writable succeded?: "+result);
	}*/
	
	public void addServiceListener(IFileTransferServiceListener listener) {
		super.addServiceListener(listener);
	}

	
	public void removeServiceListener(IFileTransferServiceListener listener) {
		super.removeServiceListener(listener);
		
	}

	
	public String getBasePath() {
		return _basePath;
	}

}
