package eu.fittest.eclipse.component.converter;

import java.io.File;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;

import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.eclipse.gui.utils.ResourceUtils;
import eu.fittest.eclipse.haslog.utils.HaslogUtils;
import eu.fittest.eclipse.log2xml.utils.StreamGobbler;


public class Log2XMLBatchConverter {
	
	/**
	 * Recursively convert all encountered log files
	 * 
	 * @author cdnguyen
	 * @param folder
	 * 
	 * @throws CoreException 
	 */
	public void recursiveConvert(IFolder folder) throws CoreException{
		List<IFile> allLogFiles = ResourceUtils.collectFiles(folder, "log", "log_");
		try {
			final File haslog = HaslogUtils.getHasLogExecutable();
			if (haslog != null && haslog.canExecute()){
				for (IFile f : allLogFiles){
					convert(haslog, f);
				}
			}
		} catch (Exception e) {
			Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
		}
	}
	
	
	/**
	 * Convert a single file
	 * @param file
	 */
	public static void convert(File haslog, IFile file){
		try {				
			
	
			
			IFolder containerFolder = (IFolder) file.getParent(); 
			
			//  exporting xml part								
//			Process process = Runtime.getRuntime().exec(haslog.getAbsolutePath() +" --compress "+f.getName(), null, _logFolder.getLocation().toFile());
			String cmd = haslog.getAbsolutePath() + " +RTS -K200M -RTS -c "+  file.getName();
			
//			String cmd = haslog.getAbsolutePath() + " " + projectConfig.getLogging().getInstrumentation().getGhcrtOption() +  " " + file.getName();
			
			Logger.getAnonymousLogger().log(Level.INFO, "haslog cmd: " + cmd);
			
			Process process = Runtime.getRuntime().exec(cmd, null, containerFolder.getLocation().toFile()) ;
			FITTESTSingleton.getThreadPool().execute(new StreamGobbler(process.getErrorStream(), Level.SEVERE));
            FITTESTSingleton.getThreadPool().execute(new StreamGobbler(process.getInputStream(), Level.INFO));
			process.waitFor();
//			process = Runtime.getRuntime().exec(haslog.getAbsolutePath() + " --xml " + f.getName(), null, _logFolder.getLocation().toFile());
			cmd = haslog.getAbsolutePath() + " +RTS -K200M -RTS -x --appEventOnly "+  file.getName();
			System.err.println("cmd : " + cmd);
			process = Runtime.getRuntime().exec(cmd, null, containerFolder.getLocation().toFile()) ;
			FITTESTSingleton.getThreadPool().execute(new StreamGobbler(process.getErrorStream(), Level.SEVERE));
            FITTESTSingleton.getThreadPool().execute(new StreamGobbler(process.getInputStream(), Level.INFO));
			process.waitFor();
		} catch (Exception e) {
			Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
		}
	}
}
