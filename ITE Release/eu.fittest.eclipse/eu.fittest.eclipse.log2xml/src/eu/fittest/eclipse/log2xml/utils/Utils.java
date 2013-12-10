package eu.fittest.eclipse.log2xml.utils;

import java.io.File;
import java.io.IOException;
import java.util.Hashtable;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.osgi.framework.Bundle;

import eu.fittest.common.core.constants.FITTESTSingleton;

public class Utils {

	private static Map<String, String> HASLOG_EXE = new Hashtable<String, String>();

	static {
		HASLOG_EXE.put("Mac OS_64bits", "haslogMacOs");
		HASLOG_EXE.put("Mac OS_32bits", "haslogMacOs");
		HASLOG_EXE.put("Windows_32bits", "haslog.exe");
		HASLOG_EXE.put("Windows_64bits", "haslog.exe");// supposed that Windows
														// 64 is still 32-bits
														// compatible
		HASLOG_EXE.put("Linux_64bits", "haslogLinux64");
		HASLOG_EXE.put("Linux_32bits", "haslogLinux32");
		HASLOG_EXE.put("x86", "32bits");
		HASLOG_EXE.put("i386", "32bits");
		HASLOG_EXE.put("x86_64", "64bits");
		HASLOG_EXE.put("amd64", "64bits");
		HASLOG_EXE.put("ppc", "64bits");
	}

	public static String getHaslogExeName() {
		final String platform = System.getProperty("os.name");
		final String arch = System.getProperty("os.arch");
		
		String exeName = "haslog.exe"; // Default Windows ;-(
		if(platform.startsWith("Windows")){
			exeName = HASLOG_EXE.get("Windows_" + HASLOG_EXE.get(arch));
		}
		else if(platform.startsWith("Linux")){
			exeName = HASLOG_EXE.get("Linux_" + HASLOG_EXE.get(arch));
		}
		else if(platform.startsWith("Mac OS")){
			exeName = HASLOG_EXE.get("Mac OS_" + HASLOG_EXE.get(arch));
		}
		return exeName;
	}
	
	/**
	 * Get the executable haslog utility
	 * 
	 * @author cdnguyen
	 * @return
	 */
	public static File getHasLogExecutable(){
		
		Bundle bundle = Platform
				.getBundle(eu.fittest.eclipse.log2xml.Activator.PLUGIN_ID);
		
		String exeName = Utils.getHaslogExeName();
		if (exeName != null) {

			Logger.getAnonymousLogger().log(Level.INFO,
					"Selected Haslog binary is " + exeName);
			File haslog;
			try {
				haslog = new File(FileLocator.toFileURL(bundle.getEntry("resources" + File.separatorChar + "bin"
								+ File.separatorChar + exeName)).getFile());
				haslog.setExecutable(true);
				
				return haslog;
			} catch (IOException e) {
				Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
			}
		}
		
		return null;
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
			Process process = Runtime.getRuntime().exec(haslog.getAbsolutePath() + " +RTS -K200M -RTS -c "+  file.getName(), null, containerFolder.getLocation().toFile()) ;
			FITTESTSingleton.getThreadPool().execute(new StreamGobbler(process.getErrorStream(), Level.SEVERE));
            FITTESTSingleton.getThreadPool().execute(new StreamGobbler(process.getInputStream(), Level.INFO));
			process.waitFor();
//			process = Runtime.getRuntime().exec(haslog.getAbsolutePath() + " --xml " + f.getName(), null, _logFolder.getLocation().toFile());
			process = Runtime.getRuntime().exec(haslog.getAbsolutePath() + " +RTS -K200M -RTS -x --appEventOnly "+  file.getName(), null, containerFolder.getLocation().toFile()) ;
			FITTESTSingleton.getThreadPool().execute(new StreamGobbler(process.getErrorStream(), Level.SEVERE));
            FITTESTSingleton.getThreadPool().execute(new StreamGobbler(process.getInputStream(), Level.INFO));
			process.waitFor();
		} catch (Exception e) {
			Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
		}
	}

}
