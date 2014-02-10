package eu.fittest.eclipse.haslog.utils;

import java.io.File;
import java.io.IOException;
import java.util.Hashtable;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.FileLocator;
import org.osgi.framework.Bundle;

import eu.fittest.eclipse.haslog.Activator;

public class HaslogUtils {

	private static Map<String, String> HASLOG_EXE = new Hashtable<String, String>();
	

	static {
		HASLOG_EXE.put("Mac OS_64bits", "haslogMacOs");
		HASLOG_EXE.put("Mac OS_32bits", "haslogMacOs");
		HASLOG_EXE.put("Windows_32bits", "haslogWin32.exe");
		HASLOG_EXE.put("Windows_64bits", "haslogWin64.exe");
		HASLOG_EXE.put("Linux_64bits", "haslogLinux");
		HASLOG_EXE.put("Linux_32bits", "haslogLinux");
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
		System.out.println("haslog : " + exeName );
		return exeName;
	}
	
	/**
	 * Get the executable haslog utility
	 * 
	 * @author cdnguyen
	 * @return
	 */
	public static File getHasLogExecutable(){
	
		
//		Bundle bundle = Platform.getBundle(eu.fittest.eclipse.haslog.Activator.PLUGIN_ID);
		
		Bundle bundle = Activator.getDefault().getBundle();
		
		String exeName = HaslogUtils.getHaslogExeName();
		
		if ((exeName != null) && (bundle != null)) {

			Logger.getAnonymousLogger().log(Level.INFO, "Selected Haslog binary is " + exeName);
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
}
