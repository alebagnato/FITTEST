package eu.fittest.eclipse.declmerge.utils;

import java.io.File;
import java.io.IOException;
import java.util.Hashtable;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.osgi.framework.Bundle;

public class DeclMergeUtils {

	private static Map<String, String> DECLMERGE_EXE = new Hashtable<String, String>();

	static {
		DECLMERGE_EXE.put("Mac OS_64bits", "declmergeMacOs");
		DECLMERGE_EXE.put("Mac OS_32bits", "declmergeMacOs");
		DECLMERGE_EXE.put("Windows_32bits", "declmergeWin32.exe");
		DECLMERGE_EXE.put("Windows_64bits", "declmergeWin64.exe");
		DECLMERGE_EXE.put("Linux_64bits", "declmergeLinux");
		DECLMERGE_EXE.put("Linux_32bits", "declmergeLinux");
		DECLMERGE_EXE.put("x86", "32bits");
		DECLMERGE_EXE.put("i386", "32bits");
		DECLMERGE_EXE.put("x86_64", "64bits");
		DECLMERGE_EXE.put("amd64", "64bits");
		DECLMERGE_EXE.put("ppc", "64bits");
	}

	public static String getDeclmergeExeName() {
		final String platform = System.getProperty("os.name");
		final String arch = System.getProperty("os.arch");
		
		String exeName = ""; 
		if(platform.startsWith("Windows")){
			exeName = DECLMERGE_EXE.get("Windows_" + DECLMERGE_EXE.get(arch));
		}
		else if(platform.startsWith("Linux")){
			exeName = DECLMERGE_EXE.get("Linux_" + DECLMERGE_EXE.get(arch));
		}
		else if(platform.startsWith("Mac OS")){
			exeName = DECLMERGE_EXE.get("Mac OS_" + DECLMERGE_EXE.get(arch));
		}
		return exeName;
	}
	
	/**
	 * Get the executable haslog utility
	 * 
	 * @author ebrosse
	 * @return
	 */
	public static File getDeclmergeExecutable(){
		
		Bundle bundle = Platform
				.getBundle(eu.fittest.eclipse.declmerge.Activator.PLUGIN_ID);
		
		String exeName = DeclMergeUtils.getDeclmergeExeName();
		if (exeName != null) {

			Logger.getAnonymousLogger().log(Level.INFO,
					"Selected Declmerge binary is " + exeName);
			File declmerge;
			try {
				declmerge = new File(FileLocator.toFileURL(bundle.getEntry("resources" + File.separatorChar + "bin"
								+ File.separatorChar + exeName)).getFile());
				declmerge.setExecutable(true);
				
				return declmerge;
			} catch (IOException e) {
				Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
			}
		}
		
		return null;
	}
}
