package eu.fittest.eclipse.dtracesplit.utils;

import java.io.File;
import java.io.IOException;
import java.util.Hashtable;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.osgi.framework.Bundle;

public class DTraceSplitUtils {

	private static Map<String, String> DTRACESPLIT_EXE = new Hashtable<String, String>();

	static {
		DTRACESPLIT_EXE.put("Mac OS_64bits", "dtrsplitMacOs");
		DTRACESPLIT_EXE.put("Mac OS_32bits", "dtrsplitMacOs");
		DTRACESPLIT_EXE.put("Windows_32bits", "dtrsplitWin32.exe");
		DTRACESPLIT_EXE.put("Windows_64bits", "dtrsplitWin64.exe");
		DTRACESPLIT_EXE.put("Linux_64bits", "dtrsplitLinux");
		DTRACESPLIT_EXE.put("Linux_32bits", "dtrsplitLinux");
		DTRACESPLIT_EXE.put("x86", "32bits");
		DTRACESPLIT_EXE.put("i386", "32bits");
		DTRACESPLIT_EXE.put("x86_64", "64bits");
		DTRACESPLIT_EXE.put("amd64", "64bits");
		DTRACESPLIT_EXE.put("ppc", "64bits");
	}

	public static String getDtrsplitExeName() {
		final String platform = System.getProperty("os.name");
		final String arch = System.getProperty("os.arch");
		
		String exeName = "";
		if(platform.startsWith("Windows")){
			exeName = DTRACESPLIT_EXE.get("Windows_" + DTRACESPLIT_EXE.get(arch));
		}
		else if(platform.startsWith("Linux")){
			exeName = DTRACESPLIT_EXE.get("Linux_" + DTRACESPLIT_EXE.get(arch));
		}
		else if(platform.startsWith("Mac OS")){
			exeName = DTRACESPLIT_EXE.get("Mac OS_" + DTRACESPLIT_EXE.get(arch));
		}
		
		return exeName;
	}
	
	/**
	 * Get the executable Dtrsplit utility
	 * 
	 * @author ebrosse
	 * @return
	 */
	public static File getDtrsplitExecutable(){
		
		Bundle bundle = Platform
				.getBundle(eu.fittest.eclipse.dtracesplit.Activator.PLUGIN_ID);
		
		String exeName = getDtrsplitExeName();
		if (exeName != null) {

			Logger.getAnonymousLogger().log(Level.INFO,
					"Selected Dtrcsplit binary is " + exeName);
			File dtrcsplit;
			try {
				dtrcsplit = new File(FileLocator.toFileURL(bundle.getEntry("resources" + File.separatorChar + "bin"
								+ File.separatorChar + exeName)).getFile());
				dtrcsplit.setExecutable(true);
				
				return dtrcsplit;
			} catch (IOException e) {
				Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
			}
		}
		
		return null;
	}
}
