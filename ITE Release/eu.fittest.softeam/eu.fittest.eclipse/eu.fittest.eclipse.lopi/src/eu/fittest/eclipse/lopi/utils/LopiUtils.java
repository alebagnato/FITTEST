package eu.fittest.eclipse.lopi.utils;

import java.io.File;
import java.io.IOException;
import java.util.Hashtable;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.osgi.framework.Bundle;

public class LopiUtils {

	private static Map<String, String> LOPI_EXE = new Hashtable<String, String>();

	static {
		LOPI_EXE.put("Mac OS_64bits", "lopiMacOs");
		LOPI_EXE.put("Mac OS_32bits", "lopiMacOs");
		LOPI_EXE.put("Windows_32bits", "lopiWin32.exe");
		LOPI_EXE.put("Windows_64bits", "lopiWin64.exe");
		LOPI_EXE.put("Linux_64bits", "lopiLinux");
		LOPI_EXE.put("Linux_32bits", "lopiLinux");
		LOPI_EXE.put("x86", "32bits");
		LOPI_EXE.put("i386", "32bits");
		LOPI_EXE.put("x86_64", "64bits");
		LOPI_EXE.put("amd64", "64bits");
		LOPI_EXE.put("ppc", "64bits");
	}

	public static String getLopiExeName() {
		final String platform = System.getProperty("os.name");
		final String arch = System.getProperty("os.arch");
		
		String exeName = ""; // Default Windows ;-(
		if(platform.startsWith("Windows")){
			exeName = LOPI_EXE.get("Windows_" + LOPI_EXE.get(arch));
		}
		else if(platform.startsWith("Linux")){
			exeName = LOPI_EXE.get("Linux_" + LOPI_EXE.get(arch));
		}
		else if(platform.startsWith("Mac OS")){
			exeName = LOPI_EXE.get("Mac OS_" + LOPI_EXE.get(arch));
		}
		return exeName;
	}
	
	/**
	 * Get the executable lopi utility
	 * 
	 * @author ebrosse
	 * @return
	 */
	public static File getLopiExecutable(){
		
		Bundle bundle = Platform
				.getBundle(eu.fittest.eclipse.lopi.Activator.PLUGIN_ID);
		
		String exeName = getLopiExeName();
		if (exeName != null) {

			Logger.getAnonymousLogger().log(Level.INFO,
					"Selected lopi binary is " + exeName);
			File lopi;
			try {
				lopi = new File(FileLocator.toFileURL(bundle.getEntry("resources" + File.separatorChar + "bin"
								+ File.separatorChar + exeName)).getFile());
				lopi.setExecutable(true);
				
				return lopi;
			} catch (IOException e) {
				Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
			}
		}
		
		return null;
	}
}
