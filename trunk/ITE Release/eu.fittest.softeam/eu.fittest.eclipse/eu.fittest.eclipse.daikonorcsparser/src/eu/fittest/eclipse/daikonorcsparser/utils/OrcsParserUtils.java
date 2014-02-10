package eu.fittest.eclipse.daikonorcsparser.utils;

import java.io.File;
import java.io.IOException;
import java.util.Hashtable;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.osgi.framework.Bundle;

public class OrcsParserUtils {

	private static Map<String, String> DAIKONICE_EXE = new Hashtable<String, String>();

	static {
		DAIKONICE_EXE.put("Mac OS_64bits", "daikoniceMacOs");
		DAIKONICE_EXE.put("Mac OS_32bits", "daikoniceMacOs");
		DAIKONICE_EXE.put("Windows_32bits", "daikoniceWin32.exe");
		DAIKONICE_EXE.put("Windows_64bits", "daikoniceWin64.exe");
		DAIKONICE_EXE.put("Linux_64bits", "daikoniceLinux");
		DAIKONICE_EXE.put("Linux_32bits", "daikoniceLinux");
		DAIKONICE_EXE.put("x86", "32bits");
		DAIKONICE_EXE.put("i386", "32bits");
		DAIKONICE_EXE.put("x86_64", "64bits");
		DAIKONICE_EXE.put("amd64", "64bits");
		DAIKONICE_EXE.put("ppc", "64bits");
	}

	public static String getDaikonIceExeName() {
		
		final String platform = System.getProperty("os.name");
		final String arch = System.getProperty("os.arch");
		
		String exeName = ""; 
		if(platform.startsWith("Windows")){
			exeName = DAIKONICE_EXE.get("Windows_" + DAIKONICE_EXE.get(arch));
		}
		else if(platform.startsWith("Linux")){
			exeName = DAIKONICE_EXE.get("Linux_" + DAIKONICE_EXE.get(arch));
		}
		else if(platform.startsWith("Mac OS")){
			exeName = DAIKONICE_EXE.get("Mac OS_" + DAIKONICE_EXE.get(arch));
		}
		return exeName;
	}
	
	/**
	 * Get the executable daikon ice utility
	 * 
	 * @author ebrosse
	 * @return
	 */
	public static File getDaikonIceExecutable(){
		
		Bundle bundle = Platform
				.getBundle(eu.fittest.eclipse.daikonorcsparser.Activator.PLUGIN_ID);
		
		String exeName = OrcsParserUtils.getDaikonIceExeName();
		if (exeName != null) {

			Logger.getAnonymousLogger().log(Level.INFO,
					"Selected Daikonice binary is " + exeName);
			File daikonice;
			try {
				daikonice = new File(FileLocator.toFileURL(bundle.getEntry("resources" + File.separatorChar + "bin"
								+ File.separatorChar + exeName)).getFile());
				daikonice.setExecutable(true);
				
				return daikonice;
			} catch (IOException e) {
				Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
			}
		}
		
		return null;
	}
}
