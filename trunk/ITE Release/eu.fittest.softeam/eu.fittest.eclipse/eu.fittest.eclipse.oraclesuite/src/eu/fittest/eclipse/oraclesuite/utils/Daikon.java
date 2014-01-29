package eu.fittest.eclipse.oraclesuite.utils;

import java.io.File;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.osgi.framework.Bundle;

public class Daikon {


	/**
	 * Get the executable daikon utility
	 * 
	 * @author ebrosse
	 * @return
	 */
	public static File getDaikonJar(){

		Bundle bundle = Platform
				.getBundle(eu.fittest.eclipse.oraclesuite.Activator.PLUGIN_ID);

		final String daikonJar = "daikon.jar";


		File daikonJarFile;
		try {
			daikonJarFile = new File(FileLocator.toFileURL(bundle.getEntry("resources" + File.separatorChar + "bin"
					+ File.separatorChar + daikonJar)).getFile());
			daikonJarFile.setExecutable(true);

			return daikonJarFile;
		} catch (IOException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
			return null;
		}
	}
}

