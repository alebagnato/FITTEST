package eu.fittest.eclipse.daikon.utils;

import java.io.File;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.osgi.framework.Bundle;

public class DaikonUtils {

	private static String daikonJar = "daikon.jar";


	/**
	 * Get the executable daikon jar
	 * 
	 * @author ebrosse
	 * @return
	 */
	public static File getDaikonJar(){

		Bundle bundle = Platform
				.getBundle(eu.fittest.eclipse.daikon.Activator.PLUGIN_ID);


		Logger.getAnonymousLogger().log(Level.INFO,
				"Selected daikon binary is " + daikonJar);
		File daikon;
		try {
			daikon = new File(FileLocator.toFileURL(bundle.getEntry("resources" + File.separatorChar + "bin"
					+ File.separatorChar + daikonJar)).getFile());
			daikon.setExecutable(true);

			return daikon;
		} catch (IOException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
		}

		return null;
	}
}
