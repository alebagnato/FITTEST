package eu.fittest.eclipse.component.logger;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.Bundle;

public class Activator extends AbstractUIPlugin {

	public static String PLUGIN_ID = "eu.fittest.eclipse.component.logger";
	
	public Activator() {

	}
	
	static public File getResource(String path) throws URISyntaxException, IOException{
		Bundle bundle = Platform.getBundle(PLUGIN_ID);
		return new File(FileLocator.toFileURL(bundle.getResource(path)).getFile());
	}

}
