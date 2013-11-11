package eu.fittest.eclipse.focus;

import java.io.File;
import java.io.IOException;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.osgi.framework.Bundle;

import eu.fittest.common.core.constants.FITTESTSingleton;

public class FocusLoader {

	
	public static void loadFocus(String param){
		String pathToJDK = Activator.getDefault().getPreferenceStore().getString("focus.pathtojdk");
		if(pathToJDK == null || pathToJDK.length()==0){
			MessageDialog.openError(Display.getCurrent().getActiveShell(), "IBM FoCuS error", "Path to JDK has not been configured.\n" +
					"Please check Windows -> Preferences -> FITTEST -> IBM FoCuS");
		}
		else{
			try {
				Vector<String> params = new Vector<String>();
				params.add(new File(pathToJDK, "/bin/java").getAbsolutePath());
				
				Bundle bundle = Platform.getBundle(Activator.PLUGIN_ID);		
				File libFolder = new File(FileLocator.toFileURL(bundle.getEntry("resources/bin")).getFile());	
				params.add("-classpath");
				File[] libFiles = libFolder.listFiles();
				boolean atLeastOne = false;
				StringBuilder libsList = new StringBuilder();
				for(int i=0;i<libFiles.length;i++){
					if(libFiles[i].getName().endsWith(".jar")){
						if(atLeastOne) libsList.append(File.pathSeparator);
						atLeastOne = true;
						libsList.append(libFiles[i].getName());
					}
				}
				params.add(libsList.toString());
				params.add("com.ibm.focus.Focus");

				Logger.getAnonymousLogger().log(Level.INFO,Activator.PLUGIN_ID + " will run "+params);
				Process process = Runtime.getRuntime().exec(params.toArray(new String[0]),null, libFolder);
	            FITTESTSingleton.getThreadPool().execute(new StreamGobbler(process.getInputStream(), Level.INFO));
				FITTESTSingleton.getThreadPool().execute(new StreamGobbler(process.getErrorStream(), Level.SEVERE));
			} catch (IOException e) {
				MessageDialog.openError(Display.getCurrent().getActiveShell(), "IBM FoCuS error", e.getMessage());
			}
		}
	}
}
