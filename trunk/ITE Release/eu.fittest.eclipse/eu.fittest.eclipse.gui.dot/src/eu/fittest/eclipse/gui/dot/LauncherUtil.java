package eu.fittest.eclipse.gui.dot;

import java.io.File;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;

import eu.fittest.eclipse.gui.dot.preferences.PreferenceConstants;

import net.claribole.zgrviewer.ConfigManager;
import net.claribole.zgrviewer.Utils;
import net.claribole.zgrviewer.ZGRViewer;

public class LauncherUtil {
	
	/**
	 * Open a dot file
	 * @param dotFile
	 */
	public static void openDOTFile(File dotFile){

		// Check the path to DOT program
		String dotPath = Activator.getDefault().getPreferenceStore().getString(PreferenceConstants.P_PATH);
		
		boolean dotOK = false;
		if (dotPath != null){
			File dotProc = new File(dotPath);
			if (dotProc.exists()){
				dotOK = true;
			}
		}
		
		if (!dotOK){
			MessageDialog.openError(Display.getCurrent().getActiveShell(), "ITE DOT Viewer Error", "Path to DOT program has not been configured.\n" +
					"Please check Windows -> Preferences -> FITTEST -> DOT Program");
			return;
		}
		
		if (Utils.osIsMacOS()){
            System.setProperty("apple.laf.useScreenMenuBar", "true");
        }
		ZGRViewer viewer = new ZGRViewer(false);
		viewer.setDotProc(dotPath);
		viewer.getGraphicsManager().reset();
		try {
			viewer.setFile(dotFile);
		} catch (Exception e){
			MessageDialog.openError(Display.getCurrent().getActiveShell(), "ITE DOT Viewer Error", "Problem opening the file!");
			// problem with file
			viewer.exit();
		}
	}
}
