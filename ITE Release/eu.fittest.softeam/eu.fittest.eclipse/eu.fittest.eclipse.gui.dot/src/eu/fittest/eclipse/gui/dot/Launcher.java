package eu.fittest.eclipse.gui.dot;

import java.io.File;

import net.claribole.zgrviewer.Utils;
//import net.claribole.zgrviewer.DOTManager;
import net.claribole.zgrviewer.ZGRViewer;

import org.eclipse.core.runtime.IPath;
import org.eclipse.ui.IEditorLauncher;

public class Launcher implements IEditorLauncher {

	@Override
	public void open(IPath file) {
		File dotFile = new File(file.toOSString());
		LauncherUtil.openDOTFile(dotFile);
	}

}
