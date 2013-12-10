package eu.fittest.eclipse.focus.commands;

import org.eclipse.core.runtime.IPath;
import org.eclipse.ui.IEditorLauncher;

import eu.fittest.eclipse.focus.FocusLoader;

public class Launcher implements IEditorLauncher {

	@Override
	public void open(IPath file) {
		FocusLoader.loadFocus(file.toOSString());
	}

}
