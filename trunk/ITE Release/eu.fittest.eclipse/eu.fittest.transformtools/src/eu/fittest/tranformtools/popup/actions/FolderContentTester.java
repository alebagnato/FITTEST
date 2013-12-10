package eu.fittest.tranformtools.popup.actions;

import java.util.List;

import org.eclipse.core.expressions.PropertyTester;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.navigator.CommonNavigator;
import org.eclipse.ui.navigator.CommonViewer;

import eu.fittest.eclipse.gui.nature.IFITTESTFolderConstants;
import eu.fittest.eclipse.gui.utils.ResourceUtils;
import eu.fittest.eclipse.gui.utils.Viewer;

public class FolderContentTester extends PropertyTester {

	public FolderContentTester() {
		// TODO Auto-generated constructor stub
	}

	public boolean test(Object receiver, String property, Object[] args, Object expectedValue) {
		if (property.endsWith("members_extension")) {
			IFolder folder = ResourceUtils.getFolder();
			if (folder != null) {
				String prefix = null;
				if (expectedValue != null) {
					prefix = expectedValue.toString();
					if (prefix.endsWith("!"))
						prefix = prefix.replace("!", "");
				}

				try {
					for (Object fileExtension : args) {
						String ext = fileExtension.toString();
						List<IFile> listFiles = ResourceUtils.collectFiles(
								folder, ext, prefix);
						if (listFiles.size() > 0)
							return true;
					}

				} catch (CoreException e) {
					e.printStackTrace();
				}
			}
		}
		return false;
	}
	

}
