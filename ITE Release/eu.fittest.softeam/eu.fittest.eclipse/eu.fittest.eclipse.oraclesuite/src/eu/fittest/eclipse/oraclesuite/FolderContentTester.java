package eu.fittest.eclipse.oraclesuite;

import java.util.List;

import org.eclipse.core.expressions.PropertyTester;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
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
	}


	public boolean test(Object receiver, String property, Object[] args,
			Object expectedValue) {
	
		if(property.endsWith("members_extension")){
			IFolder folder = getFolder();
			if(folder != null){ 
				
				try {
					List<IFile> allLogFiles = ResourceUtils.collectFiles(folder, "log", "log_");
					return (allLogFiles.size() > 0);
				} catch (CoreException e) {
					e.printStackTrace();
				}

			}
		}
		return false;
	}

	static public IFolder getFolder() {
		
		CommonNavigator navigator =(CommonNavigator) Viewer.getView(IFITTESTFolderConstants.NAVIGATOR);
		
		if(navigator!=null){
			CommonViewer viewer = navigator.getCommonViewer();
			if (viewer != null) {
				ISelection selection = viewer.getSelection();
				if (selection instanceof IStructuredSelection && !selection.isEmpty()) {
					IStructuredSelection sel = (IStructuredSelection) selection;
					Object object = sel.getFirstElement();
					return (object instanceof IFolder && !(object instanceof IProject))? (IFolder)object:null;
				}
			}
		}
		return null;
	}

}
