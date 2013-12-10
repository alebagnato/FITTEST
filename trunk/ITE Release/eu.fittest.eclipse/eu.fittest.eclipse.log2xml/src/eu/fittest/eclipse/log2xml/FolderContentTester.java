package eu.fittest.eclipse.log2xml;

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
	}


	public boolean test(Object receiver, String property, Object[] args,
			Object expectedValue) {
		
//		boolean foundXML = false;
//		boolean foundLog = false;
		
		if(property.endsWith("members_extension")){
			IFolder folder = getFolder();
			if(folder!=null){ //&& folder.getName().toLowerCase().indexOf("logger")!=-1
				
				try {
					List<IFile> allLogFiles = ResourceUtils.collectFiles(folder, "log", "log_");
					if (allLogFiles.size() > 0){
						return true;
					}
				} catch (CoreException e) {
					e.printStackTrace();
				}
//				try {
//					IResource[] resources = folder.members();
//					int i = 0;
//					while(!foundXML && (i < resources.length)){
//						if(resources[i] instanceof IFile){
//							IFile currentFile = (IFile) resources[i];
//							foundXML = currentFile.getName().endsWith(expectedValue.toString());
//							foundLog = currentFile.getName().endsWith(".log");
//						}
//						i++;
//					}
//				} catch (CoreException e) {
//					e.printStackTrace();
//				}
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
