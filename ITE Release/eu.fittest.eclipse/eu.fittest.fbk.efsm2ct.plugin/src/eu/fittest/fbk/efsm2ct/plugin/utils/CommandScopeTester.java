package eu.fittest.fbk.efsm2ct.plugin.utils;

import java.util.List;

import org.eclipse.core.expressions.PropertyTester;
import org.eclipse.core.resources.IContainer;
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
import eu.fittest.eclipse.gui.utils.Viewer;

public class CommandScopeTester extends PropertyTester {

	public CommandScopeTester() {
	}

	public boolean test(Object receiver, String property, Object[] args,
			Object expectedValue) {

//		Logger.getAnonymousLogger().info(
//				"receiver:" + receiver + "receiver class:"
//						+ (receiver.getClass()) + " property:" + property);

		if (receiver instanceof List) {

			List receiverAsList = (List) receiver;

			if (property.endsWith("filetype")) {

				if (receiverAsList.size() == 1) {

					IResource resource = (IResource) receiverAsList.get(0);

					if (resource instanceof IFile) {

						IFile f = (IFile) resource;

						return f.getName().endsWith((String) args[0]);

					}

				}
			} else if (property.endsWith("islogcollection")) {

				if (receiverAsList.size() == 1) {

					IResource resource = (IResource) receiverAsList.get(0);
					
					if (resource instanceof IContainer) {

						IFolder f = (IFolder) resource;

						return isSubfolder(f,"Testing sessions");
						
					}


				} 

			} else if (property.endsWith("isbinary")) {

				if (receiverAsList.size() == 1) {

					IResource resource = (IResource) receiverAsList.get(0);
					
					if (resource instanceof IContainer) {

						IFolder f = (IFolder) resource;

						return isSubfolder(f,"bin");
						
					}


				}

			} else if (property.endsWith("isevotest")) {

				if (receiverAsList.size() == 1) {

					IResource resource = (IResource) receiverAsList.get(0);
					
					if (resource instanceof IContainer) {

						IFolder f = (IFolder) resource;

						return isSubfolder(f,"evosuite-tests");
						
					}


				}

			}
		}


		return false;

	}

	private boolean isSubfolder(IContainer f, String folderName) {
		
		// Logger.getAnonymousLogger().info("isLogCollection on:"+f.getName());
		
		if (f.getName().equals(folderName)) {
			return true;
		} else {
			
			if (f.getParent() != null) {
				
				return isSubfolder(f.getParent(),folderName);
				
			} 
			
		}
		
		return false;

		
	}

	public boolean cu_test(Object receiver, String property, Object[] args,
			Object expectedValue) {

		boolean foundXML = false;
		boolean foundLog = false;

		if (property.endsWith("members_extension")) {
			IFolder folder = getFolder();
			if (folder != null) { // &&
									// folder.getName().toLowerCase().indexOf("logger")!=-1
				try {
					IResource[] resources = folder.members();
					int i = 0;
					while (!foundXML && (i < resources.length)) {
						if (resources[i] instanceof IFile) {
							IFile currentFile = (IFile) resources[i];
							foundXML = currentFile.getName().endsWith(
									expectedValue.toString());
							foundLog = currentFile.getName().endsWith(".log");
						}
						i++;
					}
				} catch (CoreException e) {
					e.printStackTrace();
				}
			}
		}
		return !foundXML && foundLog;
	}

	static public IFolder getFolder() {

		CommonNavigator navigator = (CommonNavigator) Viewer
				.getView(IFITTESTFolderConstants.NAVIGATOR);

		if (navigator != null) {
			CommonViewer viewer = navigator.getCommonViewer();
			if (viewer != null) {
				ISelection selection = viewer.getSelection();
				if (selection instanceof IStructuredSelection
						&& !selection.isEmpty()) {
					IStructuredSelection sel = (IStructuredSelection) selection;
					Object object = sel.getFirstElement();
					return (object instanceof IFolder && !(object instanceof IProject)) ? (IFolder) object
							: null;
				}
			}
		}
		return null;
	}

}
