package eu.fittest.tranformtools.popup.actions;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;


public class Log2FSMAction extends AbstractHandler implements IObjectActionDelegate{
	private Shell _shell;
	private IFolder[] _selectedFolders; 
	
	public Log2FSMAction() {

	}
	
	public void run(IAction action) {
		IFolder folder = _selectedFolders[0]; // first file only
		String outputModel = new File(folder.getLocation().toFile(), folder.getName()+".fsm").getAbsolutePath();
		eu.fittest.tranformtools.actions.Log2FSMAction modelInferrer = new  eu.fittest.tranformtools.actions.Log2FSMAction(folder.getLocation().toOSString(), outputModel, true);
		modelInferrer.process();
	}
	
	
	public void selectionChanged(IAction action, ISelection selection) {
		ArrayList<IFolder> sFolders = new ArrayList<IFolder>();
		IFolder folder = null;
		if (selection != null && selection instanceof IStructuredSelection) {
			IStructuredSelection ss = (IStructuredSelection) selection;

			for (Iterator iter = ss.iterator(); iter.hasNext();) {
				Object obj = iter.next();
				if (obj instanceof IFolder) {
					folder = (IFolder)obj;
				} else if (obj instanceof IAdaptable) {
					IAdaptable a = (IAdaptable) obj;
					IResource res = (IResource) a.getAdapter(IResource.class);
					if (res instanceof IFolder) {
						folder = (IFolder) res;
					}
				}

				if (folder != null && folder.isSynchronized(IResource.DEPTH_ZERO)) {
					sFolders.add(folder);
				} else if (!folder.isSynchronized(IResource.DEPTH_ZERO)) {
					MessageDialog.openInformation(_shell, "Synchronization error",
					"The selected folder is not synchronized with the system file, please refresh the project!");
				}
			}
		}

		if (sFolders.isEmpty()) {
			_selectedFolders = null;
		} else {
			_selectedFolders = sFolders.toArray(new IFolder[sFolders.size()]);
		}

	}

	public void setActivePart(IAction action, IWorkbenchPart targetPart){
		_shell = targetPart.getSite().getShell();
	}


	public Object execute(ExecutionEvent event) throws ExecutionException {
		run(null);
		return null;
	}

}
