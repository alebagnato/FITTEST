package eu.fittest.eclipse.oraclesuite.popup.actions;

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
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;

import eu.fittest.eclipse.gui.utils.ResourceUtils;
import eu.fittest.eclipse.oraclesuite.wizards.InferDaikonWizard;

public class InferDaikonAction extends AbstractHandler implements IObjectActionDelegate {

	private Shell shell;
	private IFolder[] selectedFolders; 
	
	public void run(IAction action) {
		if (selectedFolders != null && selectedFolders.length > 0) {
			IFolder folder = selectedFolders[0]; 
			
			InferDaikonWizard inferDaikonWizard = new InferDaikonWizard(folder.getLocation().toOSString());
			WizardDialog dialog = new WizardDialog(shell, inferDaikonWizard);
			dialog.open();
			
		}

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
					MessageDialog.openInformation(shell, "Synchronization error",
					"The selected folder is not synchronized with the system file, please refresh the project!");
				}
			}
		}

		if (sFolders.isEmpty()) {
			selectedFolders = null;
		} else {
			selectedFolders = sFolders.toArray(new IFolder[sFolders.size()]);
		}

	}

	public void setActivePart(IAction action, IWorkbenchPart targetPart){
		shell = targetPart.getSite().getShell();
	}


	public Object execute(ExecutionEvent event) throws ExecutionException {
		selectedFolders = new IFolder[1];
		selectedFolders[0] = ResourceUtils.getFolder();
		shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
		run(null);
		return null;
	}

}
