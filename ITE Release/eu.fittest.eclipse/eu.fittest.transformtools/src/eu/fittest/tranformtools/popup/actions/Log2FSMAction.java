package eu.fittest.tranformtools.popup.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFolder;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;

import eu.fittest.eclipse.gui.nature.IFITTESTFolderConstants;
import eu.fittest.eclipse.gui.utils.ResourceUtils;

public class Log2FSMAction extends AbstractHandler implements IObjectActionDelegate{
	private Shell _shell;
	private IFolder _selection;
	
	public Log2FSMAction() {

	}

	
	public void run(IAction action) {
		IFolder output = _selection.getProject().getFolder(IFITTESTFolderConstants.MODELS);
		new eu.fittest.tranformtools.actions.Log2FSMAction(_shell).run(action, _selection ,output);
		
	}
	
	
	public void selectionChanged(IAction action, ISelection selection) {
		if(selection instanceof IStructuredSelection)
			_selection = (IFolder) ((IStructuredSelection)selection).getFirstElement();		
	}

	public void setActivePart(IAction action, IWorkbenchPart targetPart) {
		_shell = targetPart.getSite().getShell();
	}


	public Object execute(ExecutionEvent event) throws ExecutionException {
		_selection = ResourceUtils.getFolder();
		_shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
		run(null);
		return null;
	}

}
