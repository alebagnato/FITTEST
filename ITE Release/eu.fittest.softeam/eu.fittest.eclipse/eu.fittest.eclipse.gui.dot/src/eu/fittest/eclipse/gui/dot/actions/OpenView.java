package eu.fittest.eclipse.gui.dot.actions;

import java.io.File;

import net.claribole.zgrviewer.Utils;
import net.claribole.zgrviewer.ZGRViewer;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;

import eu.fittest.eclipse.gui.dot.LauncherUtil;

public class OpenView implements IObjectActionDelegate{
	private IFile _selectedFile;
	
	public OpenView() {
		// TODO Auto-generated constructor stub
	}

	@Override
	public void run(IAction action) {			
		File dotFile = new File(_selectedFile.getLocation().toOSString());
		LauncherUtil.openDOTFile(dotFile);		
	}

	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		if(selection instanceof IStructuredSelection){
			IStructuredSelection sselection = (IStructuredSelection) selection;
			_selectedFile = (IFile)sselection.getFirstElement();
		}
		
		
	}

	@Override
	public void setActivePart(IAction action, IWorkbenchPart targetPart) {
		// TODO Auto-generated method stub
		
	}

}
