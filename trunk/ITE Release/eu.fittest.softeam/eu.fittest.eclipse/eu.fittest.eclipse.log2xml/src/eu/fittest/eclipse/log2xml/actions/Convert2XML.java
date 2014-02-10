package eu.fittest.eclipse.log2xml.actions;

import java.io.File;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;

import eu.fittest.eclipse.component.converter.Log2XMLBatchConverter;
import eu.fittest.eclipse.gui.utils.ResourceUtils;
import eu.fittest.eclipse.log2xml.FolderContentTester;

public class Convert2XML extends AbstractHandler implements IObjectActionDelegate{
	private IFolder _logFolder = null;
	
	
	public Convert2XML() {
	}

	@Override
	public void run(IAction action) {
		final File haslog = eu.fittest.eclipse.haslog.utils.HaslogUtils.getHasLogExecutable();
		if (haslog != null && haslog.canExecute()){
			try {
				//Convert all files recursively
				List<IFile> allLogFiles = ResourceUtils.collectFiles(_logFolder, "log", "log_");
				for (IFile logFile : allLogFiles){
					Log2XMLBatchConverter.convert(haslog, logFile);
				}
				_logFolder.refreshLocal(IFolder.DEPTH_INFINITE, null);
			} catch (Exception e) {
				Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
			}
		}
		else{
			Logger.getAnonymousLogger().log(Level.SEVERE, "Execution permission can't be set for file " + haslog.getAbsolutePath());
			Display.getDefault().asyncExec(new Runnable() {
		        public void run() {
		            MessageDialog.openError(Display.getDefault().getActiveShell(),
		            		"Convert Log to XML", "Execution permission can't be set for file " + haslog.getAbsolutePath());
		        }
		    });
		}
	}

	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		if(selection instanceof IStructuredSelection)
			_logFolder = (IFolder) ((IStructuredSelection)selection).getFirstElement();		
		
	}

	@Override
	public void setActivePart(IAction action, IWorkbenchPart targetPart) {
		
	}

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		_logFolder = FolderContentTester.getFolder();
		run(null);
		return null;
	}

}
