package eu.fittest.eclipse.gui.navigator;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.navigator.CommonNavigator;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.services.filetransfer.spec.FileEvent;
import eu.fittest.common.services.filetransfer.spec.IFileTransferService;
import eu.fittest.common.services.filetransfer.spec.IFileTransferServiceListener;

import eu.fittest.eclipse.startup.FITTESTServerStartUp;

public class FITTESTNavigator extends CommonNavigator implements IFileTransferServiceListener{

	public FITTESTNavigator() {
		try {
			FITTESTServerStartUp.getITEAgentInstance().getServiceRegistry().findService(IFileTransferService.class).addServiceListener(this);
		} catch (FITTESTException e) {
			Logger.getAnonymousLogger().log(Level.WARNING, e.getMessage()+" FITTESTNavigator can't listen to File Transfer Service events");
		}
	}
	
	@Override
	public synchronized void incomingEvent(FileEvent event) {
		switch (event.getKind()) {
		case upload:
			updateTree();
			break;
		default:
			break;
		}
	}
	
	static public void updateTree(){
		IProject [] projects = ResourcesPlugin.getWorkspace().getRoot().getProjects();
		for(IProject p : projects){
			try {
				p.refreshLocal(IProject.DEPTH_INFINITE, null);
			} catch (CoreException e) {
				e.printStackTrace();
			}
		}
	}

}
