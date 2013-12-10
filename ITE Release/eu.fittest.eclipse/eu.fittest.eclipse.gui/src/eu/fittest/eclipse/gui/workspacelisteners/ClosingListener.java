package eu.fittest.eclipse.gui.workspacelisteners;

import org.eclipse.core.resources.ISaveContext;
import org.eclipse.core.resources.ISaveParticipant;
import org.eclipse.core.runtime.CoreException;

import eu.fittest.eclipse.startup.FITTESTServerStartUp;

public class ClosingListener implements ISaveParticipant {

	@Override
	public void doneSaving(ISaveContext context) {
	}

	@Override
	public void prepareToSave(ISaveContext context) throws CoreException {
	}

	@Override
	public void rollback(ISaveContext context) {
		// TODO Auto-generated method stub

	}

	@Override
	public void saving(ISaveContext context) throws CoreException {
		
		if (context.getKind() == ISaveContext.FULL_SAVE)
			FITTESTServerStartUp.shutdownLocalSUTAgent();

	}

}
