package eu.fittest.eclipse.model.jobs.replay;

import java.util.List;

import org.eclipse.core.resources.IFolder;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.eclipse.component.extensionpoint.IFITTESTComponentManager;
import eu.fittest.eclipse.model.environment.Host;
import eu.fittest.eclipse.model.jobs.HUTManager;
import eu.fittest.eclipse.model.jobs.SessionType;

public class ReplayingHUTManager extends HUTManager {

	public ReplayingHUTManager(IFolder session,
			List<IFITTESTComponentManager> componentManagers, List<Host> huts) throws FITTESTException {
		super(session, componentManagers, huts, SessionType.ReplayingSession);
	}

}
