package eu.fittest.agent.ite.services.progress.impl;

import eu.fittest.agent.ite.services.progress.spec.IProgressService;
import eu.fittest.agent.ite.services.progress.spec.IProgressServiceListener;
import eu.fittest.agent.ite.services.progress.spec.ProgressEvent;
import eu.fittest.common.core.service.AbstractService;

public class ProgressServiceImpl extends AbstractService implements IProgressService{

	public ProgressServiceImpl() {
		super();
		_handlers.add(new ProgressMH(this));
	}
	
	@Override
	public String getName() {
		return IProgressService.class.getName();
	}

	@Override
	public void addProgressListener(IProgressServiceListener listener) {
		addServiceListener(listener);
	}

	@Override
	public void removeProgressListener(IProgressServiceListener listener) {
		removeServiceListener(listener);
	}

	@Override
	public void progress(String taskId, int value) {
		fireEvent(new ProgressEvent(this, taskId, value));
	}

}
