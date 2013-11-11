package eu.fittest.agent.ite.services.progress.impl;

import eu.fittest.agent.ite.services.progress.spec.IProgressService;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.Progress;
import eu.fittest.common.services.message.AbstractMessageHandler;

public class ProgressMH extends AbstractMessageHandler<IProgressService> {

    public ProgressMH(IProgressService service) {
        super(service);
    }

    public synchronized void onReception(Connection connection, Progress message) throws FITTESTException {
        getService().progress(message.getTaskId(), message.getProgress());
    }
    
}
