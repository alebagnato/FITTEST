package eu.fittest.agent.sut.services.deployment.spec;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.IRemoteService;

public interface IRemoteDeploymentService extends IRemoteService{

    void deploy(final String resource) throws FITTESTException ;

}
