package eu.fittest.agent.ite.services.progress.spec;


import eu.fittest.common.core.service.IRemoteService;


public interface IRemoteProgressService extends IRemoteService {
	void progress(String taskName, int value);
}
