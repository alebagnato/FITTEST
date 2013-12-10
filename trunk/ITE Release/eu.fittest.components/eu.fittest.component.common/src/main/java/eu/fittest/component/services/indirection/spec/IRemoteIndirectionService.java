package eu.fittest.component.services.indirection.spec;

import java.util.List;

import eu.fittest.common.core.service.IRemoteService;
import eu.fittest.common.core.xml.Initialize.Parameter;

public interface IRemoteIndirectionService extends IRemoteService{
	void initialize(List<Parameter> list);
	void start();
	void stop();
	void terminate();
	void registered();
	void resourceAvailable(String comingFrom, String resource);
}
