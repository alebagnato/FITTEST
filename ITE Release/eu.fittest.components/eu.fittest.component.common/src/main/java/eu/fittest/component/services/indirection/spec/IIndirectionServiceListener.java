package eu.fittest.component.services.indirection.spec;

import java.util.List;

import eu.fittest.common.core.xml.Initialize.Parameter;

public interface IIndirectionServiceListener{
	void registered();
	void initialize(List<Parameter> parameters);
	void start();
	void stop();
	void terminate();
	void resourceAvailable(String comingFrom, String resource);
}
