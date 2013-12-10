package eu.fittest.agent.ite.services.httpserver.spec;

import eu.fittest.common.core.exception.FITTESTException;




public interface ILocalHTTPServerService {
	void start() throws FITTESTException;
	void stop() throws FITTESTException;
}
