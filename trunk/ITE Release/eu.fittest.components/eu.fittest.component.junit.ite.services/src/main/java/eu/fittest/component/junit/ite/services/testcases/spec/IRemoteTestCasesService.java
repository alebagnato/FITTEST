package eu.fittest.component.junit.ite.services.testcases.spec;

import eu.fittest.common.core.service.IRemoteService;
import eu.fittest.common.core.xml.TestCaseStatus;
import eu.fittest.common.core.xml.TestCaseVerdict;

public interface IRemoteTestCasesService extends IRemoteService {
	void testCaseExecution(String agentId, String tcName, int tcNumber, TestCaseStatus status, TestCaseVerdict verdict, String message);
}
