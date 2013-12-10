package eu.fittest.component.junit.ite.services.testcases.impl;

import eu.fittest.common.core.service.AbstractService;
import eu.fittest.common.core.xml.TestCaseStatus;
import eu.fittest.common.core.xml.TestCaseVerdict;
import eu.fittest.component.junit.ite.services.testcases.spec.ITestCasesService;
import eu.fittest.component.junit.ite.services.testcases.spec.TestCasesEvent;

public class TestCasesServiceImpl extends AbstractService implements ITestCasesService {

	public TestCasesServiceImpl() {
		_handlers.add(new TestCaseExecutionMH(this));
	}
	
	public String getName() {
		return ITestCasesService.class.getName();
	}

	public void testCaseExecution(String agentId, String tcName, int tcNumber,
			TestCaseStatus status, TestCaseVerdict verdict, String message) {
		fireEvent(new TestCasesEvent(this, agentId, tcName, tcNumber, status, verdict, message));
	}
}
