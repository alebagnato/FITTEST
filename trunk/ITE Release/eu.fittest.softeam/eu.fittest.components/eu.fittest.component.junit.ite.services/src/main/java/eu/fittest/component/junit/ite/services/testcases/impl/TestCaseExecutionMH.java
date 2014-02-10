package eu.fittest.component.junit.ite.services.testcases.impl;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.Connection;
import eu.fittest.common.core.xml.ExecutedTestCase;
import eu.fittest.common.services.message.AbstractMessageHandler;
import eu.fittest.component.junit.ite.services.testcases.spec.ITestCasesService;

public class TestCaseExecutionMH extends AbstractMessageHandler<ITestCasesService> {

	protected TestCaseExecutionMH(ITestCasesService service) {
		super(service);	
	}

	public synchronized void onReception(Connection connection, ExecutedTestCase message) throws FITTESTException {
		_service.testCaseExecution(message.getFrom(), message.getName(), message.getNumber(), message.getStatus(), message.getVerdict(), message.getMessage());
	}
}
