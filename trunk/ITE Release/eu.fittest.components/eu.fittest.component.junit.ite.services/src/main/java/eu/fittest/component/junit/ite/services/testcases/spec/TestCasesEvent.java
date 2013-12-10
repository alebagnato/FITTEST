package eu.fittest.component.junit.ite.services.testcases.spec;

import eu.fittest.common.core.service.ServiceEvent;
import eu.fittest.common.core.xml.TestCaseStatus;
import eu.fittest.common.core.xml.TestCaseVerdict;

public class TestCasesEvent extends ServiceEvent<ILocalTestCasesService>{
	private String _testCaseName;
	private int _testCaseNumber;
	private TestCaseStatus _status;
	private String _sourceAgentId;
	private TestCaseVerdict _verdict;
	private String _message;
	
	public TestCasesEvent(ILocalTestCasesService source, String sourceAgentId, String testCaseName, int testCaseNumber, TestCaseStatus status, TestCaseVerdict verdict, String message) {
		super(source);
		_testCaseName = testCaseName;
		_testCaseNumber = testCaseNumber;
		_status = status;
		_verdict = verdict;
		_sourceAgentId = sourceAgentId;
		_message = message;
	}

	public String getTestCaseName() {
		return _testCaseName;
	}

	public int getTestCaseNumber() {
		return _testCaseNumber;
	}

	public TestCaseStatus getStatus() {
		return _status;
	}

	public String getSourceAgentId() {
		return _sourceAgentId;
	}

	public TestCaseVerdict getVerdict() {
		return _verdict;
	}
	
	public String getMessage(){
		return _message;
	}
	
}
