package eu.fittest.eclipse.model.jobs.replay;

import eu.fittest.common.core.xml.TestCaseStatus;
import eu.fittest.common.core.xml.TestCaseVerdict;

public class TestCaseExecution {
	private String _testCaseName;
	private int _testCaseNumber;
	private TestCaseStatus _status;
	private String _sourceAgentId;
	private TestCaseVerdict _verdict;
	private String _message;

	
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((_sourceAgentId == null) ? 0 : _sourceAgentId.hashCode());
		result = prime * result
				+ ((_testCaseName == null) ? 0 : _testCaseName.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		TestCaseExecution other = (TestCaseExecution) obj;
		if (_sourceAgentId == null) {
			if (other._sourceAgentId != null)
				return false;
		} else if (!_sourceAgentId.equals(other._sourceAgentId))
			return false;
		if (_testCaseName == null) {
			if (other._testCaseName != null)
				return false;
		} else if (!_testCaseName.equals(other._testCaseName))
			return false;
		return true;
	}

	public TestCaseExecution(String sourceAgentId, String testCaseName, int testCaseNumber, TestCaseStatus status, TestCaseVerdict verdict, String message) {
		_testCaseName = testCaseName;
		_testCaseNumber = testCaseNumber;
		_status = status;
		_verdict = verdict;
		_sourceAgentId = sourceAgentId;
		_message = message;
	}
	
	public String getMessage(){
		return _message;
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
}
