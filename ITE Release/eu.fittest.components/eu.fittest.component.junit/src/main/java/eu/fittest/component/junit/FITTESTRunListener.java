package eu.fittest.component.junit;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.runner.Description;
import org.junit.runner.notification.Failure;
import org.junit.runner.notification.RunListener;

import eu.fittest.common.core.connection.spec.IConnectionService;
import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.identity.spec.IIdentityService;
import eu.fittest.common.core.service.IServiceRegistry;
import eu.fittest.common.core.xml.ExecutedTestCase;
import eu.fittest.common.core.xml.Message;
import eu.fittest.common.core.xml.TestCaseStatus;
import eu.fittest.common.core.xml.TestCaseVerdict;

public class FITTESTRunListener extends RunListener{
	private int _counter;
	private int _size;
	private IServiceRegistry _registry;
	private TestCaseVerdict _verdict;
	private boolean _interrupted;
	private String _fittestEntity;
	private String _message;
	
	public void interrupt(){
		_interrupted = true;
	}
	
	public FITTESTRunListener(IServiceRegistry registry, String fittestEntity){
		_registry = registry;
		_interrupted = false;
		_fittestEntity = fittestEntity;
	}
	
	@Override
	public void testRunStarted(Description description) throws Exception {
		_counter = 0;
		_size = description.testCount();
		Logger.getAnonymousLogger().log(Level.INFO, "Test count "+description.testCount());
		
		ExecutedTestCase tcex = FITTESTSingleton.getObjectFactory().createExecutedTestCase();
		tcex.setNumber(_size);
		sendMessage(tcex);
	}
	
	@Override
	public void testStarted(Description description) throws Exception{
		if(_interrupted) throw new Error(new InterruptedException("Interrupted by ITE"));
		_counter++;
		_verdict = null;
		_message = null;
		Logger.getAnonymousLogger().log(Level.INFO, "Starting "+description.getClassName()+"."+description.getMethodName()+" ("+_counter+"/"+_size+")");
		
		ExecutedTestCase tcex = FITTESTSingleton.getObjectFactory().createExecutedTestCase();
		tcex.setName(description.getClassName()+"."+description.getMethodName());
		tcex.setNumber(_counter);
		tcex.setStatus(TestCaseStatus.STARTED);
		sendMessage(tcex);		
	}
	
	@Override
	public void testFailure(Failure failure) throws Exception {
		Logger.getAnonymousLogger().log(Level.INFO,"Failure "+failure.getDescription().getClassName()+"."+failure.getDescription().getMethodName()+" ("+_counter+"/"+_size+"): "+failure.getMessage());
		// begin by urueda
		Logger.getAnonymousLogger().log(Level.INFO, "Failure exception = " + failure.getException());
		if ((failure.getException() != null) &&
			(failure.getException() instanceof java.lang.RuntimeException)) {	
			_verdict = TestCaseVerdict.ERROR;
		}
		else { // end by urueda
			_verdict = TestCaseVerdict.FAILED;
		}
		_message = failure.getMessage();
	}
	
	@Override
	public void testFinished(Description description) throws Exception {
		Logger.getAnonymousLogger().log(Level.INFO,"Finishing "+description.getClassName()+"."+description.getMethodName()+" ("+_counter+"/"+_size+")");
		
		ExecutedTestCase tcex = FITTESTSingleton.getObjectFactory().createExecutedTestCase();
		tcex.setName(description.getClassName()+"."+description.getMethodName());
		tcex.setNumber(_counter);
		tcex.setStatus(TestCaseStatus.FINISHED);
		tcex.setVerdict(_verdict==null?TestCaseVerdict.PASSED:_verdict);
		tcex.setMessage(_message);
		sendMessage(tcex);	
	}
	
	private void sendMessage(Message m){
		m.setFrom(_registry.findService(IIdentityService.class).getMyIdentity());
		m.setTo(_fittestEntity);
		
		try {
			_registry.findService(IConnectionService.class).sendMessage(m);
		} catch (FITTESTException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
		}
	}
}
