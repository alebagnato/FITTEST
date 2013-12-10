package eu.fittest.component.junit;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.runner.JUnitCore;
import org.junit.runner.Result;

import eu.fittest.common.core.service.IServiceRegistry;

public class JUnitTestCaseTask implements Runnable{
	private Class<?>[] _testcases;
	private FITTESTRunListener _l;
	
	public void interrupt(){
		_l.interrupt();
	}
	
	public JUnitTestCaseTask(IServiceRegistry registry, Class<?>[] testcases, String fittestEntity) {
		_l = new FITTESTRunListener(registry, fittestEntity);
		_testcases = testcases;
	}
	
	public void run() {
		JUnitCore runner = new JUnitCore();
		runner.addListener(_l);
		try{
			Result result = runner.run(_testcases);
			Logger.getAnonymousLogger().log(Level.INFO, "Passed: "+(result.getRunCount() - result.getFailureCount())+"/"+result.getRunCount());
		}
		catch(Error e){
			if(e.getCause() instanceof InterruptedException){
				Logger.getAnonymousLogger().log(Level.INFO, "JUnitTestTask interrupted");
			}
		}
	}

}
