package eu.fittest.component.optimizer;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import eu.fittest.common.core.exception.FITTESTException;

public class MockOptimizer implements IOptimizerProvidedServices{
	private IOptimizerRequiredServices _rs;
	private String _targetClasses;
	private File _outputFolder;
	private boolean _interrupted;
	
	public MockOptimizer() {

	}

	@Override
	public void setOptimizerRequiredServices(IOptimizerRequiredServices requiredServices) {
		_rs = requiredServices;
	}

	@Override
	public void run() {
		Properties p = new Properties();
		p.put("contest.targetClasses", _targetClasses);
		//for(int i=0;i<101;i+=10){
		if(!_interrupted){
			//_rs.setProgress(i);
		_rs.setProgress(0);
			try {
				_rs.execute(p);
				try {
					Thread.sleep(1000L);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			} catch (FITTESTException e) {
				Logger.getAnonymousLogger().log(Level.SEVERE,e.getMessage());
			}
		}
		//}
		try {
			FileOutputStream fos = new FileOutputStream(new File(_outputFolder,"KingProperties.properties"));
			p.store(fos, "MockOptimizer inside");
			fos.close();
			_rs.setProgress(100);//this must be the very last action because it triggers the end of the session
		} catch (FileNotFoundException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
		} catch (IOException e) {
			Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
		}
	}

	@Override
	public void interrupt() {
		_interrupted = true;
	}

	@Override
	public void setOutputFolder(File outputFolder) {
		_outputFolder = outputFolder;		
	}

	@Override
	public void setSeedProperties(Properties properties) {
		_targetClasses = properties.getProperty(IOptimizerProvidedServices.TARGET_CLASSES);
	}

	@Override
	public void setRandomNumberSeed(Long seed) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void setSearchAlgorithm(SearchAlgorithm value) {
		// TODO Auto-generated method stub
		
	}

}
