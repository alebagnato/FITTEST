package eu.fittest.eclipse.model.jobs.replay;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.xml.HUTType;
import eu.fittest.component.junit.ite.services.testcases.spec.ITestCasesListener;
import eu.fittest.component.junit.ite.services.testcases.spec.ITestCasesService;
import eu.fittest.component.junit.ite.services.testcases.spec.TestCasesEvent;
import eu.fittest.eclipse.component.extensionpoint.IFITTESTComponentManager;
import eu.fittest.eclipse.gui.wizards.replayingsession.ExecutedTestCaseLabelProvider;
import eu.fittest.eclipse.model.environment.Host;
import eu.fittest.eclipse.model.jobs.HUTManager;
import eu.fittest.eclipse.model.jobs.JobEvent;
import eu.fittest.eclipse.model.jobs.JobStatus;
import eu.fittest.eclipse.model.jobs.TestingSessionJob;
import eu.fittest.eclipse.startup.FITTESTServerStartUp;

public class ReplayingSessionJob extends TestingSessionJob implements ITestCasesListener{
	private static final long REFRESH_RATE = 1000;
	private HUTManager _hutManager;
	private List<TestCaseExecution> _executedTestCaseModel;
	private IFolder _sessionFolder;
	
	private int _totalNbTestCases = 0;
	private int _nbExecutedTestCases = 0;
	private int _nbFailedTestCases = 0;
	private int _nbClientHuts = 0; 
	
	public static String NB_FAILED_TEST_CASES = "nbFailedTestCases";
	public static String NB_EXECUTED_TEST_CASES = "nbExecutedTestCases";
	public static String NB_TEST_CASE = "newTestCase";
	
	public int getNbExecutedTestCases(){
		return _nbExecutedTestCases;
	}
	
	public int getNbFailedTestCases(){
		return _nbFailedTestCases;
	}
	
	private class UpdateReplayingSession extends UpdateTask{
		@Override
		public void run() {
			long current = _elapsedTime+ System.currentTimeMillis() - _startedAt;
			_support.firePropertyChange("elapsedTime", 0, current);		
		}
	}
	
	public ReplayingSessionJob(IFolder session, List<Host> HUTs, List<IFITTESTComponentManager> componentManagers) throws FITTESTException {
		this(session, HUTs, componentManagers, new ReplayingHUTManager(session, componentManagers, HUTs));
	}
	
	protected ReplayingSessionJob(IFolder session, List<Host> HUTs, List<IFITTESTComponentManager> componentManagers, HUTManager manager) throws FITTESTException{
		super(session.getName());
		_sessionFolder = session;
		_hutManager = manager;
		_executedTestCaseModel = new ArrayList<TestCaseExecution>();
		for(Host h: HUTs){
			if(h.getType().equals(HUTType.CLIENT) || h.getType().equals(HUTType.MIXED)){
				_nbClientHuts++;
			}
		}
	}
	
	public void incrementExecuted() throws FITTESTException{
		int old = _nbExecutedTestCases;
		_nbExecutedTestCases++;
		_support.firePropertyChange(NB_EXECUTED_TEST_CASES, old, _nbExecutedTestCases);
		setProgress(new Long(_nbExecutedTestCases*100L/_totalNbTestCases).intValue());
		if(getProgress().intValue()>=100) runToCompletion(JobEvent.Stop);
	}

	protected void runToCompletion(JobEvent event) throws FITTESTException{
		switch (event) {
		case Abort:
			_hutManager.stopHUTs();
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
			_hutManager.terminateHUTs();
			setStatus(JobStatus.ABORTED);
			break;
		case Start:
			switch (_status) {
			case NEW:
				_hutManager.initializeHUTs();
				_hutManager.startHUTs();
				setStatus(JobStatus.RUNNING);
				break;
			case PAUSED:
				_hutManager.startHUTs();
				setStatus(JobStatus.RUNNING);
				break;
			default:
				break;
			}
			break;
		case Stop:
			_hutManager.stopHUTs();
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
			_hutManager.terminateHUTs();
			setStatus(JobStatus.COMPLETED);
			break;
		case Pause:
			_hutManager.stopHUTs();
			setStatus(JobStatus.PAUSED);
			break;
		default:
			break;
		}
	}
	
	public void setStatus(JobStatus status) throws FITTESTException {
		_support.firePropertyChange("status", _status , _status = status);
		switch (status) {
		case NEW:
			FITTESTServerStartUp.getITEAgentInstance().getServiceRegistry().findService(ITestCasesService.class).addServiceListener(this);
			break;
		case RUNNING:
			runningState();
			break;
		case ABORTED:
			_elapsedTime += System.currentTimeMillis() - _startedAt;
			_updateTask.cancel();
			FITTESTServerStartUp.getITEAgentInstance().getServiceRegistry().findService(ITestCasesService.class).removeServiceListener(this);
			try {
				_sessionFolder.delete(true, new NullProgressMonitor());
			} catch (CoreException e) {
				throw new FITTESTException(e.getMessage());
			}
			break;
		case COMPLETED:
			_elapsedTime += System.currentTimeMillis() - _startedAt;
			_updateTask.cancel();
			FITTESTServerStartUp.getITEAgentInstance().getServiceRegistry().findService(ITestCasesService.class).removeServiceListener(this);
			try {
				saveResults();
			} catch (FileNotFoundException e) {
				throw new FITTESTException(e.getMessage());
			} catch (CoreException e) {
				throw new FITTESTException(e.getMessage());
			}
			break;
		case PAUSED:
			_updateTask.cancel();
			_elapsedTime += System.currentTimeMillis() - _startedAt;
			break;
		default:
			break;
		}
	}

	private void runningState() {
		setStartedAt(System.currentTimeMillis());
		_updateTask = new UpdateReplayingSession();
		_timer.scheduleAtFixedRate(_updateTask, REFRESH_RATE, REFRESH_RATE);
	}

	@Override
	public synchronized void incomingEvent(TestCasesEvent event) {
		if(event.getTestCaseName() == null && _totalNbTestCases==0){//no name so starting the suite
			_totalNbTestCases = event.getTestCaseNumber() * _nbClientHuts;
			_support.firePropertyChange("totalNbTestCases", 0, _totalNbTestCases);
		}
		else{
			TestCaseExecution tce = new TestCaseExecution(event.getSourceAgentId(), event.getTestCaseName(), event.getTestCaseNumber(),
					event.getStatus(), event.getVerdict(),event.getMessage());
			switch (event.getStatus()) {
			case FINISHED:
				try {
					int index = _executedTestCaseModel.indexOf(tce);
					_executedTestCaseModel.set(index, tce);
					incrementExecuted();
					switch(event.getVerdict()){
					case FAILED:
					case ERROR: // by urueda
						int old = _nbFailedTestCases;
						_nbFailedTestCases++;
						_support.firePropertyChange(NB_FAILED_TEST_CASES, old, _nbFailedTestCases);
						break;
					case PASSED:
						break;
						default:
							break;
					}
				} catch (FITTESTException e) {
					Logger.getAnonymousLogger().log(Level.INFO, e.getMessage());
				}
				break;
			case STARTED:
				_executedTestCaseModel.add(0, tce);
				_support.firePropertyChange(NB_TEST_CASE, null, tce);
				break;
			default:
				break;
			}
		}
	}

	public List<TestCaseExecution> getExecutedTestCaseModel() {
		return Collections.unmodifiableList(_executedTestCaseModel);
	}
	
	private void saveResults() throws FileNotFoundException, CoreException{
		ExecutedTestCaseLabelProvider label = new ExecutedTestCaseLabelProvider();
		IFile report = _sessionFolder.getFile("report.txt");
		PrintWriter reportWriter = new PrintWriter(new FileOutputStream(report.getLocation().toOSString())); 
		for(TestCaseExecution tce : _executedTestCaseModel){
			reportWriter.println(label.getText(tce));
		}
		reportWriter.close();
		_sessionFolder.refreshLocal(IFolder.DEPTH_ONE, new NullProgressMonitor());
	}

}
