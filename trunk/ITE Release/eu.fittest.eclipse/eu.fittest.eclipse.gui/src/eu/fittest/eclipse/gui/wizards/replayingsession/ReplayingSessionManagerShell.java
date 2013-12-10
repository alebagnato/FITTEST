package eu.fittest.eclipse.gui.wizards.replayingsession;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.TimeZone;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.ProgressBar;
import org.eclipse.swt.widgets.Button;

import eu.fittest.eclipse.gui.wizards.AbstractSessionManager;
import eu.fittest.eclipse.model.jobs.JobStatus;
import eu.fittest.eclipse.model.jobs.replay.ReplayingSessionJob;

import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.layout.RowLayout;
import eu.fittest.common.core.exception.FITTESTException;

public class ReplayingSessionManagerShell extends AbstractSessionManager {
	private ReplayingSessionJob _job;
	private Button _btnStart;
	private Button _btnPause;
	private Button _btnStop;
	private Button _btnAbort;
	
	private DateTime _elapsedTime;
	private DateTime _startedAt;
	private ProgressBar _progress;
	private Label _failed;
	private Button _details;
	private ListViewer _detailedList;
	private Label _failedLbl;
	
	@Override
	protected void checkSubclass() {

	}

	private void setState(JobStatus status){
		switch(status){
		case ABORTED:
			_btnAbort.setEnabled(false);
			_btnStop.setEnabled(false);
			_btnPause.setEnabled(false);
			_btnStart.setEnabled(false);
			break;
		case COMPLETED:
			_btnAbort.setEnabled(false);
			_btnStop.setEnabled(false);
			_btnPause.setEnabled(false);
			_btnStart.setEnabled(false);
			break;
		case NEW:
			_btnStart.setEnabled(true);
			_btnAbort.setEnabled(false);
			_btnStop.setEnabled(false);
			_btnPause.setEnabled(false);
			break;
		case PAUSED:
			_btnAbort.setEnabled(true);
			_btnStop.setEnabled(true);
			_btnPause.setEnabled(false);
			_btnStart.setEnabled(true);
			break;
		case RUNNING:
			_btnAbort.setEnabled(true);
			_btnStop.setEnabled(true);
			_btnPause.setEnabled(true);
			_btnStart.setEnabled(false);
			break;
		}
	}
	
	/**
	 * Create the shell.
	 * @param display
	 */
	public ReplayingSessionManagerShell(Shell display, ReplayingSessionJob job) {
		super(display,job);
		
		_job = job;
		
		setText(_job.getName());
		setLayout(new GridLayout(2, false));
		
		Label lblStartedAt = new Label(this, SWT.NONE);
		lblStartedAt.setText("Started at:");
		
		_startedAt = new DateTime(this, SWT.BORDER | SWT.TIME);		
		
		Label lblElapsedTime = new Label(this, SWT.NONE);
		lblElapsedTime.setText("Elapsed time:");
		
		_elapsedTime = new DateTime(this, SWT.BORDER | SWT.TIME);
		
		
		Label lblProgress = new Label(this, SWT.NONE);
		lblProgress.setText("Progress:");		
		_progress = new ProgressBar(this, SWT.NONE);
		_progress.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false, 1, 1));
		
		
		Label details = new Label(this, SWT.NONE);
		details.setText("Details");
		
		createWithoutDetails();
		
		initializeContents();
		
		registerListeners();
	}

	private Group _commands=null;
	
	private void createWithoutDetails(){
		cleanWidgets();
		
		_details = new Button(this, SWT.DOWN | SWT.ARROW);
		_details.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 1, 1));
		_details.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				createWithDetails();
			}
		});
		
		createCommands();
		pack();
	}
	
	private void createCommands(){		
		_commands = new Group(this, SWT.NONE);
		_commands.setLayout(new RowLayout(SWT.HORIZONTAL));
		_commands.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, false, false, 2, 1));
		
		_btnStart = new Button(_commands, SWT.NONE);
		_btnStart.setText("Start");
		
		_btnPause = new Button(_commands, SWT.NONE);
		_btnPause.setText("Pause");
		
		_btnStop = new Button(_commands, SWT.NONE);
		_btnStop.setText("Stop");
		
		_btnAbort = new Button(_commands, SWT.NONE);
		_btnAbort.setText("Abort");
		
		_btnStop.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				try{
					_job.stop();
				} catch (FITTESTException ex) {
					MessageDialog.openError(getShell(), "Replaying session error", ex.getMessage());
				}
			}
		});
		
		_btnPause.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				try{
					_job.pause();
				} catch (FITTESTException ex) {
					MessageDialog.openError(getShell(), "Replaying session error", ex.getMessage());
				}
			}
		});
				
		_btnStart.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				try {
					_job.start();
				} catch (FITTESTException ex) {
					MessageDialog.openError(getShell(), "Replaying session error", ex.getMessage());
				}
			}
		});
		
		_btnAbort.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				try {
					_job.abort();
				} catch (FITTESTException ex) {
					MessageDialog.openError(getShell(), "Replaying session error", ex.getMessage());
				}
			}
		});
		
		setState(_job.getStatus());
	}
	
	private void cleanWidgets(){
		if(_commands!=null){
			_commands.dispose();
			_commands = null;
		}
		if(_detailedList!=null){
			_detailedList.getControl().dispose();
			_detailedList = null;
		}
		if(_details!=null){
			_details.dispose();
			_details = null;
		}
		if(_failed!=null){
			_failed.dispose();
			_failedLbl.dispose();
			_failed = null;
			_failedLbl = null;
		}

	}
	
	private void createWithDetails(){
		cleanWidgets();
		
		_details = new Button(this, SWT.UP | SWT.ARROW);
		_details.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 1, 1));
		_details.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				createWithoutDetails();
			}
		});
		
		_failedLbl = new Label(this, SWT.NONE);
		_failedLbl.setText("Failed:");

		_failed = new Label(this, SWT.NONE);
		_failed.setText(_job.getNbFailedTestCases()+"/"+_job.getNbExecutedTestCases());
		_failed.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false, 1, 1));
		
		_detailedList = new ListViewer(this, SWT.SINGLE | SWT.V_SCROLL | SWT.BORDER);
		_detailedList.getControl().setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 2, 1));
		_detailedList.setContentProvider(new ExecutedTestCaseContentProvider());
		_detailedList.setLabelProvider(new ExecutedTestCaseLabelProvider());
		_detailedList.setInput(_job.getExecutedTestCaseModel());
		
		createCommands();
		pack();
	}
	
	
	/**
	 * Create contents of the shell.
	 */
	protected void initializeContents() {		
		
		_elapsedTime.setTime(0,0, 0);
		_startedAt.setTime(0,0,0);
		_elapsedTime.setEnabled(false);
		_startedAt.setEnabled(false);
		if(_job.getStartedAt()!=0){
			new TimeChangeListener(_startedAt).propertyChange(new PropertyChangeEvent(_job, "startedAt",0,_job.getStartedAt()));
			new TimeChangeListener(_elapsedTime, TimeZone.getTimeZone("GMT")).propertyChange(new PropertyChangeEvent(_job, "elapsedTime",0,_job.getElapsedTime()));
		}
		new ProgressChangeListener(_progress).propertyChange(new PropertyChangeEvent(_job, "progress", 0, _job.getProgress()));
		FailedChangeListener l = new FailedChangeListener();
		l.propertyChange(new PropertyChangeEvent(_job, ReplayingSessionJob.NB_EXECUTED_TEST_CASES,0, _job.getNbExecutedTestCases()));
		l.propertyChange(new PropertyChangeEvent(_job, ReplayingSessionJob.NB_FAILED_TEST_CASES,0, _job.getNbFailedTestCases()));
	}
	
	private void registerListeners(){
		PropertyChangeListener listener =  new TimeChangeListener(_startedAt);
		_job.addPropertyChangeListener("startedAt", listener);
		_listeners.put("startedAt", listener);
		
		listener =  new TimeChangeListener(_elapsedTime,TimeZone.getTimeZone("GMT"));
		_job.addPropertyChangeListener("elapsedTime", listener);
		_listeners.put("elapsedTime",listener);
		
		listener = new ProgressChangeListener(_progress);
		_job.addPropertyChangeListener("progress", listener);
		_listeners.put( "progress",listener);
		
		listener = new StatusChangeListener();
		_job.addPropertyChangeListener("status", listener);
		_listeners.put( "status",listener);
		
		listener =  new FailedChangeListener();
		_job.addPropertyChangeListener(ReplayingSessionJob.NB_FAILED_TEST_CASES, listener);
		_job.addPropertyChangeListener(ReplayingSessionJob.NB_EXECUTED_TEST_CASES, listener);
		_job.addPropertyChangeListener(ReplayingSessionJob.NB_TEST_CASE, listener);
		_listeners.put(ReplayingSessionJob.NB_FAILED_TEST_CASES,listener);
		_listeners.put(ReplayingSessionJob.NB_EXECUTED_TEST_CASES,listener);
		_listeners.put(ReplayingSessionJob.NB_TEST_CASE,listener);
		
		addShellListener(new ShellAdapter() {		
			@Override
			public void shellClosed(ShellEvent e) {
				clean();				
				dispose();
			}
		});
		
	}
	
	private class StatusChangeListener implements PropertyChangeListener{
		@Override
		public void propertyChange(final PropertyChangeEvent event) {
			Display.getDefault().asyncExec(new Runnable() {
				
				@Override
				public void run() {
					setState((JobStatus)event.getNewValue());						
				}
			});
			
		}
	}
	
	private class FailedChangeListener implements PropertyChangeListener{
		private int _total = 0;
		private int _nbFailed = 0;
		@Override
		public void propertyChange(PropertyChangeEvent event) {
			if(event.getPropertyName().equals(ReplayingSessionJob.NB_EXECUTED_TEST_CASES)){
				_total = (Integer)event.getNewValue();
			}
			else if(event.getPropertyName().equals(ReplayingSessionJob.NB_FAILED_TEST_CASES)){
				_nbFailed = (Integer)event.getNewValue();			
			}
			if(_failed!=null){
				Display.getDefault().asyncExec(new Runnable() {			
					@Override
					public void run() {
						_detailedList.refresh();
						_failed.setText(_nbFailed+"/"+_total);	
						_failed.getShell().layout();
					}
				});
			}
			
		}
	}
	
	public static void main(String[] args) {
		final Display display = new Display();
		final Shell shell = new Shell(display);
		shell.open();
		new ReplayingSessionManagerShell(shell, null).open();
		while (!shell.isDisposed()) {
			if (!display.readAndDispatch()) display.sleep();
		}
	}

}
