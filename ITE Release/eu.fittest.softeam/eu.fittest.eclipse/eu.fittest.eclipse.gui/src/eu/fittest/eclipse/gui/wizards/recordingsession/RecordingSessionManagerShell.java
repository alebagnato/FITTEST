package eu.fittest.eclipse.gui.wizards.recordingsession;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.TimeZone;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.ProgressBar;
import org.eclipse.swt.widgets.Shell;

import eu.fittest.common.core.constants.FITTESTSingleton;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.eclipse.gui.wizards.AbstractSessionManager;
import eu.fittest.eclipse.model.jobs.JobStatus;
import eu.fittest.eclipse.model.jobs.record.RecordingSessionJob;

public class RecordingSessionManagerShell extends AbstractSessionManager {
	private RecordingSessionJob _job;
	private Button _btnStart;
	private Button _btnPause;
	private Button _btnStop;
	private Button _btnAbort;
	
	private DateTime _elapsedTime;
	private DateTime _startedAt;
	private ProgressBar _progress;
	
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
	public RecordingSessionManagerShell(Shell display, RecordingSessionJob job) {
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
		
		if(_job.getTotalDuration()>0){
			Label lblProgress = new Label(this, SWT.NONE);
			lblProgress.setText("Progress:");		
			_progress = new ProgressBar(this, SWT.NONE);
			_progress.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false, 1, 1));
		}
		else{
			_progress = null;
		}
		
		Group group = new Group(this, SWT.NONE);
		group.setLayout(new RowLayout(SWT.HORIZONTAL));
		group.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, false, false, 2, 1));
		
		_btnStart = new Button(group, SWT.NONE);
		_btnStart.setText("Start");
		
		_btnPause = new Button(group, SWT.NONE);
		_btnPause.setText("Pause");
		
		_btnStop = new Button(group, SWT.NONE);
		_btnStop.setText("Stop");
		
		_btnAbort = new Button(group, SWT.NONE);
		_btnAbort.setText("Abort");
		
		new Label(this, SWT.NONE);
		new Label(this, SWT.NONE);
		
		createContents();
		registerListeners();
	}

	/**
	 * Create contents of the shell.
	 */
	protected void createContents() {
		_elapsedTime.setTime(0,0,0);
		_startedAt.setTime(0,0,0);
		_elapsedTime.setEnabled(false);
		_startedAt.setEnabled(false);
		if(_job.getStartedAt()!=0){
			new TimeChangeListener(_startedAt).propertyChange(new PropertyChangeEvent(_job, "startedAt",0,_job.getStartedAt()));
			new TimeChangeListener(_elapsedTime, TimeZone.getTimeZone("GMT")).propertyChange(new PropertyChangeEvent(_job, "elapsedTime",0,_job.getElapsedTime()));
		}
		if(_progress!=null){
			new ProgressChangeListener(_progress).propertyChange(new PropertyChangeEvent(_job, "progress", 0, _job.getProgress()));
		}
		setState(_job.getStatus());
		pack();
	}
	
	private void registerListeners(){
		PropertyChangeListener listener =  new TimeChangeListener(_startedAt);
		_job.addPropertyChangeListener("startedAt", listener);
		_listeners.put("startedAt",listener);
		
		listener =  new TimeChangeListener(_elapsedTime,TimeZone.getTimeZone("GMT"));
		_job.addPropertyChangeListener("elapsedTime", listener);
		_listeners.put("elapsedTime",listener);
		
		if(_progress!=null){
			listener = new ProgressChangeListener(_progress);
			_job.addPropertyChangeListener("progress", listener);
			_listeners.put("progress",listener);
		}
		
		listener = new StatusChangeListener();
		_job.addPropertyChangeListener("status", listener);
		_listeners.put("status",listener);
		
		addShellListener(new ShellAdapter() {		
			@Override
			public void shellClosed(ShellEvent e) {
				clean();				
				dispose();
			}
		});
		
		_btnStop.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				try{
					_job.stop();
				} catch (FITTESTException ex) {
					MessageDialog.openError(getShell(), "Recording session error", ex.getMessage());
				}
			}
		});
		
		_btnPause.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				try{
					_job.pause();
				} catch (FITTESTException ex) {
					MessageDialog.openError(getShell(), "Recording session error", ex.getMessage());
				}
			}
		});
		
		_btnStart.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				_btnStart.setText("Starting");
				_btnStart.setEnabled(false);
				FITTESTSingleton.getThreadPool().execute(new Runnable() {
					public void run() {
						try{
							_job.start();
						} catch (FITTESTException ex) {
							MessageDialog.openError(getShell(), "Recording session error", ex.getMessage());
						}
					}
				});
				_btnStart.setText("Start");
			}
		});
		
		_btnAbort.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				try{
					_job.abort();
				} catch (FITTESTException ex) {
					MessageDialog.openError(getShell(), "Recording session error", ex.getMessage());
				}
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

}
