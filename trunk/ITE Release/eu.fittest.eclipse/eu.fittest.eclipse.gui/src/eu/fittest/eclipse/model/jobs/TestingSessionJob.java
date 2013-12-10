package eu.fittest.eclipse.model.jobs;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.Timer;
import java.util.TimerTask;

import eu.fittest.common.core.exception.FITTESTException;

public abstract class TestingSessionJob {
	public TestingSessionJob(String name) throws FITTESTException {
		_support = new PropertyChangeSupport(this);
		_name = name;
		_progress = 0;
		_elapsedTime = 0L;
		_startedAt = 0L;
		_timer = new Timer(name);
	}
	
	public void init() throws FITTESTException{
		setStatus(JobStatus.NEW);
	}

	protected Timer _timer;
	protected UpdateTask _updateTask;
	private String _name;
	protected PropertyChangeSupport _support;
	private Integer _progress;
	protected JobStatus _status;
	protected Long _elapsedTime;
	protected Long _startedAt;
	
	protected abstract class UpdateTask extends TimerTask{
		
	}
	
	abstract public void setStatus(JobStatus status) throws FITTESTException;
	
	public Long getElapsedTime() {
		return _elapsedTime;
	}
	
	public Long getStartedAt() {
		return _startedAt;
	}
	
	public JobStatus getStatus() {
		return _status;
	}
	
	public String getName() {
		return _name;
	}
	
	public void addPropertyChangeListener(String property, PropertyChangeListener l){
		_support.addPropertyChangeListener(property, l);
	}
	
	public void removePropertyChangeListener(String property, PropertyChangeListener l){
		_support.removePropertyChangeListener(property, l);
	}
	
	public Integer getProgress() {
		return _progress;
	}
	public void setProgress(Integer progress) {
		_support.firePropertyChange("progress", _progress, _progress = progress);
	}
	
	protected void setStartedAt(Long startedAt) {
		_support.firePropertyChange("startedAt", _startedAt, _startedAt= startedAt);
	}
	
	protected abstract void runToCompletion(JobEvent status) throws FITTESTException;
	
	public void start() throws FITTESTException{
		runToCompletion(JobEvent.Start);
	}
	
	public void stop() throws FITTESTException {
		runToCompletion(JobEvent.Stop);
	}

	public void pause() throws FITTESTException {
		runToCompletion(JobEvent.Pause);
	}

	public void abort() throws FITTESTException {
		runToCompletion(JobEvent.Abort);
	}
	
	public void resume() throws FITTESTException{
		runToCompletion(JobEvent.Start);
	}
}
