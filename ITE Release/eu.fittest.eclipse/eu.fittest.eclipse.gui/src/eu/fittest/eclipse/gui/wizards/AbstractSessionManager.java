package eu.fittest.eclipse.gui.wizards;

import java.awt.Toolkit;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.ProgressBar;
import org.eclipse.swt.widgets.Shell;

import eu.fittest.eclipse.model.jobs.TestingSessionJob;

public abstract class AbstractSessionManager extends Shell{
	protected Map<String,PropertyChangeListener> _listeners;
	private TestingSessionJob _job;
	
	public AbstractSessionManager(Shell display, TestingSessionJob job) {
		super(display, SWT.SHELL_TRIM);
		_job = job;
		_listeners = new HashMap<String,PropertyChangeListener>();
	}
	
	protected void clean() {
		for(Entry<String,PropertyChangeListener> entry:_listeners.entrySet()){
			_job.removePropertyChangeListener(entry.getKey(),entry.getValue());
		}
		_listeners.clear();
	}
	
	protected class TimeChangeListener implements PropertyChangeListener{
		private DateTime _time;
		private TimeZone _zone;
		
		public TimeChangeListener(DateTime time, TimeZone zone) {
			_time = time;
			_zone = zone;
		}
		
		public TimeChangeListener(DateTime time) {
			this(time, TimeZone.getDefault());
		}
		
		@Override
		public void propertyChange(final PropertyChangeEvent evt) {
			Display.getDefault().asyncExec(new Runnable() {
				@Override
				public void run() {
					Long time = (Long) evt.getNewValue();
					Calendar date = Calendar.getInstance(_zone);
					date.setTimeInMillis(time);
					_time.setSeconds(date.get(Calendar.SECOND));
					_time.setMinutes(date.get(Calendar.MINUTE));
					_time.setHours(date.get(Calendar.HOUR_OF_DAY));		
				}
			});								
		}
	}
	
	protected class ProgressChangeListener implements PropertyChangeListener{
		private ProgressBar _progress;
		
		public ProgressChangeListener(ProgressBar progress) {
			_progress = progress;
		}
		@Override
		public void propertyChange(final PropertyChangeEvent event) {
			Display.getDefault().asyncExec(new Runnable() {
				
				@Override
				public void run() {
					_progress.setSelection((Integer)event.getNewValue());
					if(((Integer)event.getNewValue()).intValue()>=100){
						Toolkit.getDefaultToolkit().beep();
					}
				}
			});								
		}
	}
}
