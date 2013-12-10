package eu.fittest.eclipse.gui.views.job;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import org.eclipse.swt.widgets.Display;

import eu.fittest.eclipse.gui.utils.Viewer;

public class TableChangeListener implements PropertyChangeListener{
	@Override
	public void propertyChange(PropertyChangeEvent evt) {
		Display.getDefault().asyncExec(new Runnable() {
			
			@Override
			public void run() {
				if (Viewer.getView(JobsView.VIEW_ID) != null) 
					((JobsView)Viewer.getView(JobsView.VIEW_ID)).refresh();				
			}
		});				
	}
}
