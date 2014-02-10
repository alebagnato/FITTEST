package eu.fittest.eclipse.gui.views.job;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

import eu.fittest.eclipse.gui.utils.Viewer;
import eu.fittest.eclipse.gui.wizards.recordingsession.RecordingSessionManagerShell;
import eu.fittest.eclipse.gui.wizards.replayingsession.ReplayingSessionManagerShell;
import eu.fittest.eclipse.model.jobs.JobModel;
import eu.fittest.eclipse.model.jobs.TestingSessionJob;
import eu.fittest.eclipse.model.jobs.record.RecordingSessionJob;
import eu.fittest.eclipse.model.jobs.replay.ReplayingSessionJob;

public class JobsView extends ViewPart {
	public static String VIEW_ID = "eu.fittest.eclipse.gui.views.job";
	private TableViewer _jobs;
	
	public JobsView() {
		// TODO Auto-generated constructor stub
	}
	
	public void refresh(){
		_jobs.refresh();
	}

	@Override
	public void createPartControl(Composite parent) {
		_jobs = new TableViewer(parent, SWT.MULTI | SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION);
		 createColumns();
		 _jobs.getTable().setLinesVisible (true);
		 _jobs.getTable().setHeaderVisible (true);
		 _jobs.setContentProvider(new ArrayContentProvider());
		 _jobs.setInput(JobModel.INSTANCE.getJobs());
		 
		 _jobs.addDoubleClickListener(new IDoubleClickListener() {
			
			@Override
			public void doubleClick(DoubleClickEvent event) {	
				TestingSessionJob job = (TestingSessionJob) _jobs.getTable().getSelection()[0].getData();
				if(job instanceof RecordingSessionJob){
					new RecordingSessionManagerShell(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), (RecordingSessionJob) job).open();
				}
				else if(job instanceof ReplayingSessionJob){
					new ReplayingSessionManagerShell(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), (ReplayingSessionJob) job).open();
				}
			}
		 });
		 _jobs.getTable().setToolTipText("Double-click on a row to open control commands");
	}

	@Override
	public void setFocus() {
		// TODO Auto-generated method stub

	}

	
	private void createColumns() {
		String[] titles = {"Name", "Status", "Progress"};
		int[] bounds = { 300, 100, 100};

		TableViewerColumn col = Viewer.createTableViewerColumn(_jobs, titles[0], bounds[0], 0);
		col.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(Object element) {
				TestingSessionJob job = (TestingSessionJob) element;
				return job.getName();
			}
		});

		col = Viewer.createTableViewerColumn(_jobs, titles[1], bounds[1], 1);
		col.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(Object element) {
				TestingSessionJob job = (TestingSessionJob) element;
				return job.getStatus().toString();
			}
		});

		col = Viewer.createTableViewerColumn(_jobs, titles[2], bounds[2], 2);
		col.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(Object element) {
				TestingSessionJob job = (TestingSessionJob) element;
				if(job.getProgress()<0) return "N/A";
				else return job.getProgress().toString()+"%";
			}
		});
		
	}

}
