package eu.fittest.eclipse.log2xml.actions;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.navigator.CommonNavigator;
import org.eclipse.ui.navigator.CommonViewer;
import org.eclipse.ui.progress.UIJob;
import org.eclipse.ui.services.IEvaluationService;

public class XMLSelectionListener implements ISelectionChangedListener {

	private Viewer viewer;
	 
	public void dispose() {
		if (viewer != null) {
			viewer.removeSelectionChangedListener(this);
		}
	}
	
	public void hookOnViewer(final String viewerId) {
		IWorkbench workbench = PlatformUI.getWorkbench();
		if (viewerId != null && workbench != null && workbench.getDisplay() != null) {
			Display display = workbench.getDisplay();
			Thread displayThread = display.getThread();
			if (workbench.isStarting() || !Thread.currentThread().equals(displayThread)) {
				// while workbench is starting defer hooking until later
				UIJob job = new UIJob(display, "viewer hooker") {
					@Override
					public IStatus runInUIThread(IProgressMonitor monitor) {
						hookOnViewer(viewerId);
						return Status.OK_STATUS;
					}
				};
				job.schedule(250);
			} else if (viewerId != null) {
				CommonNavigator navigator =(CommonNavigator) eu.fittest.eclipse.gui.utils.Viewer.getView(viewerId); 
				if(navigator!=null){
					CommonViewer viewer = navigator.getCommonViewer();
					if (viewer != null) {
						if (this.viewer != null) {
							this.viewer.removeSelectionChangedListener(this);
						}
						requestRefresh();
						viewer.addSelectionChangedListener(this);
						this.viewer = viewer;
					}
				}
			}
		}
	}
	
	public void selectionChanged(SelectionChangedEvent event) {
		requestRefresh();
	}
 
	protected void requestRefresh() {
		IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
		IEvaluationService evaluationService = (IEvaluationService) window.getService(IEvaluationService.class);
		if (evaluationService != null) {
			evaluationService.requestEvaluation("eu.fittest.eclipse.log2xml.members_extension");
		}
	}

}
