package eu.fittest.fbk.efsm2ct.plugin.views;

import org.eclipse.jface.action.Action;
import org.eclipse.ui.actions.ActionFactory.IWorkbenchAction;

import eu.fittest.fbk.efsm2ct.plugin.Activator;

public class LogViewClearAction extends Action implements IWorkbenchAction {

	private static final String ID = "eu.fittest.fbk.efsm2ct.plugin.views.LogViewClearAction";
	private LogView logView;

	public LogViewClearAction(LogView view) {
		setId(ID);
		this.logView = view;
		setText("Clear view");
		setImageDescriptor(Activator.getImageDescriptor("icons/removeall.gif"));
		setDescription("Clear view");
	}

	public void run() {
		logView.clear();
	}

	public void dispose() {
	}

}