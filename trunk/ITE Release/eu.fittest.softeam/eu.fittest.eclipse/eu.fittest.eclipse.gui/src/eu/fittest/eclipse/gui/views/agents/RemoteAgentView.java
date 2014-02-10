package eu.fittest.eclipse.gui.views.agents;


import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.ManagedForm;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.part.ViewPart;

import eu.fittest.eclipse.gui.Activator;


public class RemoteAgentView extends ViewPart{
	
	public static String VIEW_ID = "eu.fittest.eclipse.gui.views.agent";

	RemoteAgentViewBlock block;
	Composite parent;
	ManagedForm managedForm;
	
	public RemoteAgentView() {
		block = new RemoteAgentViewBlock(this);
	}

	@Override
	public void createPartControl(Composite parent) {
		this.parent = parent;
		
		managedForm = new ManagedForm(parent); 
		final ScrolledForm form = managedForm.getForm();
		form.setText("FITTEST Remote Agents");
		form.setBackgroundImage(Activator.getDefault().getImage(Activator.IMG_FORM_BG));
		
		block.createContent(managedForm);
	}

	@Override
	public void setFocus() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void dispose() {
		block.dispose();
		super.dispose();
	}
	
//	public void refresh(){
//		Display.getDefault().asyncExec(new Runnable() {
//		      public void run() {
//		    	  managedForm.dispose();
//		    	  createPartControl(parent);
//		    	  parent.layout();
//		    	  parent.redraw();
//		      }
//		});
//	}
	
	
	
}
