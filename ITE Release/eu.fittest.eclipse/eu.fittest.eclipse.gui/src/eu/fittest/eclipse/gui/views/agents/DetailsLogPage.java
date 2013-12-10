package eu.fittest.eclipse.gui.views.agents;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.IDetailsPage;
import org.eclipse.ui.forms.IFormPart;
import org.eclipse.ui.forms.IManagedForm;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.forms.widgets.TableWrapData;
import org.eclipse.ui.forms.widgets.TableWrapLayout;

import eu.fittest.eclipse.model.environment.Host;
import eu.fittest.eclipse.model.environment.HostModel;
import eu.fittest.eclipse.model.environment.IHostsListener;

public class DetailsLogPage implements IDetailsPage{

	private IManagedForm mform;

	private Text textInfo;
	private Text textLogs;
	private Host input;
	Composite parent;

	public void createContents(Composite parent) {
		this.parent = parent;
		
		GridLayout layout = new GridLayout();
		layout.marginRight = 10;
		layout.marginBottom = 10;
		parent.setLayout(layout);

		FormToolkit toolkit = mform.getToolkit();

		Section s1 = toolkit.createSection(parent, Section.TWISTIE | Section.EXPANDED);
		s1.setText("Agent Info");

		GridData td = new GridData(GridData.FILL_BOTH);
		s1.setLayoutData(td);
		
		toolkit.createCompositeSeparator(s1);

		Composite client1 = toolkit.createComposite(s1, SWT.TOP);
		client1.setLayout(new FillLayout(SWT.VERTICAL));

		textInfo = toolkit.createText(client1, "", SWT.V_SCROLL | SWT.MULTI | SWT.BORDER);
		toolkit.paintBordersFor(client1);
		s1.setClient(client1);

		// Execution Log
		Section s2 = toolkit.createSection(parent, Section.TWISTIE | Section.EXPANDED);
		s2.setText("Interaction Logs");

		td = new GridData(GridData.FILL_BOTH);
		s2.setLayoutData(td);

		toolkit.createCompositeSeparator(s2);

		Composite client2 = toolkit.createComposite(s2);
		client2.setLayout(new FillLayout(SWT.VERTICAL));

		textLogs = toolkit.createText(client2, "", SWT.V_SCROLL | SWT.MULTI | SWT.BORDER);
		toolkit.paintBordersFor(client2);
		s2.setClient(client2);
	}


	public void commit(boolean onSave) {
	}

	public void initialize(IManagedForm form) {
		this.mform = form;
	}

	public boolean isDirty() {
		return false;
	}

	public boolean isStale() {
		return false;
	}

	public void refresh() {
	}

	public void setFocus() {
	}

	public boolean setFormInput(Object input) {
		return false;
	}

	public void selectionChanged(IFormPart part, ISelection selection) {
		IStructuredSelection ssel = (IStructuredSelection) selection;
		if (ssel.size() == 1) {
			input = (Host) ssel.getFirstElement();
		} else
			input = null;
		update();
	}

	private void update(){
		if (input!=null){
			textInfo.setText(input.getInfo());
			textLogs.setText(input.getActivityLog());
		}
	}


	@Override
	public void dispose() {
		// TODO Auto-generated method stub
		
	}
	
}
