package eu.fittest.eclipse.gui.wizards.replayingsession;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.eclipse.gui.wizards.AbstractSessionWizardPageOne;
import eu.fittest.eclipse.model.environment.HostModel;

public class FITTESTReplayingSessionWizardPageTwo extends AbstractSessionWizardPageOne {
	private static final String PAGE_NAME = "New Replaying Session";

	public FITTESTReplayingSessionWizardPageTwo(IProject selectedProject) {
		super(PAGE_NAME, selectedProject);
	}

	@Override
	public void createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NULL);
		GridLayout gridLayout = new GridLayout();
		gridLayout.numColumns = 2;
		composite.setLayout(gridLayout);

		GridData layoutData = new GridData();
		Label label = new Label(composite, SWT.NULL);
		label.setText("Session name:");
		layoutData.horizontalAlignment = SWT.BEGINNING;
		label.setLayoutData(layoutData);

		this._sessionNameField = new Text(composite, SWT.SINGLE | SWT.BORDER);
		this._sessionNameField.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				_sessionName = _sessionNameField.getText().trim();
				dialogChanged();
			}
		});

		layoutData = new GridData(GridData.FILL_HORIZONTAL);
		//		layoutData.grabExcessHorizontalSpace = true;
		//		layoutData.horizontalAlignment = GridData.FILL;
		this._sessionNameField.setLayoutData(layoutData);		

		layoutData = new GridData();

		Group hostsGroup = new Group(composite, SWT.SHADOW_NONE);
		layoutData.horizontalSpan = 2;
		layoutData.horizontalAlignment = GridData.FILL;
		layoutData.grabExcessHorizontalSpace = true;
		layoutData.verticalAlignment = GridData.FILL;
		layoutData.grabExcessVerticalSpace = true;
		hostsGroup.setLayoutData(layoutData);

		gridLayout = new GridLayout();
		gridLayout.numColumns = 2;
		hostsGroup.setLayout(gridLayout);

		hostsGroup.setText("Available hosts on test environment");


		this. _hosts = new TableViewer(hostsGroup, SWT.MULTI | SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION);
		createColumns();
		this._hosts.getTable().setLinesVisible (true);
		this._hosts.getTable().setHeaderVisible (true);

		this._hosts.setContentProvider(new ArrayContentProvider());
		try {
			this._hosts.setInput(cloneHosts(HostModel.getInstance().getHosts()));
		} catch (FITTESTException e1) {
			e1.printStackTrace();
		}

		layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
		layoutData.horizontalSpan = 2;
		layoutData.heightHint = 200;
		this._hosts.getControl().setLayoutData(layoutData);

		this._sessionNameField.setFocus();
		dialogChanged();
		setControl(composite);
	}	

}
