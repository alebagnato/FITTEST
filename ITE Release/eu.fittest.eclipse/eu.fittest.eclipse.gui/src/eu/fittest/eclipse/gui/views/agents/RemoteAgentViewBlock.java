package eu.fittest.eclipse.gui.views.agents;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Table;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.DetailsPart;
import org.eclipse.ui.forms.IManagedForm;
import org.eclipse.ui.forms.MasterDetailsBlock;
import org.eclipse.ui.forms.SectionPart;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.part.ViewPart;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.eclipse.gui.Activator;
import eu.fittest.eclipse.model.environment.Host;
import eu.fittest.eclipse.model.environment.HostModel;
import eu.fittest.eclipse.model.environment.IHostsListener;

public class RemoteAgentViewBlock extends MasterDetailsBlock implements
		IHostsListener {

	HostModel model = null;
	ViewPart view;
	TableViewer viewer;

	public RemoteAgentViewBlock(ViewPart view) {
		super();
		this.view = view;
	}

	@Override
	protected void createMasterPart(final IManagedForm managedForm,
			Composite parent) {
		FormToolkit toolkit = managedForm.getToolkit();

		Section section = toolkit.createSection(parent, Section.DESCRIPTION);
		section.setText("Remote agents");
		section.setDescription("Select an agent to see info and logs");
		section.marginWidth = 10;
		section.marginHeight = 5;

		// GridData sessionLayoutData = new GridData(GridData.FILL_VERTICAL);
		// sessionLayoutData.widthHint = 200;
		// section.setLayoutData(sessionLayoutData);

		toolkit.createCompositeSeparator(section);
		Composite client = toolkit.createComposite(section, SWT.WRAP);
		GridLayout layout = new GridLayout();
		// layout.numColumns = 1;
		layout.marginWidth = 2;
		layout.marginHeight = 2;
		client.setLayout(layout);
		Table t = toolkit.createTable(client, SWT.NULL);
		GridData gd = new GridData(GridData.FILL_BOTH);
		// gd.heightHint = 20;
		gd.widthHint = 200;
		t.setLayoutData(gd);
		toolkit.paintBordersFor(client);
		// Button b = toolkit.createButton(client, "Add...", SWT.PUSH);
		// gd = new GridData(GridData.VERTICAL_ALIGN_BEGINNING);
		// b.setLayoutData(gd);
		section.setClient(client);
		final SectionPart spart = new SectionPart(section);
		managedForm.addPart(spart);
		viewer = new TableViewer(t);
		viewer.addSelectionChangedListener(new ISelectionChangedListener() {
			public void selectionChanged(SelectionChangedEvent event) {
				managedForm.fireSelectionChanged(spart, event.getSelection());
			}
		});
		viewer.setContentProvider(new MasterContentProvider());
		viewer.setLabelProvider(new MasterLabelProvider());
		
		viewer.addDoubleClickListener(new IDoubleClickListener() {
			
			@Override
			public void doubleClick(DoubleClickEvent event) {
				ISelection selection =  event.getSelection();
				if (selection != null){
				IStructuredSelection ssel = (IStructuredSelection) selection;
					if (ssel.size() == 1 && ssel.getFirstElement() instanceof Host) {
						Host selectedHost = (Host) ssel.getFirstElement();
						selectedHost.clearActivity();
					} 
				}
			}
		});

		try {
			this.model = HostModel.getInstance();
			model.addHostsListener(this);
		} catch (FITTESTException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		viewer.setInput(model);
		
		
	}
	

	public class MasterContentProvider implements IStructuredContentProvider {

		public Object[] getElements(Object inputElement) {
			if (inputElement instanceof HostModel) {
				return model.getHosts().toArray();
			}
			return new Object[0];
		}

		public void dispose() {
		}

		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		}

	}

	public class MasterLabelProvider extends LabelProvider implements
			ITableLabelProvider {

		public Image getColumnImage(Object element, int columnIndex) {
			return PlatformUI.getWorkbench().getSharedImages()
					.getImage(ISharedImages.IMG_OBJ_FILE);
		}

		public String getColumnText(Object element, int columnIndex) {
			return element.toString();
		}

	}

	@Override
	protected void createToolBarActions(IManagedForm managedForm) {
		final ScrolledForm form = managedForm.getForm();
		Action haction = new Action("hor", Action.AS_RADIO_BUTTON) {
			public void run() {
				sashForm.setOrientation(SWT.HORIZONTAL);
				form.reflow(true);
			}
		};
		haction.setChecked(true);
		haction.setToolTipText("Horizontal orientation");
		haction.setImageDescriptor(Activator.getDefault().getImageRegistry()
				.getDescriptor(Activator.IMG_HORIZONTAL));
		Action vaction = new Action("ver", Action.AS_RADIO_BUTTON) {
			public void run() {
				sashForm.setOrientation(SWT.VERTICAL);
				form.reflow(true);
			}
		};
		vaction.setChecked(false);
		vaction.setToolTipText("Vertical orientation");
		vaction.setImageDescriptor(Activator.getDefault().getImageRegistry()
				.getDescriptor(Activator.IMG_VERTICAL));
		form.getToolBarManager().add(haction);
		form.getToolBarManager().add(vaction);
	}

	@Override
	protected void registerPages(DetailsPart detailsPart) {
		detailsPart.registerPage(Host.class, new DetailsLogPage());
	}

	@Override
	public void notifyHostAdded(Host host) {
		Display.getDefault().asyncExec(new Runnable() {
		      public void run() {
		    	  viewer.refresh();
		      }
		});
		
	}

	@Override
	public void notifyHostRemoved(Host host) {
		Display.getDefault().asyncExec(new Runnable() {
		      public void run() {
		    	  viewer.refresh();
		      }
		});
	}

	public void dispose() {
		if (model != null){
			model.removeHostsListener(this);
		}
	}

	@Override
	public void notifyComponentAdded(Host host, String componentId) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void notifyComponentRemoved(Host host, String componentId) {
		// TODO Auto-generated method stub
		
	}
}
