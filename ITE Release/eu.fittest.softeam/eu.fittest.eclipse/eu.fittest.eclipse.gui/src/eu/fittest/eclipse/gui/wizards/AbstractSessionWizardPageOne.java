package eu.fittest.eclipse.gui.wizards;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.naming.InvalidNameException;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Text;

import eu.fittest.common.core.xml.HUTEnvironment;
import eu.fittest.common.core.xml.HUTType;
import eu.fittest.eclipse.gui.nature.IFITTESTFolderConstants;
import eu.fittest.eclipse.gui.utils.Validation;
import eu.fittest.eclipse.gui.utils.Viewer;
import eu.fittest.eclipse.gui.wizards.project.HUTEditingSupport;
import eu.fittest.eclipse.model.environment.Host;

public abstract class AbstractSessionWizardPageOne  extends WizardPage{

	protected Text _sessionNameField;
	protected String _sessionName;
	protected TableViewer _hosts;
	
	protected IProject _selectedProject;
	
	public String getSessionName(){
		return this._sessionName;
	}
	
	@SuppressWarnings("unchecked")
	public List<Host> getHosts(){
		return (List<Host>) this._hosts.getInput();
	}
	
	protected java.util.List<Host> cloneHosts(java.util.List<Host> hosts){
		java.util.List<Host> clone = new ArrayList<Host>();
		for(Host host : hosts){
			//if(host.getEnvironment().equals(HUTEnvironment.TEST)){ // commented by urueda (display TEST && PRODUCTION environments)
				Host clonedHost = host.clone();
				clonedHost.addPropertyChangeListener("hUT",new PropertyChangeListener() {				
					@Override
					public void propertyChange(PropertyChangeEvent event) {
						dialogChanged();
					}
				});
				clone.add(clonedHost);
			//}
		}
		return clone;
	}
	
	protected AbstractSessionWizardPageOne(String pageName, IProject selectedProject) {
		super(pageName);
		this._selectedProject = selectedProject;
	}
	
	protected boolean oneClientHUTisSelected() {
		boolean selected = false;
		Iterator<Host> hosts = getHosts().iterator();
		while(!selected && hosts.hasNext()){
			Host current = hosts.next();
			selected = current.isHUT() && (current.getType().equals(HUTType.CLIENT) || current.getType().equals(HUTType.MIXED));
		}
		return selected;
	}
	
	protected boolean onlyOneServerHUTisSelected() {
		int selected = 0;
		Iterator<Host> hosts = getHosts().iterator();
		while(selected<=1 && hosts.hasNext()){
			Host current = hosts.next();
			if(current.isHUT() && (current.getType().equals(HUTType.SERVER) || current.getType().equals(HUTType.MIXED))){
				selected++;
			}
		}
		return selected==1;
	}
	
	protected boolean sessionAlreadyExists(){
		boolean found = false;
		
		try {
			IResource[] resources = this._selectedProject.getFolder(IFITTESTFolderConstants.TESTING_SESSIONS).members();
			int i = 0;
			while(!found && i<resources.length){
				found = resources[i].getName().equals(this._sessionName);
				i++;
			}
		} catch (CoreException e) {
			Logger.getAnonymousLogger().log(Level.INFO, e.getMessage());
		}
		
		return found;
	}
	
	protected void updateStatus(String message) {
		setErrorMessage(message);
		setPageComplete(message == null);
	}
	
	protected void createColumns() {
		//String[] titles = {"Name", "IP", "Description", "Type", "HUT"};
		String[] titles = {"Name", "IP", "Description", "Env", "Type", "HUT"}; // by urueda
		//int[] bounds = { 100, 100, 100, 60, 40 };
		int[] bounds = { 100, 100, 100, 100, 60, 40 }; // by ueuda

		TableViewerColumn col = Viewer.createTableViewerColumn(this._hosts, titles[0], bounds[0], 0);
		col.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(Object element) {
				Host h = (Host) element;
				return h.getName();
			}
		});

		col = Viewer.createTableViewerColumn(this._hosts, titles[1], bounds[1], 1);
		col.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(Object element) {
				Host h = (Host) element;
				return h.getIp();
			}
		});

		col = Viewer.createTableViewerColumn(this._hosts, titles[2], bounds[2], 2);
		col.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(Object element) {
				Host h = (Host) element;
				return h.getDescription();
			}
		});
		
		// begin by urueda
		col = Viewer.createTableViewerColumn(this._hosts, titles[3], bounds[3], 3);
		col.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(Object element) {
				Host h = (Host) element;
				return h.getEnvironment().toString();
			}			
		});		
		// end by urueda
		
		//col = Viewer.createTableViewerColumn(this._hosts, titles[3], bounds[3], 3);
		col = Viewer.createTableViewerColumn(this._hosts, titles[4], bounds[4], 4); // by urueda
		col.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(Object element) {
				Host h = (Host) element;
				return h.getType().toString();
			}			
		});
		
		//col = Viewer.createTableViewerColumn(this._hosts, titles[4], bounds[4], 4);
		col = Viewer.createTableViewerColumn(this._hosts, titles[5], bounds[5], 5); // by urueda
		col.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(Object element) {
				return null;
			}

			@Override
			public Image getImage(Object element) {
				if (((Host) element).isHUT()) {
					return Viewer.CHECKED_ICON;
				} else {
					return Viewer.UNCHECKED_ICON;
				}
			}			
		});
		col.setEditingSupport(new HUTEditingSupport(this._hosts));
	}
	
	protected void dialogChanged() {
		if(this._sessionName == null || this._sessionName.length() == 0){
			updateStatus("Session name must not be empty");
		}
		else if(sessionAlreadyExists()){
			updateStatus("Session name must be unique");
		}
		else {
			try{
				Validation.containsBadChar(this._sessionName);
				// commented by urueda (we allow to create a recording session as far as a Server agent is selected)
				/*if(!oneClientHUTisSelected()){
					updateStatus("Please select at least one client as HUT");
				}
				else*/ if(!onlyOneServerHUTisSelected()){
					updateStatus("Please select only one server HUT");
				}
				else {
					updateStatus(null);
				}
			}catch (InvalidNameException e) {
				updateStatus("Session name can contain only "+e.getExplanation());	
			}
		}
	}

}
