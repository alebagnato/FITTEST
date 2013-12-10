package es.cv.gvcase.fefem.common;

import java.io.IOException;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.emf.common.command.CommandStack;
import org.eclipse.emf.common.ui.MarkerHelper;
import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecore.util.Diagnostician;
import org.eclipse.emf.ecore.xmi.XMLResource;
import org.eclipse.emf.ecore.xmi.XMLResource.URIHandler;
import org.eclipse.emf.ecore.xmi.impl.URIHandlerImpl;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceFactoryImpl;
import org.eclipse.emf.edit.domain.IEditingDomainProvider;
import org.eclipse.emf.edit.ui.action.ValidateAction;
import org.eclipse.emf.edit.ui.util.EditUIUtil;
import org.eclipse.emf.transaction.TransactionalEditingDomain;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialogWithToggle;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IPropertyListener;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.editor.FormEditor;
import org.eclipse.ui.forms.editor.FormPage;
import org.eclipse.ui.forms.editor.IFormPage;

import eu.fittest.eclipse.testproject.formeditor.Activator;

public abstract class FEFEMEditor extends FormEditor implements IEditingDomainProvider {


	protected EObject model;
	
	protected Resource resource;
	
	protected TransactionalEditingDomain domain;

	@Override
	protected void addPages() {
		
		loadModel();
		
		for (FormPage page : getEditorPagesList()){
			try {				
				addPage(page);
				page.addPropertyListener(new MyPropertyListener());
			} catch (PartInitException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		
	}
	
	protected void loadModel() {
		getToolkit();
		
		IEditorInput input = this.getEditorInput();
		// Load the model as an EMF Model
		IFile modelFile = ((IFileEditorInput) input).getFile(); 
		model = loadModel(modelFile.getFullPath());
	}
	
	protected EObject loadModel(IPath filePath) {
		
		ResourceSet resourceSet = new ResourceSetImpl();
		
		// Register the appropriate resource factory to handle all file extensions.
		resourceSet.getResourceFactoryRegistry().getExtensionToFactoryMap()
				.put(Resource.Factory.Registry.DEFAULT_EXTENSION,
						new XMIResourceFactoryImpl());

		// Register the package to ensure it is available during loading.
		resourceSet.getPackageRegistry().put(get_eNS_URI(),
				get_eINSTANCE());

		domain = TransactionalEditingDomain.Factory.INSTANCE.createEditingDomain(resourceSet);
		
		URI resourceURI = URI.createPlatformResourceURI(filePath.toString(),true);

//		URI resourceURI = EditUIUtil.getURI(getEditorInput());
		
		try {
			// Load the resource through the editing domain.
			//
			resource = domain.getResourceSet().getResource(resourceURI, true);
		}
		catch (Exception e) {
			resource = domain.getResourceSet().getResource(resourceURI, false);
		}
		
//		resource = resourceSet.getResource(uri, true);
		
		if(resource instanceof XMLResource) {
			((XMLResource)this.resource).getDefaultSaveOptions().put(
					XMLResource.OPTION_URI_HANDLER, getURIHandler());
		}

		this.setPartName(getEditorInput().getName());
		return resource.getContents().get(0);
	}
	
	@Override
	public void doSave(IProgressMonitor monitor) {
		
		Activator plugin = Activator.getDefault();
		IPreferenceStore prefStore =  plugin.getPreferenceStore();
		
		boolean toggleState = prefStore
        .getString(Activator.PREFERENCES_VALIDATE_ON_SAVE).equals(
            MessageDialogWithToggle.ALWAYS)
        || prefStore.getString(Activator.PREFERENCES_VALIDATE_ON_SAVE).equals(
            MessageDialogWithToggle.NEVER);
		
		if (!toggleState){
			MessageDialogWithToggle dialog = MessageDialogWithToggle.openYesNoCancelQuestion(
					PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
					"On save validation",
					"Do you want to validate the model before saving?",
					"Remember my decision",
					false,
					prefStore,
					Activator.PREFERENCES_VALIDATE_ON_SAVE);
			
				
			if (dialog.getReturnCode() == IDialogConstants.YES_ID){
				validateModel();
			} else if (dialog.getReturnCode() == IDialogConstants.CANCEL_ID){
				return;
			}
		} else if (prefStore.getString(Activator.PREFERENCES_VALIDATE_ON_SAVE).equals(MessageDialogWithToggle.ALWAYS)){
			validateModel();
		}
		
		if (pages != null) {
			for (int i = 0; i < pages.size(); i++) {
				Object page = pages.get(i);
				if (page instanceof IFormPage) {
					IFormPage fpage = (IFormPage) page;
					fpage.doSave(monitor);
				}
			}
		}
		
		try {
			this.resource.save(null);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
	
	private void validateModel(){
		Diagnostician diagnostician = org.eclipse.emf.ecore.util.Diagnostician.INSTANCE;
		Diagnostic diagnostic = diagnostician.validate(this.getModel());
		MarkerHelper markerHelper = new ValidateAction.EclipseResourcesUtil();
		try {
			markerHelper.deleteMarkers(this.getModel());
			markerHelper.createMarkers(diagnostic);
		} catch (CoreException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	@Override
	public void doSaveAs() {
		// TODO Allow isSave
		
	}
	@Override
	public boolean isSaveAsAllowed() {
		return false;
	}
	
	public EObject getModel(){
		return this.model;
	}
	
	public CommandStack getCommandStack(){
		return this.domain.getCommandStack();
	}
	
	public TransactionalEditingDomain getEditingDomain(){
		return this.domain;
	}
	
	private class MyPropertyListener implements IPropertyListener {
		public void propertyChanged(Object source, int propId) {
			if (propId == PROP_DIRTY) {
				firePropertyChange(PROP_DIRTY);
			}
		}
	}
	
	protected URIHandler getURIHandler() {
		return new URIHandlerImpl() {
			@Override
			public URI deresolve(URI uri) {
				if (resolve && !uri.isRelative() && !uri.isPlatformPlugin()) {
					URI deresolvedURI = uri.deresolve(baseURI, true, true, false);
					if (deresolvedURI.hasRelativePath()) {
						uri = deresolvedURI;
					}
				}
				return uri;
			}
		};
	}
	
	
	protected abstract String get_eNS_URI();
	
	protected abstract EPackage get_eINSTANCE();
		
	protected abstract List<FormPage> getEditorPagesList();
	

}
