package eu.fittest.eclipse.testproject.formeditor;

import java.util.ArrayList;
import java.util.List;


import org.eclipse.core.runtime.IPath;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.xmi.XMLResource;
import org.eclipse.emf.edit.ui.util.EditUIUtil;
import org.eclipse.ui.forms.editor.FormPage;
import es.cv.gvcase.fefem.common.FEFEMEditor;
import eu.fittest.eclipse.testproject.domain.TestProjectEditingDomainFactory;
import eu.fittest.eclipse.testproject.pages.GeneralPage;
import eu.fittest.eclipse.testproject.pages.LoggingPage;
import eu.fittest.eclipse.testproject.pages.ModelInferencePage;
import eu.fittest.eclipse.testproject.pages.OraclePage;
import eu.fittest.eclipse.testproject.pages.TestGenerationPage;
import eu.fittest.test.project.ProjectPackage;

public class TestProjectEditor extends FEFEMEditor {

	@Override
	protected String get_eNS_URI() {
		return ProjectPackage.eNS_URI;
	}

	@Override
	protected EPackage get_eINSTANCE() {
		return ProjectPackage.eINSTANCE;
	}

	@Override
	protected List<FormPage> getEditorPagesList() {
		List<FormPage> pageList = new ArrayList<FormPage>();
		pageList.add(new GeneralPage(this));
		pageList.add(new LoggingPage(this));
		pageList.add(new ModelInferencePage(this));
		pageList.add(new TestGenerationPage(this));
		pageList.add(new OraclePage(this));
		return pageList;
	}

	@Override
	protected EObject loadModel(IPath filePath) {
		
		TestProjectEditingDomainFactory domainFactor = new TestProjectEditingDomainFactory();
		domain = domainFactor.createEditingDomain();
		URI resourceURI = EditUIUtil.getURI(getEditorInput());
		
		try {
			// Load the resource through the editing domain.
			resource = domain.getResourceSet().getResource(resourceURI, true);
		}
		catch (Exception e) {
			resource = domain.getResourceSet().getResource(resourceURI, false);
		}
		
		if(resource instanceof XMLResource) {
			((XMLResource)this.resource).getDefaultSaveOptions().put(
					XMLResource.OPTION_URI_HANDLER, getURIHandler());
		}

		this.setPartName(getEditorInput().getName());
		return resource.getContents().get(0);
	}
	
	
	public void setPage(int index){
		if (index >= 0 && index < getPageCount()){
			setActivePage(index);
		}
	}
	
}
