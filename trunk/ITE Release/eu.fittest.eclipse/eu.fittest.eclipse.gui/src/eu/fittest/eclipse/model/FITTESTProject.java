package eu.fittest.eclipse.model;

import java.util.ArrayList;

import org.eclipse.core.resources.IProject;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.PlatformUI;

public class FITTESTProject extends FITTESTArtifact{
	private IProject _project;
	
	public FITTESTProject(IProject project) {
		super(null);
		_project = project;
	}
	
	public String getProjectName(){
		return _project.getName();
	}

	@Override
	public Object[] getChildren(Object parentElement) {
		if(_children==null){
			_children = new ArrayList<FITTESTArtifact>();
			_children.add(new FITTESTEnvironment(this));
			_children.add(new FITTESTModels(this));	
			_children.add(new FITTESTTestSuites(this));
			_children.add(new FITTESTTestingSessions(this));	
		}
		return _getChildren(parentElement);
	}
	
	@Override
	public boolean equals(Object obj) {
		if(obj instanceof FITTESTProject){
			return ((FITTESTProject)obj)._project.equals(_project);
		}
		else return false;
	}

	@Override
	public String getText(Object element) {
		return _project.getName();
	}
	
	@Override
	public Image getImage(Object element) {
		return _project.isOpen()?
		PlatformUI.getWorkbench().getSharedImages().getImage(org.eclipse.ui.ide.IDE.SharedImages.IMG_OBJ_PROJECT):
			PlatformUI.getWorkbench().getSharedImages().getImage(org.eclipse.ui.ide.IDE.SharedImages.IMG_OBJ_PROJECT_CLOSED);
	}
	
}
