package eu.fittest.eclipse.model;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.PlatformObject;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

import eu.fittest.eclipse.gui.nature.TestingProject;

public class FITTESTWorkspaceRoot extends PlatformObject implements ITreeContentProvider{
	static private FITTESTWorkspaceRoot _instance = null;
	
	static public FITTESTWorkspaceRoot getInstance(){
		if(_instance==null){
			_instance = new FITTESTWorkspaceRoot();
		}
		return _instance;
	}

	private FITTESTWorkspaceRoot() {
	}
	
	@Override
	public void dispose() {
		
	}

	@Override
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		
	}

	@Override
	public Object[] getElements(Object inputElement) {		
		return getChildren(inputElement);
	}

	@Override
	public Object[] getChildren(Object parentElement) {
		IProject [] projects = ResourcesPlugin.getWorkspace().getRoot().getProjects();
		List<FITTESTProject> fprojects = new ArrayList<FITTESTProject>();
        for (IProject p : projects) {
        	FITTESTProject current = new FITTESTProject(p);
            try {
                if (p.getNature(TestingProject.NATURE_ID) != null) {
                	fprojects.add(current);
                }
            } catch (CoreException e) {
                // Go to the next IProject
            }
        }
        return fprojects.toArray();
	}

	@Override
	public Object getParent(Object element) {
		return null;
	}

	@Override
	public boolean hasChildren(Object element) {
		return getChildren(element).length >0;
	}
}
