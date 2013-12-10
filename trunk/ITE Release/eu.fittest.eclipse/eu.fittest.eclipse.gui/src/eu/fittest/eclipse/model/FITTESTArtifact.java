package eu.fittest.eclipse.model;

import java.util.List;

import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.graphics.Image;

public abstract class FITTESTArtifact implements ITreeContentProvider, ILabelProvider{

	private FITTESTArtifact _parent;
	protected List<FITTESTArtifact> _children;
	
	protected FITTESTArtifact(FITTESTArtifact parent) {
		_parent = parent;
		_children = null;
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

	protected Object[] _getChildren(Object parentElement) {
		return _children.toArray();
	}

	@Override
	public Object getParent(Object element) {
		return _parent;
	}

	@Override
	public boolean hasChildren(Object element) {
		return getChildren(element).length>0;
	}

	@Override
	public void addListener(ILabelProviderListener listener) {
		
	}

	@Override
	public boolean isLabelProperty(Object element, String property) {
		return false;
	}

	@Override
	public void removeListener(ILabelProviderListener listener) {
		
	}

	@Override
	public Image getImage(Object element) {
		return null;
	}

}
