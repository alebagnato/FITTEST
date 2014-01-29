package eu.fittest.eclipse.gui.navigator;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;


public class FITTESTContentProvider implements ITreeContentProvider {
	
	@Override
	public void dispose() {		

	}

	@Override
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {		

	}

	@Override
	public Object[] getElements(Object inputElement) {
		if(inputElement instanceof ITreeContentProvider){
			return ((ITreeContentProvider)inputElement).getElements(inputElement);
		}
		else return null;
	}

	@Override
	public Object[] getChildren(Object parentElement) {
		if(parentElement instanceof ITreeContentProvider){
			return ((ITreeContentProvider)parentElement).getChildren(parentElement);
		}
		else return null;
	}
	
	@Override
	public Object getParent(Object element) {
		if(element instanceof ITreeContentProvider){
			return ((ITreeContentProvider)element).getParent(element);
		}
		else return null;
	}

	@Override
	public boolean hasChildren(Object element) {
		if(element instanceof ITreeContentProvider){
			return ((ITreeContentProvider)element).hasChildren(element);
		}
		else return false;
	}

}
