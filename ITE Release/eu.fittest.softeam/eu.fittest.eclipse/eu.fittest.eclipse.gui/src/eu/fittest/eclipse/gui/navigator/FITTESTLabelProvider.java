package eu.fittest.eclipse.gui.navigator;

import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;


public class FITTESTLabelProvider implements ILabelProvider {

	@Override
	public void addListener(ILabelProviderListener listener) {

	}

	@Override
	public void dispose() {

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
		if(element instanceof ILabelProvider){
			return ((ILabelProvider)element).getImage(element);
		}
		else return null;
	}

	@Override
	public String getText(Object element) {
		if(element instanceof ILabelProvider){
			return ((ILabelProvider)element).getText(element);
		}
		else return null;
	}

}
