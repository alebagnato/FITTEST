package es.cv.gvcase.fefem.common.providers;

import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.edit.provider.ComposedAdapterFactory;
import org.eclipse.emf.edit.provider.IItemLabelProvider;
import org.eclipse.emf.edit.provider.ReflectiveItemProviderAdapterFactory;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryLabelProvider;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

public class EObjectLabelProvider extends LabelProvider {

	/* (non-Javadoc)
	 * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
	 */
	@Override
	public String getText(Object element) {
		 if (element instanceof EObject){
             AdapterFactoryLabelProvider adapter = new AdapterFactoryLabelProvider(
                                  	new ComposedAdapterFactory(ComposedAdapterFactory.Descriptor.Registry.INSTANCE));
             return adapter.getText(element);
		 }
		 return "";
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.viewers.LabelProvider#getImage(java.lang.Object)
	 */
	@Override
	public Image getImage(Object element) {
		 if (element == null)
             return null;
    
		 AdapterFactoryLabelProvider adapter = new AdapterFactoryLabelProvider(
                             new ComposedAdapterFactory(ComposedAdapterFactory.Descriptor.Registry.INSTANCE));
    
		 return adapter.getImage(element);
	}

	

}
