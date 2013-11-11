package es.cv.gvcase.fefem.common.utils;

import org.eclipse.core.databinding.UpdateValueStrategy;
import org.eclipse.core.runtime.IStatus;

import es.cv.gvcase.fefem.common.composites.EMFPropertyComposite;

public class DefaultTargetToModelUpdateValueStrategy extends UpdateValueStrategy {

	EMFPropertyComposite composite = null;
	
	public DefaultTargetToModelUpdateValueStrategy(EMFPropertyComposite composite){
		this.composite = composite;
	}
	
	/* (non-Javadoc)
	 * @see org.eclipse.core.databinding.UpdateValueStrategy#validateBeforeSet(java.lang.Object)
	 */
	@Override
	public IStatus validateBeforeSet(Object value) {
		IStatus status = super.validateBeforeSet(value);
		if (composite.getEObject() != null){
			composite.getPage().setDirty(true);
			composite.getPage().refresh();
		}
		return status;
	}

}
