package es.cv.gvcase.fefem.common.composites;

import org.eclipse.core.databinding.observable.ChangeEvent;
import org.eclipse.core.databinding.observable.IChangeListener;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.forms.widgets.FormToolkit;

import es.cv.gvcase.fefem.common.FEFEMPage;

public abstract class EMFEObjectComposite extends EMFPropertyComposite {

	private Group group;
	
	
	public EMFEObjectComposite(Composite parent, int style, FormToolkit toolkit, EObject eObject, FEFEMPage page) {
		super(parent, style, toolkit, eObject,page);
	}
	
	public EMFEObjectComposite(Composite parent, int style,  FormToolkit toolkit, Viewer viewer, FEFEMPage page){
		super(parent,style, toolkit, viewer, page);
	}
	
	public Group getGroup() {
		return group;
	}
	
	@Override
	protected void createWidgets(FormToolkit toolkit) {
		this.setLayout(new GridLayout(1, false));
		
		group = new Group(this, SWT.NONE);
		
		group.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		group.setLayout(new GridLayout(1, false));
		
		if(this.getEObject() != null) {
			Object text = this.getEObject().eGet(getFeature());
			if (text != null){
				group.setText(text.toString());
			}
		}
		
		toolkit.adapt(this);
	}
	
	protected void bindFeatureToWidget() {
		super.bindFeatureToWidget();
		
		if (getFeatureValue() != null){
			group.setText(getFeatureValue().toString());	
		}
		
		if(this.modelObservable != null) {
			this.modelObservable.addChangeListener(new IChangeListener(){
				public void handleChange(ChangeEvent event) {
					if(group != null && !group.isDisposed()) {
						String valueToSet = updatedGroupText() != null ? updatedGroupText() : "";
						group.setText(valueToSet);
					}				
				}
				
			});
		}
	}
	
	protected String updatedGroupText(){
		return getFeatureValue()!=null ? getFeatureValue().toString() : null;
	}
	
	@Override
	protected String getLabelText() {
		// TODO Auto-generated method stub
		return null;
	}
	

	@Override
	protected IObservableValue getTargetObservable() {
		return null;
	}
	
	
}
