/***************************************************************************
* Copyright (c) 2008 Conselleria de Infraestructuras y Transporte,
* Generalitat de la Comunitat Valenciana . All rights reserved. This program
* and the accompanying materials are made available under the terms of the
* Eclipse Public License v1.0 which accompanies this distribution, and is
* available at http://www.eclipse.org/legal/epl-v10.html
*
* Contributors: Mario Cervera Ubeda (Integranova)
*
**************************************************************************/
package es.cv.gvcase.fefem.common.composites;

import org.eclipse.core.databinding.UpdateValueStrategy;
import org.eclipse.core.databinding.conversion.Converter;
import org.eclipse.core.databinding.conversion.IConverter;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;

import es.cv.gvcase.fefem.common.FEFEMPage;
import es.cv.gvcase.fefem.common.providers.EObjectLabelProvider;
import es.cv.gvcase.fefem.common.widgets.CSingleObjectChooser;

/**
 * An implementation of a composite with a chooser.
 * Invoke bindFeatureToWidget to bind the chooser with the feature
 * 
 * @author Mario Cervera
 */
public abstract class EMFPropertyEReferenceComposite
	extends EMFPropertyComposite {
	
	/**
	 * The combo box control for the section.
	 */
	private CSingleObjectChooser chooser;
	
	public EMFPropertyEReferenceComposite(Composite parent, int style,
			FormToolkit toolkit, EObject eObject, FEFEMPage page) {
		super(parent, style, toolkit, eObject, page);
		
		hookListeners();
	}
	
	public EMFPropertyEReferenceComposite(Composite parent, int style,
			FormToolkit toolkit, Viewer viewer, FEFEMPage page) {
		super(parent, style, toolkit, viewer, page);
		
		hookListeners();
	}
	
	protected CSingleObjectChooser getChooser() {
		return chooser;
	}
	
	public void setLabelProvider(ILabelProvider lp) {
		chooser.setLabelProvider(lp);
	}
	
	public void setChoices(Object[] choices) {
		chooser.setChoices(choices);
	}

	protected void createWidgets(FormToolkit toolkit) {
		
		this.setLayout(new GridLayout(2, false));
		
		createLabel(toolkit);
		
		chooser = new CSingleObjectChooser(this, toolkit, SWT.NONE);
		chooser.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		chooser.setLabelProvider(getLabelProvider());
		IStructuredContentProvider provider = this.getChooserContentProvider();
		if (provider != null){
			chooser.setStructuredContentProvider(provider);
		} else {
			setChoices(getChoices());
		}		
		
		if (getFeature() != null) {
			chooser.setChangeable(getFeature().isChangeable() && this.isEditable());
		}
	}
	
	private IStructuredContentProvider getChooserContentProvider() {
		return new IStructuredContentProvider(){

			public Object[] getElements(Object inputElement) {
				Object [] choices = getChoices();
				for (int i=0; i<choices.length; i++){
					if (choices[i] == null){
						choices[i] = "";
					}
				}
				return choices;
			}

			public void dispose() {				
			}

			public void inputChanged(Viewer viewer, Object oldInput,
					Object newInput) {
			}
			
		};
	}

	protected void hookListeners() {
		chooser.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				handleComboModified();
			}
		});
	}
	
	/**
	 * Handle the combo modified event.
	 */
	protected void handleComboModified() {
		if(modelObservable != null) {
			modelObservable.setValue(chooser.getSelection());
		}
		
		getPage().setDirty(true);
	}
	
	/**
	 * A converter is needed to convert the String of the Text field
	 * of the chooser into a model element
	 * 
	 * @return the converter
	 */
	protected IConverter getConverter() {
		
		return new ChooserConverter(null, null);
	}
	
	@Override
	protected UpdateValueStrategy getModelToTargetUpdateValueStrategy() {
		UpdateValueStrategy uvs = new UpdateValueStrategy();
		if(getConverter() != null) {
			uvs.setConverter(getConverter());
		}
		return uvs;
	}
	
	@Override
	protected IObservableValue getTargetObservable() {
		return SWTObservables.observeText(chooser.getText(), SWT.Modify);
	}
	
	private class ChooserConverter extends Converter {
		
		public ChooserConverter(Object fromType, Object toType) {
			super(fromType, toType);
		}

		public Object convert(Object fromObject) {
			
			if(fromObject instanceof EObject) {
				return getLabelProvider().getText(fromObject);
			}
			
			return null;
		}
	}

	protected ILabelProvider getLabelProvider(){
		return new EObjectLabelProvider();
	}
	
	protected abstract Object[] getChoices();

	@Override
	protected void handleSelectionChanged() {	
		if (this.isDisposed()) return;
		this.setChoices(getChoices());
		if (getChooser() != null && this.getFeatureValue() != null){
			getChooser().setSelection(this.getFeatureValue());
		}
	}
}
