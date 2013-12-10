/***************************************************************************
* Copyright (c) 2008 Conselleria de Infraestructuras y Transporte,
* Generalitat de la Comunitat Valenciana . All rights reserved. This program
* and the accompanying materials are made available under the terms of the
* Eclipse Public License v1.0 which accompanies this distribution, and is
* available at http://www.eclipse.org/legal/epl-v10.html
*
* Contributors: Jose Manuel García Valladolid (CIT) - Initial API and implementation
*
**************************************************************************/

package es.cv.gvcase.fefem.common.databinding;

import org.eclipse.core.databinding.observable.IObservable;
import org.eclipse.core.databinding.observable.list.IObservableList;
import org.eclipse.core.databinding.observable.masterdetail.IObservableFactory;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.emf.databinding.edit.EMFEditObservables;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.transaction.TransactionalEditingDomain;

/**
 * Return an IObservableList of children for a given parent object 
 * Required by ObservableListTreeContentProvider constructor
 */
public class EMFObservableTreeFactory implements IObservableFactory {

	protected EStructuralFeature reflexiveFeature;
	protected TransactionalEditingDomain editingDomain;
	protected  IObservableList modelObservable;
	
	public EMFObservableTreeFactory(EStructuralFeature reflexiveFeature,TransactionalEditingDomain editingDomain, IObservableList modelObservable) {
		super();
		this.editingDomain = editingDomain;
		this.reflexiveFeature = reflexiveFeature;
		this.modelObservable = modelObservable;
	}


	/* (non-Javadoc)
	 * @see org.eclipse.core.databinding.observable.masterdetail.IObservableFactory#createObservable(java.lang.Object)
	 */
	public IObservable createObservable(Object target) {
		
		if((target instanceof IObservableList)||(target instanceof IObservableValue)){
			
			// Single mode or Master/Detail mode
			// In both modes the received modelObservable is already resolved
			return this.modelObservable;
			
		}else{
					
			// Now we get the reflexive EObject node
			IObservableList childrenModelObservable = EMFEditObservables.observeList(editingDomain,	(EObject) target, reflexiveFeature);
			return childrenModelObservable;
		}
		
		
		
	}

}
