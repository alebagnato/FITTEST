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

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.jface.databinding.viewers.TreeStructureAdvisor;

public class EMFTreeStructuredAdvisor extends TreeStructureAdvisor {

	protected EStructuralFeature reflexiveReference;
	
	
	public EMFTreeStructuredAdvisor(EStructuralFeature reflexiveReference) {
		super();
		this.reflexiveReference = reflexiveReference;
	}

	

	@Override
	public Boolean hasChildren(Object element) {
		if(element instanceof EObject){
			EObject e = (EObject) element;
			try{
				Object r = e.eGet(reflexiveReference);
				if(r!=null){
					if (r instanceof EList){
						return new Boolean(((EList) r).size() > 0);
					}
				}
			}catch (Exception exc){

			}
		}
		return new Boolean(false);
	}

	
}
