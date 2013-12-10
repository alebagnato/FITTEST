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

package es.cv.gvcase.fefem.common.utils;

import java.util.Iterator;
import java.util.Vector;

import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.Viewer;

/**
 * This class is used to synchronize the selection of a set of selection capable viewers
 */
public class SelectionSynchronizer implements ISelectionChangedListener {

	protected Vector<Viewer> viewers = new Vector<Viewer>();
	
	/* (non-Javadoc)
	 * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
	 */
	public void selectionChanged(SelectionChangedEvent event) {
		if(viewers.contains(event.getSource())){
			Iterator<Viewer> iter = viewers.iterator();
			while(iter.hasNext()){
				Viewer vp = (Viewer) iter.next();
				if(!vp.equals(event.getSource())){
					vp.removeSelectionChangedListener(this);
					vp.setSelection(event.getSelection());
					vp.refresh();
					vp.addSelectionChangedListener(this);
				}
			}
		}

	}

	public void addViewer(Viewer vp){
		if(!viewers.contains(vp)){
			viewers.add(vp);
			vp.addSelectionChangedListener(this);
		}
	}
	
	public void removeViewerProvider(Viewer vp){
		if(viewers.contains(vp)){
			viewers.remove(vp);
			vp.removeSelectionChangedListener(this);
		}
	}
	
}
