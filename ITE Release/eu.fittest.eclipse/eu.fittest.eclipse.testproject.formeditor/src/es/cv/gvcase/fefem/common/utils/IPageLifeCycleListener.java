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


/**
 * This interface is created in order to listen editor pages life cycle events 
 */
public interface IPageLifeCycleListener {

	/**
	 * Notify when all page content widgets were created
	 * @param managedForm
	 */
	public void contentCreated(IPageLifeCycle page);
}
