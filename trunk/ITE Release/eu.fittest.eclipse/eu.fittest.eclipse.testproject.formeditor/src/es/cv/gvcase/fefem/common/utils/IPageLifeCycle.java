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
 * This interface defines all methods that any Editor Page must implements
 * in order to have all its life cycle events monitored
 */
public interface IPageLifeCycle {

	/**
	 * Add a new PageLifeCycleListener
	 * @param l
	 */
	public void addPageLifeCycleListener(IPageLifeCycleListener l);
	
	/**
	 * Remove this PageLifeCycleListener from the list
	 * @param l
	 */
	public void removePageLifeCycleListener(IPageLifeCycleListener l);
	
	/**
	 * Notifies all the registered PageLifeCycleListeners that the content of
	 * this page has been created
	 */
	public void notifyContentCreated();
}
