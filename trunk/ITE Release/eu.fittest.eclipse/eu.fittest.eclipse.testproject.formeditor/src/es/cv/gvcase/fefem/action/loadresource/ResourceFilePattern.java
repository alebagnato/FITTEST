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
package es.cv.gvcase.fefem.action.loadresource;

/**
 * This class is needed to indentify kinds of Resources by its filename extension pattern
 *
 * @author Jose Manuel García Valladolid
 */
public class ResourceFilePattern {

	private String pattern;
	private String description;
	
	
	public ResourceFilePattern(String pattern) {
		super();
		this.pattern = pattern;
	}


	public ResourceFilePattern(String pattern,
			String description) {
		super();
		this.description = description;
		this.pattern = pattern;
	}


	public String getPattern() {
		return pattern;
	}


	public void setPattern(String pattern) {
		this.pattern = pattern;
	}


	public String getDescription() {
		return description;
	}


	public void setDescription(String description) {
		this.description = description;
	}
	
	
}
