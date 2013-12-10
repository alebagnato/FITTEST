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

import java.io.IOException;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.eclipse.emf.edit.domain.EditingDomain;
import org.eclipse.emf.edit.ui.action.LoadResourceAction;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;

import es.cv.gvcase.fefem.common.widgets.FilteredLoadResourceDialog;
import eu.fittest.eclipse.testproject.formeditor.Activator;

public class FilteredLoadResourceAction extends LoadResourceAction {

	protected ResourceFilePattern[] fileExtensionsPatterns;
	
	/**
	 * 
	 */
	public FilteredLoadResourceAction() {
		this.setIconResource();
	}

	/**
	 * @param domain
	 */
	public FilteredLoadResourceAction(EditingDomain domain) {
		super(domain);
		this.setIconResource();
	}

	
	public ResourceFilePattern[] getFileExtensionsPatterns() {
		return fileExtensionsPatterns;
	}

	public void seFileExtensionsPatterns(ResourceFilePattern[] resFileExtensionsPatterns) {
		this.fileExtensionsPatterns = resFileExtensionsPatterns;
	}

	@Override
	public void run() {
		FilteredLoadResourceDialog loadResourceDialog =
		      new FilteredLoadResourceDialog
		          (PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), domain);
		  
		 if(fileExtensionsPatterns != null)
			 loadResourceDialog.setResourcePatterns(fileExtensionsPatterns);
		 loadResourceDialog.open();
	}
		
	private void setIconResource(){
			
		this.setImageDescriptor(new ImageDescriptor(){

			@Override
			public ImageData getImageData() {
				String imagePath = "";
				try {
					imagePath = FileLocator.toFileURL(
							Platform.getBundle(Activator.PLUGIN_ID)
									.getResource("icons/full/etool16")).getPath();
					imagePath += "filteredloadresource.gif";
					Image image = new Image(Display.getCurrent(), imagePath);
					
					return image.getImageData();
				} catch (IOException e) {
				}

				return null;
				
			}
			
		});
	}
}
