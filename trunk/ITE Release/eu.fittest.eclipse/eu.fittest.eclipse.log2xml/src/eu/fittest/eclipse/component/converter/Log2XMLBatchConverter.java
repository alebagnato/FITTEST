package eu.fittest.eclipse.component.converter;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.osgi.framework.Bundle;

import eu.fittest.eclipse.gui.utils.ResourceUtils;
import eu.fittest.eclipse.log2xml.utils.Utils;

public class Log2XMLBatchConverter {
	
	/**
	 * Recursively convert all encountered log files
	 * 
	 * @author cdnguyen
	 * @param folder
	 * 
	 * @throws CoreException 
	 */
	public void recursiveConvert(IFolder folder) throws CoreException{
		List<IFile> allLogFiles = ResourceUtils.collectFiles(folder, "log", "log_");
		try {
			final File haslog = Utils.getHasLogExecutable();
			if (haslog != null && haslog.canExecute()){
				for (IFile f : allLogFiles){
					Utils.convert(haslog, f);
				}
			}
		} catch (Exception e) {
			Logger.getAnonymousLogger().log(Level.SEVERE, e.getMessage());
		}
	}
}
