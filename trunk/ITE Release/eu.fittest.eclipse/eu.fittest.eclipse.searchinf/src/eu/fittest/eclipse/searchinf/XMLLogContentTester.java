package eu.fittest.eclipse.searchinf;

import java.util.List;

import org.eclipse.core.expressions.PropertyTester;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;

import eu.fittest.eclipse.gui.utils.ResourceUtils;

public class XMLLogContentTester extends PropertyTester  {

	@Override
	public boolean test(Object receiver, String property, Object[] args,
			Object expectedValue) {
		if(property.endsWith("members_extension")){
			IFolder folder = ResourceUtils.getFolder();
			if(folder!=null){ 
				try {
					List<IFile> allLogFiles = ResourceUtils.collectFiles(folder, "xml", "log_");
					if (allLogFiles.size() > 0){
						return true;
					}
				} catch (CoreException e) {
					e.printStackTrace();
				}
			}
		}
		return false;
	}

}
