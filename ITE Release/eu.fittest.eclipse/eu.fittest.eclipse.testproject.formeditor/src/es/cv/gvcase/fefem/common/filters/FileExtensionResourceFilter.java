package es.cv.gvcase.fefem.common.filters;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.eclipse.core.internal.resources.File;
import org.eclipse.core.resources.IResource;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.ui.internal.ide.StringMatcher;


/**
 * This filter filters only those resource Files that matches any of the registered file extension patterns.
 * All other resource types (Folders, Projects, etc) are passed.
 * 
 * @author Jose Manuel García Valladolid
 */
@SuppressWarnings("restriction")
public class FileExtensionResourceFilter extends ViewerFilter {

	private Map<String,StringMatcher> patternMatchers = new HashMap<String,StringMatcher>();
	
	
	public void registerExtensionPattern(String extPattern){
		if(!patternMatchers.containsKey(extPattern)){
			patternMatchers.put(extPattern, new StringMatcher(extPattern,false,false));
		}
	}
	
	public void unregisterExtensionPattern(String extPattern){
		if(patternMatchers.containsKey(extPattern)){
			patternMatchers.remove(extPattern);
		}
	}
	
	public String[] getRegisteredExtensionPatterns(){
		return patternMatchers.keySet().toArray(new String[0]);
	}
	
	public boolean isRegisteredExtensionPattern(String extPattern){
		return patternMatchers.containsKey(extPattern);
	}
	
	@Override
	public boolean select(Viewer viewer, Object parentElement, Object element) {
		
		 IResource resource = null;
	        if (element instanceof IResource) {
	            resource = (IResource) element;
	      
	            String name = resource.getName();
	            if(resource instanceof File){
	            	Iterator<String> iter = patternMatchers.keySet().iterator();
	            	while(iter.hasNext()){
	            		if(patternMatchers.get(iter.next()).match(name))
	            			return true;
	            	}
		            return false;
	            }else
	            	return true;
	        }
	        return false;
	        
	}

}
