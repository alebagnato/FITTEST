package eu.fittest.fbk.efsm2ct.plugin.utils;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;

public class ResourceUtils {

	public static List<IResource> match(IFolder folder, String regex) throws CoreException {

		Pattern pattern = Pattern.compile(regex);
		
		IResource [] members = folder.members();
		
		ArrayList<IResource> list = new ArrayList<IResource>();
		
		for (int i=0; i<members.length; i++) {
			
			if (pattern.matcher(members[i].getName()).find()) {
				list.add(members[i]);
			}
			
		}
		
		return list;

	}
	
	public static List<IResource> matchRecursively(IFolder folder, String regex) throws CoreException {

		List<IResource> res = match(folder, regex);
		
		for (IResource r : folder.members()) {
			
			if (r instanceof IFolder) {
				
				res.addAll(matchRecursively((IFolder)r, regex));
				
			}
			
		}
		
		
		return res;

	}
	
	/**
	 * clears the string passed as argument so that it could be a valid package name in Java
	 * @param str
	 * @return
	 */
	
	public static String clearForPackageName(String str) {
		
		return str.replaceAll("[ ]", "_");
		
	}
	
	
}
