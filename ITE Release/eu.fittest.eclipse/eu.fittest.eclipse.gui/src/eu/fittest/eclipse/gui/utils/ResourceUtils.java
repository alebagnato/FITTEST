package eu.fittest.eclipse.gui.utils;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Vector;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.navigator.CommonNavigator;
import org.eclipse.ui.navigator.CommonViewer;

import eu.fittest.eclipse.gui.nature.IFITTESTFolderConstants;

public class ResourceUtils {
	
	/*
	 * Get currently selected folder
	 */
	static public IFolder getFolder() {
		
		CommonNavigator navigator =(CommonNavigator) Viewer.getView(IFITTESTFolderConstants.NAVIGATOR);
		
		if(navigator!=null){
			CommonViewer viewer = navigator.getCommonViewer();
			if (viewer != null) {
				ISelection selection = viewer.getSelection();
				if (selection instanceof IStructuredSelection && !selection.isEmpty()) {
					IStructuredSelection sel = (IStructuredSelection) selection;
					Object object = sel.getFirstElement();
					return (object instanceof IFolder && !(object instanceof IProject))? (IFolder)object:null;
				}
			}
		}
		return null;
	}
	
	
    public static void createFolder(IFolder folder) throws CoreException {
        IContainer parent = folder.getParent();
        if (parent instanceof IFolder) {
            createFolder((IFolder) parent);
        }
        if (!folder.exists()) {
            folder.create(false, true, null);
        }
    }
    
	static public Collection<File> collectFiles(File folder, String extension) throws CoreException{
		Collection<File> testcases = new Vector<File>();
		for(File file: folder.listFiles()){
			if(file.isDirectory()){
				testcases.addAll(collectFiles(file, extension));
			}
			else if(file.getName().endsWith(extension)){
				testcases.add(file);
			}
		}
		return testcases;
	}
	
	
	/**
	 * List recursively all files that have the extension and prefix
	 * 
	 * @param folder, extension
	 * @return List<IFile> of files
	 * @throws CoreException 
	 * 
	 * @author cdnguyen
	 * 
	 */
	public static List<IFile> collectFiles(IFolder folder, String fileExtension, String fileNamePrefix) throws CoreException{
		List<IFile> fileList = new ArrayList<IFile>();
		if (folder != null && fileExtension != null){
			for (IResource r : folder.members()){
				if (r instanceof IFile){
					boolean checkExtension = false;
					if (fileExtension != null && r.getName().endsWith(fileExtension)){
						checkExtension = true;
					} else if (fileExtension == null){
						checkExtension = true;
					}
					boolean checkPrefix = false;
					if (fileNamePrefix != null && r.getName().startsWith(fileNamePrefix)){
						checkPrefix = true;
					} else if (fileNamePrefix == null){
						checkPrefix = true;
					}
					if (checkExtension && checkPrefix){
						fileList.add((IFile) r);
					}
				} else if (r instanceof IFolder){
					fileList.addAll(collectFiles((IFolder) r, fileExtension, fileNamePrefix));
				}
			}
		}
		return fileList;
	}
	
	
	/**
	 * Method that locate the resouce and return OS-specific absolute path
	 * 
	 * @param project
	 * @param memberResourcePath
	 * @return
	 */
	public static String getOSAbsolutePath(IProject project, String memberResourcePath){
		IResource res = project.findMember(memberResourcePath);
		if (res != null){
			return res.getLocation().toOSString();
		}
		
		return null;
	}
	
}
