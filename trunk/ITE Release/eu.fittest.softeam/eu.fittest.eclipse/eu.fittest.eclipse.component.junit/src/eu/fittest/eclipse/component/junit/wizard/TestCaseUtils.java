package eu.fittest.eclipse.component.junit.wizard;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.URISyntaxException;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jdt.core.compiler.batch.BatchCompiler;
import org.eclipse.jface.wizard.ProgressMonitorPart;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.console.MessageConsole;
import org.eclipse.ui.console.MessageConsoleStream;
import org.osgi.framework.Bundle;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.eclipse.component.junit.Activator;
import eu.fittest.eclipse.gui.nature.IFITTESTFolderConstants;
import eu.fittest.eclipse.gui.utils.ResourceUtils;
import eu.fittest.eclipse.gui.utils.Viewer;

public class TestCaseUtils {
	static public void copyTestCases(IFolder targetDir, List<IFile> testcases) throws CoreException{
		for(IFile tc : testcases){
			IPath packagePath = tc.getProjectRelativePath().makeRelativeTo(tc.getProject().getFolder(IFITTESTFolderConstants.TEST_SUITES).getProjectRelativePath());			
			IPath targetFile = targetDir.getProjectRelativePath().append(packagePath).makeAbsolute();				
			IFolder targetPackage = targetDir.getProject().getFolder(targetFile.removeLastSegments(1));
			if(!targetPackage.exists()) ResourceUtils.createFolder(targetPackage);
			tc.copy(targetDir.getFullPath().append(packagePath), true, new ProgressMonitorPart(Display.getCurrent().getActiveShell(), null));
		}
	}
	
	static public  void compileTestCases(File outputFolder, IFolder session, Collection<File> testcases) throws IOException, URISyntaxException, FITTESTException {
		StringBuilder builder = new StringBuilder();
		Iterator<File> it = testcases.iterator();
		while(it.hasNext()){
			File f = it.next();
			builder.append("\""+f.getAbsolutePath()+"\" ");
		}
		builder.append("-d \""+outputFolder.getAbsolutePath()+"\" ");
		
		Bundle bundle = Platform.getBundle(Activator.PLUGIN_ID);		
		File libFolder = new File(FileLocator.toFileURL(bundle.getEntry("resources/lib")).getFile());	
		builder.append("-classpath ");
		File[] libFiles = libFolder.listFiles();
		for(int i=0;i<libFiles.length;i++){
			builder.append("\""+libFiles[i].getAbsolutePath()+"\"");
			if(i<libFiles.length-1) builder.append(File.pathSeparator);
		}
		builder.append(" -1.6");
		
		Logger.getAnonymousLogger().log(Level.INFO,"Compilation string: "+builder.toString());
		
		MessageConsole console = Viewer.findConsole(session.getName());
		MessageConsoleStream stream = console.newMessageStream();
		boolean success = BatchCompiler.compile(builder.toString(), new PrintWriter(stream), new PrintWriter(stream), new CompilationProgressMonitor());
		if(!success){
			console.activate();
			throw new FITTESTException("Can't compile test cases for session "+session.getName());
		}
	}
}
