package eu.fittest.fbk.efsm2ct.tools.comp;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import javax.tools.JavaCompiler;
import javax.tools.JavaCompiler.CompilationTask;
import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.ToolProvider;

import eu.fittest.fbk.efsm2ct.tools.files.FileFinder;

/**
 * A simple interface to an external java compilation service
 * 
 * @author tiella
 * 
 */

public class JavaCompileService {

	private String targetDirectory;
	private List<String> classpath = new ArrayList<String>();
	private JavaCompiler jc;
	private StandardJavaFileManager sjfm;
	Iterable<? extends JavaFileObject> fileObjects;

	public JavaCompileService(String sourceDirectory, String targetDirectory) throws IOException {
		super();

		this.jc = ToolProvider.getSystemJavaCompiler();
		this.sjfm = jc.getStandardFileManager(null, null, null);

		FileFinder fm = new FileFinder(".java");

		List<File> sources = fm.findFilesRecursively(new File(sourceDirectory));

		this.fileObjects = sjfm.getJavaFileObjects(sources.toArray(new File[0]));
		this.targetDirectory = targetDirectory;
	}

	public void addPath(String path) {
		classpath.add(path);

	}

	public void run() {

		List<String> options = new ArrayList<String>();

		options.add("-d");
		options.add(targetDirectory);

		if (!classpath.isEmpty()) {

			StringBuilder sb = new StringBuilder();

			for (String path : classpath) {

				sb.append(path).append(':');

			}

			sb.deleteCharAt(sb.length() - 1);

			options.add("-classpath");
			options.add(sb.toString());

		}

		CompilationTask task = jc.getTask(null, null, null, options, null, fileObjects);

//		logger.info("user.dir=" + System.getProperty("user.dir"));
//		logger.info("compiling with options: " + options + " " + fileObjects);

		task.call();

		try {
			sjfm.close();
		} catch (IOException e) {

			e.printStackTrace();
		}

		System.out.println("Class has been successfully compiled");
	}

}
