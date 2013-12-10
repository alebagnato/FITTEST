package eu.fittest.fbk.efsm2ct.tools.evosuite;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import eu.fittest.fbk.efsm2ct.tools.CommandService;

public abstract class JavaCommandService extends CommandService {

	
	private List<String> classpath = new ArrayList<String>();
	private File jarFile;

	public JavaCommandService() {
		super();
	}
	
	public void setJarFile(File jarFile) {
		this.jarFile = jarFile;
	}

	

	public void init() {

		addCmd("java");

		if (jarFile != null) {
			addCmd("-jar");
			addCmd(jarFile.getPath());
		}
		
		if (!classpath.isEmpty()) {
			addCmd("-cp");
			addCmd(buildClasspath());
		}

	}


	
	
	private String buildClasspath() {

		StringBuilder sb = new StringBuilder();

		if (!classpath.isEmpty()) {

			for (String path : classpath) {
				sb.append(path).append(':');
			}

			sb.deleteCharAt(sb.length() - 1);
		}

		return sb.toString();

	}

	public void addClasspath(String string) {
		classpath.add(string);

	}
	
	

}