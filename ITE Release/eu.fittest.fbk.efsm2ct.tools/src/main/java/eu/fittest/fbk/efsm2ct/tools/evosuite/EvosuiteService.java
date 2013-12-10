package eu.fittest.fbk.efsm2ct.tools.evosuite;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * provides the service of running Evosuite against a specified class
 * 
 * @author tiella
 * 
 */

public class EvosuiteService extends JavaCommandService {

	public static final String global_timeout = "global_timeout";
	public static final String startup_method = "startup_method";
	public static final String shutdown_method = "shutdown_method";
	public static final String print_to_system = "print_to_system";

	public static final String population = "population";
	public static final String make_accessible = "make_accessible";
	public static final String search_budget = "search_budget";
	public static final String minimize = "minimize";
	public static final String log_timeout = "log_timeout";
	public static final String timeout = "timeout";
	public static final String chromosome_length = "chromosome_length";
	public static final String minimization_timeout = "minimization_timeout";
	public static final String stopping_port = "stopping_port";
	public static final String report_dir= "report_dir"; // evosuite-report

	public static int stoppingPort = 7000;
	private LogConsumer checker;

	private String evosuiteDirPath = ".";
	private String evosuiteJarName = "evosuite-minimal-1.0.0.jar";
	private String target;
	private Map<String, String> properties = new HashMap<String, String>();
	

	public EvosuiteService(String target) {
		this.target = target;
	}

	public String getEvosuiteDirPath() {
		return evosuiteDirPath;
	}

	public void setEvosuiteDirPath(String evosuiteDirPath) {
		this.evosuiteDirPath = evosuiteDirPath;
		setJarFile(new File(evosuiteDirPath,evosuiteJarName));
	}

	public String getEvosuiteJarName() {
		return evosuiteJarName;
	}

	public void setEvosuiteJarName(String evosuiteJarName) {
		this.evosuiteJarName = evosuiteJarName;
		setJarFile(new File(evosuiteDirPath,evosuiteJarName));
	}

	public void addProperty(String key, String value) {

		properties.put(key, value);

	}

	public void setKillChecker(LogConsumer checker) {
		this.checker = checker;
	}

	public int getStoppingPort() {
		return stoppingPort;
	}

//	public void setStoppingPort(int stoppingPort) {
//		this.stoppingPort = stoppingPort;
//	}

	public void init() {
		
		super.init();
		
		/*
		 * java -jar $EVODIR/evosuite-0.1-SNAPSHOT-jar-minimal.jar -cp $cp
		 * $strategy -class $sut -Dshow_progress=${show_progress}
		 * -Dstartup_method=_startup -Dshutdown_method=_shutdown
		 * -Dpopulation=$population -Dmake_accessible=false
		 * -Dsearch_budget=${search_budget} -Dminimize=$minimize
		 * -Dlog_timeout=true -Dtimeout=$timeout
		 * -Dglobal_timeout=${global_timeout}
		 * -Dchromosome_length=$chromosome_length
		 * -Dminimization_timeout=${minimization_timeout} -Dlog.level=info
		 * -Dprint_to_system=${print_to_system}
		 */
		
		addCmd("-generateSuite");
		addCmd("-class");
		addCmd(target);
		addCmd("-Dshow_progress=false");

		if (checker != null) {

			// cmd.add(String.format("-D%s=%s",EvosuiteService.stopping_port,Integer.toString(stoppingPort)));

			if (!properties.containsKey(EvosuiteService.stopping_port)) {
				properties.put(EvosuiteService.stopping_port, Integer.toString(stoppingPort));
			}

			if (!properties.get(EvosuiteService.print_to_system).equals("true")) {
				properties.put(EvosuiteService.print_to_system, "true");
			}
			

			addLogConsumer(checker);

		}

		for (Map.Entry<String, String> p : properties.entrySet()) {

			addCmd(String.format("-D%s=%s", p.getKey(), p.getValue()));

		}

		// cmd.add("-Dclient_on_thread=true");

	}


}
