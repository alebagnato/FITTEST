package eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Properties;

public class PropertyBasedLogFilterFactory {

	public LogFilter createFilter(File file) throws IOException {

		Properties properties = new Properties();

		FileReader fw = new FileReader(file);

		properties.load(fw);

		fw.close();

		return createFilter(properties);

	}

	public LogFilter createFilter(Properties properties) throws IOException {

		LogFilter filter = new LogFilter();
		int k = 1;

		while (true) {

			String line = properties.getProperty(String.format(
					"filter.regex.%d", k));
			k++;

			if (line == null) {
				break;
			}

			TargetMatchingRegExpEventCondition cond = new TargetMatchingRegExpEventCondition(
					line);
			filter.addCondition(cond);

		}

		return filter;
	}

}
