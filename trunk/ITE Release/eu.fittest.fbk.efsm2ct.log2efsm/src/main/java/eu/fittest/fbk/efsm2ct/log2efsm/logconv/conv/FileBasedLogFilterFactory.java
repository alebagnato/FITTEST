package eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class FileBasedLogFilterFactory {
	
	public LogFilter createFilter(File filterFile) throws IOException {
				
		BufferedReader br = new BufferedReader(new FileReader(filterFile));
		LogFilter filter = new LogFilter();
		
		String line = br.readLine();
		
		while (line != null) {
			
			TargetMatchingRegExpEventCondition cond = new TargetMatchingRegExpEventCondition(line);
			filter.addCondition(cond);
			line = br.readLine();
			
		}
		
		br.close();
		
		return filter;
	}

}
