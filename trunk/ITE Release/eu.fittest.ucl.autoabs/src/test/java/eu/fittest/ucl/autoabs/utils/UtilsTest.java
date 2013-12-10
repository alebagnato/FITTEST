package eu.fittest.ucl.autoabs.utils;

import static org.junit.Assert.*;

import java.io.File;
import java.io.FileWriter;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Vector;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.Ignore;

import eu.fittest.ucl.autoabs.utils.Utils;
import eu.fittest.ucl.autoabs.cluster.member.ConcreteState;

public class UtilsTest {
	
	private static final String TEST_FILE_NAME = "example.csv";
	private static final String[] CSV_LINES = {
		"EVENT,N",
		"START,0",
		"add,1",
		"add,2",
		"pay,-1"
		};
	
	private File testCSVFile = null;
	
	@Before
	public void setUp() throws Exception {
		this.tearDown();
		this.testCSVFile = new File(TEST_FILE_NAME);
		//populate with example values
		FileWriter fw = new FileWriter(this.testCSVFile);
		for(String line : CSV_LINES){
			fw.write(line + Utils.eol);
		}
		fw.close();
	}

	@After
	public void tearDown() throws Exception {
		if(this.testCSVFile != null){
			if(this.testCSVFile.exists())
				this.testCSVFile.delete();
			this.testCSVFile = null;
		}
	}

	@Ignore
	public void testParseCsvTrace() {
		List<String> csvFiles = new ArrayList<String>();
		csvFiles.add(TEST_FILE_NAME);
		
		Vector<List<TraceEvent>> events = Utils.computeAllTraceEvents(csvFiles);
		assertEquals("only 1 vector item", 1, events.size());
		
		List<TraceEvent> eventList = events.get(0);
		assertEquals("events == num csv lines", (CSV_LINES.length - 1), eventList.size());
		
		//check each state:
		for(int i = 0; i < eventList.size(); i++) {
			ConcreteState state = eventList.get(i).getConcreteState();
			List<BigDecimal> doubles = state.getDoubleValues();
			assertEquals("doubles == doublePositions.size()", 1, doubles.size());
			
			String[] csvValues = CSV_LINES[i + 1].split(",");
			BigDecimal referenceDouble = new BigDecimal(csvValues[1]);
			assertEquals("double in csv and state", referenceDouble, doubles.get(0));
		}
	}
}
