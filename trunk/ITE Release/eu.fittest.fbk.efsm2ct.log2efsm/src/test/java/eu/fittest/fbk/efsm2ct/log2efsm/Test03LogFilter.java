package eu.fittest.fbk.efsm2ct.log2efsm;

import static org.junit.Assert.*;

import java.io.File;
import java.io.IOException;
import java.util.List;

import org.junit.Test;

import eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv.ConverterException;
import eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv.FileBasedLogFilterFactory;
import eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv.FlexstoreEventCondition;
import eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv.FlexstoreFilter;
import eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv.LogFilter;
import eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv.LogReader;
import eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv.TargetMatchingRegExpEventCondition;
import eu.fittest.fbk.efsm2ct.log2efsm.logconv.model.Event;

public class Test03LogFilter {

	// @Test
	public void test1() {
		
		System.out.println("\ntest1:\n");

		File f = new File("src/test/input/v_5898/log_3.xml");

		LogReader reader = new LogReader();

		try {

			List<Event> evs = reader.read(f);

			System.out.println("unfiltered events:" + evs.size());

			for (Event ev : evs) {

				System.out.println("Target: "
						+ ev.getRecordEvent().get("targetID").getValue());

			}

			LogFilter filter = new LogFilter();
			filter.addCondition(new FlexstoreEventCondition());

			List<Event> fevs = filter.filter(evs);

			System.out.println();

			System.out.println("filtered events:" + fevs.size());

			for (Event ev : fevs) {

				System.out.println("Target: "
						+ ev.getRecordEvent().get("targetID").getValue());

			}

		} catch (ConverterException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

	// @Test
	public void test2() {

		System.out.println("\ntest2:\n");
		
		File f = new File("src/test/input/v_5898/log_3.xml");

		LogReader reader = new LogReader();

		try {

			List<Event> evs = reader.read(f);

			System.out.println("unfiltered events:" + evs.size());

			for (Event ev : evs) {

				System.out.println("Target: "
						+ ev.getRecordEvent().get("targetID").getValue());

			}

			LogFilter filter = new FlexstoreFilter();

			List<Event> fevs = filter.filter(evs);

			System.out.println();

			System.out.println("filtered events:" + fevs.size());

			for (Event ev : fevs) {

				System.out.println("Target: "
						+ ev.getRecordEvent().get("targetID").getValue());

			}

		} catch (ConverterException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

	// @Test
	public void test3() {

		System.out.println("\ntest3:\n");
		
		File f = new File("src/test/input/v_5898/log_2.xml");

		LogReader reader = new LogReader();

		try {

			List<Event> evs = reader.read(f);

			System.out.println("unfiltered events:" + evs.size());

			for (Event ev : evs) {

				System.out.println("Target: "
						+ ev.getRecordEvent().get("targetID").getValue());

			}

			LogFilter filter = new FlexstoreFilter();

			List<Event> fevs = filter.filter(evs);

			System.out.println();

			System.out.println("filtered events:" + fevs.size());

			for (Event ev : fevs) {

				System.out.println("Target: "
						+ ev.getRecordEvent().get("targetID").getValue());

			}

		} catch (ConverterException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

	@Test
	public void test4() throws IOException, ConverterException {

		System.out.println("\ntest4:\n");
		
		File f = new File("src/test/input/v_5898_1/log_1376291796646.xml");


			 LogFilter filter = new FileBasedLogFilterFactory().createFilter(new File("src/test/input/flexstore_exclude_regex.txt"));		

		     LogReader reader = new LogReader();
			 
			List<Event> evs = reader.read(f);

			System.out.println("unfiltered events:" + evs.size());

			for (Event ev : evs) {

				System.out.println("Target: "
						+ ev.getRecordEvent().get("targetID").getValue());

			}

			List<Event> fevs = filter.filter(evs);

			System.out.println();

			System.out.println("filtered events:" + fevs.size());

			for (Event ev : fevs) {

				System.out.println("Target: "
						+ ev.getRecordEvent().get("targetID").getValue());

			}

		

	}

	
}
