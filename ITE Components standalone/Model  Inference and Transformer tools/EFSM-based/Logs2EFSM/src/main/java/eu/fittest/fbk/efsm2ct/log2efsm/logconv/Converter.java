package eu.fittest.fbk.efsm2ct.log2efsm.logconv;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;
import java.util.Set;
import java.util.logging.Filter;
import java.util.logging.Logger;

import eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv.ConverterException;
import eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv.FileBasedLogFilterFactory;
import eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv.FlexstoreFilter;
import eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv.LogFilter;
import eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv.LogReader;
import eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv.ModelBasedConverter;
import eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv.MutatorDescriptor;
import eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv.MutatorsExtractor;
import eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv.PropertyBasedLogFilterFactory;
import eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv.RefuseNoneLogFilter;
import eu.fittest.fbk.efsm2ct.log2efsm.logconv.conv.TargetMatchingRegExpEventCondition;
import eu.fittest.fbk.efsm2ct.log2efsm.logconv.model.Event;

public class Converter {

	public enum FilterType { FILE, PRORPERTIES };
	
	private static Logger logger = Logger.getAnonymousLogger();
	private ModelBasedConverter mbConvert = new ModelBasedConverter();
	private FilterType filterType = FilterType.PRORPERTIES;
	

	/**
	 * @param String
	 *            inputfolderpath: path of the folder that contains the xml logs
	 * @param String
	 *            prefix_outputFileName: prefix of the name for the generated
	 *            log files
	 * @throws Exception 
	 */

	public void run(File[] fileList, File mutatorsDescriptionFile, String outputFileNamePrefix, File targetDirectory, File filterFile) throws Exception {

		extractMutators(fileList, mutatorsDescriptionFile, filterFile);

		convert(fileList, outputFileNamePrefix, targetDirectory, filterFile);

	}

	private void extractMutators(File[] fileList, File outputFile, File filterFile) throws Exception {

		MutatorsExtractor ee = new MutatorsExtractor();

		for (File inFile : fileList) {

			logger.info("converting:" + inFile);

			LogReader logReader = new LogReader();

			List<Event> eventsIn = null;

			try {
			
			LogFilter filter = null;
			
			if (filterFile != null) {
			
				switch(filterType) {
				case FILE:
					filter = new FileBasedLogFilterFactory().createFilter(filterFile);
					break;
				case PRORPERTIES:
					filter = new PropertyBasedLogFilterFactory().createFilter(filterFile);
					break;
				}
							
				
			} else {
				
				filter = new RefuseNoneLogFilter();
				
			}
			
			try {
				eventsIn = logReader.read(inFile);
				
			} catch (ConverterException e) {
				e.printStackTrace();
				throw e;
			}
						
			List<Event> events = filter.filter(eventsIn);

			System.out.println("events read:" + events.size());
		
			ee.extractEvents(events);
			
			} catch (Exception ex) {
				ex.printStackTrace();
				throw ex;
			}

		}

		saveMutatorsTable(outputFile, ee.getMds());

	}

	private void saveMutatorsTable(File outputFile, Set<MutatorDescriptor> mds) {

		try {
			PrintWriter pw = new PrintWriter(outputFile);

			int alias = 1;

			for (MutatorDescriptor d : mds) {

				pw.format("%s/%s/%d\n", d.getTarget(), d.getEvent(), d.getArity());
				alias++;
			}

			pw.close();

		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			System.exit(1);
		}

	}

	private void convert(File[] filelist, String prefix_outputFileName, File outputFolder, File filterFile) {

		if (!outputFolder.exists()) {
			outputFolder.mkdirs();
		}

		LogFilter filter = null;// new FlexstoreFilter(); // TODO read the filter from a file
		
		try {
		
		if (filterFile != null) {
			filter = new FileBasedLogFilterFactory().createFilter(filterFile);
		} else {
			filter = new RefuseNoneLogFilter();
		}
		
		for (int i = 0; i < filelist.length; i++) {

			if (filelist[i].isFile()) {

				try {

					File inFile = filelist[i];
					File outFile = new File(outputFolder, prefix_outputFileName + "_" + i + ".txt");
					// for debugging
					// new File("/dev/stdout");

					logger.info("converting:" + inFile);

					mbConvert.convert(inFile, outFile, filter);

					logger.info("converted to:" + outFile.getAbsolutePath());

				} catch (ConverterException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}

			}

		}
		
		} catch (Exception ex) {
			ex.printStackTrace();
		}

	}
}
