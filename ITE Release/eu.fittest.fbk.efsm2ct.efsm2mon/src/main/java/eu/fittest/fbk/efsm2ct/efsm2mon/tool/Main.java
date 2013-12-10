/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package eu.fittest.fbk.efsm2ct.efsm2mon.tool;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import java.util.Map;

import eu.fittest.fbk.efsm2ct.efsm2mon.Version;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.generator.TemplateBasedGeneratorInterface;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.generator.velocity.VelocityBasedGenerator;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.indent.Indenter;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.indent.JavaIndenter;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.model.Model;
import eu.fittest.fbk.efsm2ct.efsm2mon.tool.parser.FsmParser;

/**
 * 
 * @author tiella
 */
public class Main {

	public enum OutputType {

		DOT_ALIAS, DOT_FITTEST, DOT, JAVA, GML, GML_ALIAS, GENERIC
	}

	private static String prefix;
	

	/**
	 * @param args
	 *            the command line arguments
	 */
	public static void main(String[] args) {

		int argc = 0;
		final int argsRequired = 2;

		boolean onlyOut = true;
		String destDirName = ".";
		String ofileName = null;
		OutputType otype = OutputType.JAVA;
		boolean hasRunControl = false;
		String templateFile = null;
		String ext = ".txt";

		while (args.length - argc >= argsRequired + 1) {

			// some options should be specified
			String opt = args[argc];
			argc++;

			if (!opt.startsWith("-")) {
				usage();
				return;
			}

			if (opt.equals("--dot")) {
				otype = OutputType.DOT;
			}

			if (opt.equals("--dota")) {
				otype = OutputType.DOT_ALIAS;
			}

			if (opt.equals("--gml")) {
				otype = OutputType.GML;
			}

			if (opt.equals("--gmla")) {
				otype = OutputType.GML_ALIAS;
			}

			if (opt.equals("--rc")) {
				hasRunControl = true;
			}

			if (opt.equals("--java-tmpl")) {
				templateFile = args[argc];
				argc++;
			}

			if (opt.equals("--tmpl")) {
				otype = OutputType.GENERIC;
				templateFile = args[argc];
				argc++;
			}
			

			if (opt.equals("-d")) {

				onlyOut = false;

				destDirName = args[argc];
				argc++;

			}

			if (opt.equals("-o")) {

				ofileName = args[argc];

				argc++;

			}
			
			if (opt.equals("-p")) {

				prefix = args[argc];

				argc++;

			}

		}

		if (args.length - argc != argsRequired) {

			usage();
			return;
		}

		String fsmFilePath = args[argc];
		argc++;
		String packageName = args[argc];
		argc++;

		String outFile = null;

		Model m;
		try {
			m = FsmParser.parse(new FileReader(fsmFilePath));

			StringBuilder annotation = new StringBuilder();

			if (m.checkSemantic(annotation)) {

				switch (otype) {
				case DOT:
				case DOT_ALIAS:
					outFile = runDot(m, packageName, destDirName, onlyOut, otype);
					break;
				case GML_ALIAS:
				case GML:
					outFile = runGml(m, packageName, destDirName, onlyOut, otype);
					break;
				case JAVA:
					outFile = runJava(m, packageName, prefix, destDirName, onlyOut, hasRunControl, templateFile);
					break;
				case GENERIC:
					File fout = new File(destDirName, ofileName);
					outFile = run(m, packageName, fout, onlyOut, templateFile, null, hasRunControl);
					break;

				}

				if (outFile == null) {
					System.err.println("ERROR: Can't generate.");
				}

			} else {
				System.err.println("Semantic Errors:" + annotation);
			}

		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (FsmTesterException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

	public static String runJava(Model m, String packageName, String prefix, String destDirName, boolean onlyOut, boolean hasRunControl, String tmplFileName) throws FsmTesterException, IOException {

		File dout = new File(destDirName);

		String packageDir = packageName.replace(".", File.separator);

		File packdir = new File(dout, packageDir);

		packdir.mkdirs();

		String ext = ".java";

		File fout = new File(packdir, prefix+m.getSutName() + ext);

		return run(m, packageName, fout, onlyOut, tmplFileName, new JavaIndenter(), hasRunControl);

	}

	public static String runDot(Model m, String packageName, String destDirName, boolean onlyOut, OutputType otype) throws FsmTesterException, IOException {

		String templateFileName = null;

		switch (otype) {

		case DOT_ALIAS:
			templateFileName = "eu/fittest/fbk/efsm2ct/efsm2mon/vm/dot-alias.vm";
			break;
		case DOT_FITTEST:
			templateFileName = "eu/fittest/fbk/efsm2ct/efsm2mon/vm/dot-fittest.vm";
			break;
		case DOT:
			templateFileName = "eu/fittest/fbk/efsm2ct/efsm2mon/vm/dot.vm";
			break;
		default:
			throw new RuntimeException("unexpected type:" + otype);

		}

		String ext = ".dot";

		File fout = new File(destDirName, m.getSutName() + ext);

		return run(m, packageName, fout, onlyOut, templateFileName, null, false);

	}

	public static String runGml(Model m, String packageName, String destDirName, boolean onlyOut, OutputType otype) throws FsmTesterException, IOException {

		String templateFileName = "eu/fittest/fbk/efsm2ct/efsm2mon/vm/gml.vm";
		String ext = ".gml";

		switch (otype) {

		case GML_ALIAS:
			templateFileName = "eu/fittest/fbk/efsm2ct/efsm2mon/vm/gml-alias.vm";
			break;
		case GML:
			break;
		default:
			throw new RuntimeException("unexpected type:" + otype);

		}

		File fout = new File(destDirName, m.getSutName() + ext);

		return run(m, packageName, fout, onlyOut, templateFileName, null, false);
	}

	public static String run(Model m, String packageName, File fout, boolean onlyOut, String templateFileName, Indenter indenter, boolean hasRunControl) throws FsmTesterException, IOException {
		return run(m, packageName, fout, onlyOut, templateFileName, indenter, hasRunControl, null);
	}

	public static String run(Model m, String packageName, File fout, boolean onlyOut, String templateFileName, Indenter indenter, boolean hasRunControl, Map<String, Object> additionalData)
			throws FsmTesterException, IOException {
		
	
		m.setHasRunControl(hasRunControl);
		
		Generator generator = new Generator();

		generator.setModel(m);
		
		generator.setAdditionalData(additionalData);
		
		generator.setIndenter(indenter);
		
		generator.setPackageName(packageName);
		
		generator.setTemplateFileName(templateFileName);
		
		Writer writer = null;
		
		String res = null;
		
		
		if (onlyOut) {

			writer = new PrintWriter(System.out);
			
			res = "<stdout>";

		} else {

			writer = new PrintWriter(fout);

			res = fout.getAbsolutePath();
		}
		
		generator.setFout(writer);
		
		generator.run();
		
		
		if (!onlyOut) {
			writer.close();
			System.out.println("file generated in file: " + fout.getAbsolutePath());
		}
		
		return res;

	}

	
	public static void usage() {
		System.out.println("efsm2mon - ver " + Version.getVersion());
		System.out.println("expected arguments: [-d <dest dir>] [--dot | --dota | --gml --gmla] [--rc] [--java-tmpl <template-file> | --tmpl <template-file> ] <fsm> <package>"); // [{-vl
																																					// |
																																					// -st}]
	}
}
