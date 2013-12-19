package eu.fittest.fbk.efsm2ct.log2efsm.infer.formats;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintWriter;

/**
 * Simple println-based code generator
 * 
 * @author tiella
 * 
 */

public class FsmtestDriverGenerator {

	private MutatorDescriptionSet mds;
	private String className;
	private String packageName;

	public FsmtestDriverGenerator(String packageName, String className, MutatorDescriptionSet mds) throws FileNotFoundException {
		this.mds = mds;
		this.className = className;
		this.packageName = packageName;
	}

	public void generate(String outputDirectoryFn) throws FileNotFoundException {

		File outDir = new File(outputDirectoryFn);
		File outFile = new File(outDir, className + ".java");

		StringBuilder sb = new StringBuilder();

		sb.append("package " + packageName + ";\n");

		sb.append("\n");

		sb.append("import eu.fittest.fbk.efsm2ct.flexdrv.FlexDriverException;\n");
		sb.append("import eu.fittest.fbk.efsm2ct.flexdrv.FlexDriver;\n");

		sb.append("\n");

		sb.append("public class ");
		sb.append(className).append(" ");
		sb.append("extends AbstractFlexSut ");
		sb.append(" {\n");

		for (MutatorDescriptor m : mds.getValues()) {

			StringBuilder formParams = new StringBuilder();
			StringBuilder actParams = new StringBuilder();

			for (int i = 0; i < m.getArity(); i++) {

				formParams.append("int ").append("x").append(i);
				actParams.append("Integer.toString(").append("x").append(i).append(")");

				if (i < m.getArity() - 1) {

					formParams.append(", ");
					actParams.append(", ");

				}

			}

			sb.append(String.format("public void %s(%s) throws FlexDriverException {\n", m.getName(), formParams));
			sb.append(String.format("\tsd = flexDriver.invoke(\"%s\",\"%s\"", m.getTarget(), m.getEvent()));

			if (m.getArity() > 0) {
				sb.append(", ");
				sb.append(actParams);
			}

			sb.append(");\n");

			sb.append("}\n");

		}

		sb.append("}\n");

		PrintWriter pw = new PrintWriter(new FileOutputStream(outFile));
		pw.println(sb);
		pw.close();

	}

}
