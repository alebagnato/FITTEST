package eu.fittest.eclipse.oraclesuite.wizards;

import java.io.File;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.Wizard;

import eu.fittest.eclipse.gui.Activator;
import eu.fittest.eclipse.oraclesuite.actions.PerlScripts;
import eu.fittest.project.config.OracleType;


public class InferlloWizard extends Wizard {

	InferlloPage inputPage = null;

	private String logDir ="";

	public InferlloWizard(String logDir) {
		super();
		this.logDir = logDir;
	}


	public boolean performFinish() {
		if (inputPage != null){

			final File logFolder = new File(inputPage.getLogFolder());

			final String _GHRCTopts = inputPage.getGHRCTopts();
			final String _oracleFile = inputPage.getOracleFile();
			final String _reportFile = inputPage.getReportFile();
			final List<String> _functionsToInclude = inputPage.getFunctionToInclude();
			final String _otherLLOoptions = inputPage.getOtherLLOOption();


			Job generateJob = new Job("Low level oracle inference"){

				protected IStatus run(final IProgressMonitor monitor) {	

					PerlScripts.inferllo(logFolder, _GHRCTopts, _oracleFile, _reportFile, _functionsToInclude, _otherLLOoptions);
					return new Status(IStatus.OK, "FITTEST low level oracle inference", 
							IStatus.OK, "Finish low level oracle inference!", null);
				}
			};
			generateJob.schedule();

			return true;
		}
		return false;

	}


	public void addPages() {

		OracleType oracleParams = Activator.getDefault().getActiveProjectConfig().getOracle();

		ImageDescriptor imgDes = Activator.getImageDescriptor("icons" + File.separator + "fittest-logo-small.jpg");
		this.inputPage = new InferlloPage("Infer Low level Oracle");
		this.inputPage.setTitle("Infer low level oracles");
		this.inputPage.setDescription("Select the logs for infer low level oracles");
		this.inputPage.setImageDescriptor(imgDes);

		this.inputPage.setLogFolder(this.logDir);

		//GHRCTopts
		String temp = oracleParams.getGHCRTopts();
		if (temp == null)
			temp = "";
		this.inputPage.setGHRCTopts(temp);


		//Oracle File
		temp = oracleParams.getOracleFile();
		if ( (temp == null) || (temp.equals("")) )
			temp = "oracle.inv";
		this.inputPage.setOracleFile(temp);

		//Report File
		temp = oracleParams.getReportFile();
		if ( (temp == null) || (temp.equals("")) )
			temp = "report.txt";
		this.inputPage.setReportFile(temp);


		//FunctionsToInclude
		temp = oracleParams.getFunctionsToInclude();
		if (temp == null)
			temp = "";
		this.inputPage.setFunctionToInclude(temp);

		//LLOOption
		temp = oracleParams.getLloOption();
		if (temp == null)
			temp = "";
		this.inputPage.setOtherLLOOption(temp);

		addPage(inputPage);
	}



}
