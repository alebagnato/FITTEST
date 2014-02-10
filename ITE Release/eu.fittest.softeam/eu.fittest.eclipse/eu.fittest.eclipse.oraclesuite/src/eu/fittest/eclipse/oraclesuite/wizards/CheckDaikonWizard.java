package eu.fittest.eclipse.oraclesuite.wizards;

import java.io.File;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.Wizard;

import eu.fittest.eclipse.gui.Activator;
import eu.fittest.eclipse.oraclesuite.actions.PerlScripts;
import eu.fittest.project.config.OracleType;


public class CheckDaikonWizard extends Wizard {

	CheckDaikonPage inputPage = null;

	private String logFolder = "";

	public CheckDaikonWizard(String logFolder) {
		super();
		this.logFolder = logFolder;
	}


	public boolean performFinish() {
		if (this.inputPage != null){

			final String logFolder = this.inputPage.getOldLogFolder();
			final String _newlogFolder = this.inputPage.getNewLogFolder();
			final String _oracleFileName = this.inputPage.getOracleFile();
			final String _violationFileName = this.inputPage.getViolationRecordFile();
			final String _reportFileName = this.inputPage.getReportFile();

			Job generateJob = new Job("Checking daikon"){

				protected IStatus run(final IProgressMonitor monitor) {	


					PerlScripts.checkDaikon(logFolder, _newlogFolder, _oracleFileName, _violationFileName, _reportFileName);
					return new Status(IStatus.OK, "FITTEST tranformation plugin", 
							IStatus.OK, "Finish mining domain input file!", null);
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
		this.inputPage = new CheckDaikonPage("Check Property-based oracles");
		this.inputPage.setTitle("Checking Property-based oracles");
		this.inputPage.setDescription("Check Property-based oracles for high-level events");
		this.inputPage.setImageDescriptor(imgDes);

		this.inputPage.setOldLogFolder(this.logFolder);


		//Oracle File
		String temp = oracleParams.getOracleFile();
		if ( (temp == null) || (temp.equals("")) )
			temp = "oracle.inv";
		this.inputPage.setOracleFile(temp);

		//Report File
		temp = oracleParams.getReportFile();
		if ( (temp == null) || (temp.equals("")) )
			temp = "report.txt";
		this.inputPage.setReportFile(temp);

		//Violation File
		temp = oracleParams.getViolationFile();
		if ( (temp == null) || (temp.equals("")) )
			temp = "violations.txt";
		this.inputPage.setViolationRecordFile(temp);


		addPage(inputPage);
	}



}
