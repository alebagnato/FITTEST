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


public class InferDaikonWizard extends Wizard {

	InferDaikonPage inputPage = null;
	String logDir = "";

	public InferDaikonWizard(String logDir) {
		super();
		this.logDir = logDir;
	}


	public boolean performFinish() {
		if (inputPage != null){

			final String _logFolder = inputPage.getLogDir();
			final String _GHRCTopts = inputPage.getGHRCopts();
			final String _oracleFile = inputPage.getOracleFile();
			final String _reportFile = inputPage.getReportFile();

			final String _eventsToInclude = inputPage.getEventsToInclude();
			final List<String> _fieldsToInclude = inputPage.getFieldsToInclude();


			Job generateJob = new Job("Infer daikon"){

				protected IStatus run(final IProgressMonitor monitor) {	


					PerlScripts.inferDaikon(_logFolder, _GHRCTopts, _oracleFile, _reportFile, _eventsToInclude, _fieldsToInclude);
					return new Status(IStatus.OK, "FITTEST oracle plugin", 
							IStatus.OK, "Finish infer daikon", null);
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
		inputPage = new InferDaikonPage("Infer Daikon");
		inputPage.setTitle("Infer low level oracle");
		inputPage.setDescription("Specify the parameters of the inference");
		inputPage.setImageDescriptor(imgDes);

		this.inputPage.setLogDir(this.logDir);
		
		//GHRCTopts
		String temp = oracleParams.getGHCRTopts();
		if (temp == null)
			temp = "";
		this.inputPage.setGHRCTopts(temp);
		
		//Oracle File
		temp = oracleParams.getOracleFile();
		if ((temp == null) || (temp.equals("")))
			temp = "oracle.inv";
		this.inputPage.setOracleFile(temp);
		
		
		//Report File
		temp = oracleParams.getReportFile();
		if ((temp == null) || (temp.equals("")))
			temp = "report.txt";
		this.inputPage.setReportFile(temp);
		
		//EventsToInclude
		temp = oracleParams.getEventsToInclude();
		if (temp == null)
			temp = "";
		this.inputPage.setEventsToInclude(temp);
		
		//FieldsToInclude
		temp = oracleParams.getFieldsToInclude();
		if (temp == null)
			temp = "";
		this.inputPage.setFieldsToInclude(temp);

		addPage(inputPage);
	}



}
