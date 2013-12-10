package eu.fittest.eventSequenceGenerator.data.config;

import javax.xml.bind.annotation.XmlElement;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import eu.fittest.eventSequenceGenerator.data.TypeOfCoverage;
import eu.fittest.eventSequenceGenerator.diversity.TypeOfFitnessFunction;

/**
*
* @author Alessandro
*
*/
@XmlRootElement(name = "config")
//If you want you can define the order in which the fields are written
//Optional
//@XmlType(propOrder = { "folderOriginalTraces", "typeOfCoverage", "folderNewTraces", "maxLengthGeneratedTraces", "k", "typeOfVisit", "fileNameOfAbstractTC", "typeOfFitness",  "N_max", "t_initial" , "SuiteIt_max", "TcIt_max" })
public class Xml {

	private String folderTraces;
	private String folderOriginalFSM;
	private String typeOfCoverage;
	private String folderNewEventSequences;
	private int maxLengthGeneratedTraces;
	private int k;
	private String typeOfVisit;
	private String fileNameOfAbstractTC;
	//private String typeOfFitness;
	private int n_max;
	private double t_initial;
	private int suiteIt_max;
	private int tcIt_max;


	public String getFolderTraces() {
		return this.folderTraces;
	}

	public void setFolderTraces(String folderTraces) {
		this.folderTraces = folderTraces.trim();
	}
	
	// If you like the variable name, e.g. "name", you can easily change this
	// name for your XML-Output:
	public String getFolderOriginalFSM() {
		return this.folderOriginalFSM;
	}

	public void setFolderOriginalFSM(String folderOriginalFSM) {
		this.folderOriginalFSM = folderOriginalFSM.trim();
	}

	public String getTypeOfCoverage() {
		return this.typeOfCoverage;
	}

	public void setTypeOfCoverage(String typeOfCoverage) {
		this.typeOfCoverage = typeOfCoverage.trim();
	}

	public String getFolderNewEventSequences() {
		return this.folderNewEventSequences;
	}

	public void setFolderNewEventSequences(String folderNewEventSequences) {
		this.folderNewEventSequences = folderNewEventSequences.trim();
	}

	public int getMaxLengthGeneratedTraces() {
		return this.maxLengthGeneratedTraces;
	}

	public void setMaxLengthGeneratedTraces(int maxLengthGeneratedTraces) {
		this.maxLengthGeneratedTraces = maxLengthGeneratedTraces;
	}

	public int getK() {
		return this.k;
	}

	public void setK(int k) {
		this.k = k;
	}

	public String getTypeOfVisit() {
		return this.typeOfVisit;
	}

	public void setTypeOfVisit(String typeOfVisit) {
		this.typeOfVisit = typeOfVisit.trim();
	}

	public String getFileNameOfAbstractTC() {
		return this.fileNameOfAbstractTC;
	}

	public void setFileNameOfAbstractTC(String fileNameOfAbstractTC) {
		this.fileNameOfAbstractTC = fileNameOfAbstractTC.trim();
	}

	public int getN_max() {
		return this.n_max;
	}
	
	public void setN_max(int nMax) {
		this.n_max = nMax;
	}

/*  public void setN_max(String N_max) {
		 if (!N_max.equals("")) this.N_max=new Integer(N_max).intValue();
	}*/

	public double getT_initial() {
		return this.t_initial;
	}
	
	public void setT_initial(double tInitial) {
		this.t_initial = tInitial;
	}

/*	public void setT_initial(String t_initial) {
		 if (!t_initial.equals("")) this.t_initial=new Double(t_initial).doubleValue();
	}*/



	public void setSuiteIt_max(int suiteItMax) {
		this.suiteIt_max = suiteItMax;
	}

	public int getSuiteIt_max() {
		return this.suiteIt_max;
	}


	/*	public void setSuiteIt_max(String SuiteIt_max) {
		if (!SuiteIt_max.equals("")) this.SuiteIt_max=new Integer(SuiteIt_max).intValue();
	}*/

	public int getTcIt_max() {
		return this.tcIt_max;
	}

/*		public void setTcIt_max(String TcIt_max) {
		if (!TcIt_max.equals("")) this.TcIt_max=new Integer(TcIt_max).intValue();
	}*/

	
	public void setTcIt_max(int tcItMax) {
		this.tcIt_max = tcItMax;
	}

}
