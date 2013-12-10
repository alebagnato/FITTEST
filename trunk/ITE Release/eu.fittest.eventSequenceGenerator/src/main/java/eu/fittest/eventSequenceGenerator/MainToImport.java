package eu.fittest.eventSequenceGenerator;

import java.util.Vector;
import javax.xml.bind.Unmarshaller;
import java.io.*;
import javax.xml.bind.JAXBContext;

import eu.fittest.eventSequenceGenerator.data.config.Xml;
import eu.fittest.eventSequenceGenerator.data.TypeOfCoverage;
import eu.fittest.eventSequenceGenerator.data.TypeOfVisit;
import eu.fittest.eventSequenceGenerator.data.VisitorFactory;
import eu.fittest.eventSequenceGenerator.diversity.TypeOfFitnessFunction;
import eu.fittest.eventSequenceGenerator.eventSeqGenerator.EventSeqGenerator;


/**
 * Main class 
 * 
 * @author Alessandro Marchetto
 *
 */
public class MainToImport {
	private static final String CONFIG_XML = "config_sequenceExtractor.xml";

	EventSeqGenerator eventSeqGen=new EventSeqGenerator();
	
	public boolean executionFromProxy=false;
	
	boolean setupExecuted=false;
	
	//per coverage
	public String folderTraces="";	
	//per visiting, per SEM, per diversity
	public String folderOriginalFSM="";
	//			
	public String typeOfCoverage="";


	public String folderNewEventSequences="output"+System.getProperty("file.separator")+typeOfCoverage;
	int maxLengthGeneratedTraces=100;
	int k=2; //2,3,4,5
	String typeOfVisit=TypeOfVisit.givenLengthK.toString(); //BreadthFirstVisit,BreadthFirstVisitWithLoop,BreadthFirstVisitWithGlobalLoop,givenLengthK
	String fileNameOfAbstractTC="abstractTC";
		
	String typeOfFitness=TypeOfFitnessFunction.TC_absolute.toString();	
	int N_max=1000;
	double t_initial=5;
	int SuiteIt_max=100;
	int TcIt_max=100;
	
	Vector<eu.fittest.eventSequenceGenerator.data.Path> paths;
	
	/*
	 * to set up from the proxy
	 */
	public void setParameters_fromProxy(String visitorName,String inputModel,String folderinputTraces){
		typeOfCoverage=visitorName;
		folderOriginalFSM=inputModel;
		folderTraces=folderinputTraces;
		executionFromProxy=true;
	}
	
	/*
	 * to set up manually
	 */
	private void setParameters_fromProxy_default(){
		//public String typeOfCoverage="VISITOR_BREADTHFIRST";  					//Cu
		//public String typeOfCoverage="VISITOR_BREADTHFIRST_WITH_GLOBAL_LOOPS";  	//Cu
		//public String typeOfCoverage="VISITOR_BREADTHFIRST_WITH_LOCAL_LOOPS";  	//Cu
		//Ale
		//public String typeOfCoverage="VISITOR_BREADTHFIRST_2";
		//public String typeOfCoverage="VISITOR_BREADTHFIRST_2";
		//public String typeOfCoverage="VISITOR_COVERAGEWITHFRQ_LOGS";  			//logs
		//public String typeOfCoverage="VISITOR_xLeastFrequentEven_LOGS";  			//logs
		//public String typeOfCoverage="VISITOR_COVERAGE_UNIFORM_LOGS"; 			//logs
		//public String typeOfCoverage="VISITOR_COVERAGE_UNIFORM";
		//public String typeOfCoverage="VISITOR_SEQ_MAXK";
		//public String typeOfCoverage="VISITOR_SEQK";
		//public String typeOfCoverage="VISITOR_SEMK";
		//public String typeOfCoverage="VISITOR_SEM_MAXK";
		//public String typeOfCoverage="VISITOR_SEM_LastEventPair_MAXK";
		//public String typeOfCoverage="VISITOR_SEM_LastEventPair_K";; 
		//public String typeOfCoverage="VISITOR_ALT_MAXK";
		//public String typeOfCoverage="VISITOR_DIVERSITY_TC";
		//public String typeOfCoverage="VISITOR_DIVERSITY_TL";
		//public String typeOfCoverage="VISITOR_DIVERSITY_EDM";
		//public String typeOfCoverage="VISITOR_DIVERSITY_EDA";
		//public String typeOfCoverage="VISITOR_DIVERSITY_LastEventPair_TC";
		//public String typeOfCoverage="VISITOR_DIVERSITY_LastEventPair_TL";
		//public String typeOfCoverage="VISITOR_DIVERSITY_LastEventPair_EDA";
		//public String typeOfCoverage="VISITOR_DIVERSITY_LastEventPair_EDM";
		typeOfCoverage="VISITOR_DIVERSITY_LastEventPair_TC";
		//
		//folderOriginalFSM="input"+System.getProperty("file.separator")+"NotePadTest"+System.getProperty("file.separator")+"model"+System.getProperty("file.separator")+"NotePad.Ale.fsm";
		folderOriginalFSM="input"+System.getProperty("file.separator")+"NexesManager"+System.getProperty("file.separator")+"model"+System.getProperty("file.separator")+"NexesManager.fsm";
		folderTraces="input"+System.getProperty("file.separator")+"logs";
		executionFromProxy=true;
	}
	
	private void defaultSetUp_Params(){

		maxLengthGeneratedTraces=100;
		k=2;
		typeOfVisit=TypeOfVisit.givenLengthK.toString();
		fileNameOfAbstractTC="abstractTC";
		
		N_max=1000;
		t_initial=5;
		SuiteIt_max=1000;
		TcIt_max=1000;
		
		if (!executionFromProxy){
			setParameters_fromProxy_default();
			String tmp= typeOfCoverage;
			if (!convert(tmp)) {
				if (TypeOfCoverage.valueOf(tmp)!=null){
				//System.out.println("");
				}
				else {
					typeOfCoverage=TypeOfCoverage.coverage_uniform_fromFSM.toString();	
				}
			}
		}
		
		//diversity
		 if (( typeOfCoverage.startsWith("VISITOR_DIVERSITY_LastEventPair_") )||( typeOfCoverage.startsWith("diversity_onlyLastEvent_") )){
			   if ( typeOfCoverage.endsWith("TC")){
				   typeOfFitness=TypeOfFitnessFunction.TC_absolute.toString();
			   } else  if ( typeOfCoverage.endsWith("TL")){
				   typeOfFitness=TypeOfFitnessFunction.TL.toString();
			   } else  if ( typeOfCoverage.endsWith("EDM")){
				   typeOfFitness=TypeOfFitnessFunction.EDM.toString();
			   } else  if ( typeOfCoverage.endsWith("EDA")){
				   typeOfFitness=TypeOfFitnessFunction.EDA.toString();
			   }
			   typeOfCoverage="diversity_onlyLastEvent_";
		   }else if ( (typeOfCoverage.startsWith("VISITOR_DIVERSITY_"))||( typeOfCoverage.startsWith("diversity") )){
			   if ( typeOfCoverage.endsWith("TC")){
				   typeOfFitness=TypeOfFitnessFunction.TC_absolute.toString();
			   } else  if ( typeOfCoverage.endsWith("TL")){
				   typeOfFitness=TypeOfFitnessFunction.TL.toString();
			   } else  if ( typeOfCoverage.endsWith("EDM")){
				   typeOfFitness=TypeOfFitnessFunction.EDM.toString();
			   } else  if ( typeOfCoverage.endsWith("EDA")){
				   typeOfFitness=TypeOfFitnessFunction.EDA.toString();
			   }
			   
			   typeOfCoverage="diversity"; 
		}
		
		
			 if (( folderOriginalFSM.lastIndexOf("/")>-1)&&( folderOriginalFSM.length()>1)) {	
				 folderNewEventSequences= folderOriginalFSM.trim().substring( folderOriginalFSM.trim().lastIndexOf("/")+1)+System.getProperty("file.separator")+typeOfCoverage.trim();
			 }
			 else  folderNewEventSequences="output"+System.getProperty("file.separator")+ typeOfCoverage;

		 
		setupExecuted=true;
	}
	
	
	
	//main
	public static void main(String[] args)  {
		MainToImport eventseqgen=new MainToImport();
		
		String usage = "usage: java -jar EventSequenceGenerator inputFolder inputFolderTracess outFolder abstractTCName criterion k typeOfVisit typeOfFitness N_max t_initial SuiteIt_max TcIt_max\n" 
			 + "   where: \n"			
	    	 + "   inputFolder \t: path to the folder of the actual fsm (e.g., default: 'input/tudu_actual/fsm.fsm')\n"
	    	 + "   inputFolderTraces \t: path to the folder of the traces (e.g., default: 'input/logs')\n"
	        + "    outFolder \t: path to the folder of the generated traces (e.g., default: 'generatedPaths'); \n"
	        + "    abstractTCName \t: (prefix)name for the generated abstrac test cases (e.g., default: 'abstractTC'); \n"
	        + "    criterion \t: the criterion used to extract event sequences (e.g., default: 'VISITOR_COVERAGE_UNIFORM' - VISITOR_BREADTHFIRST,VISITOR_BREADTHFIRST_WITH_GLOBAL_LOOPS,VISITOR_BREADTHFIRST_WITH_LOCAL_LOOPS, VISITOR_BREADTHFIRST_2,VISITOR_BREADTHFIRST_2,VISITOR_COVERAGEWITHFRQ_LOGS, VISITOR_xLeastFrequentEven_LOGS, VISITOR_COVERAGE_UNIFORM_LOGS, VISITOR_COVERAGE_UNIFORM,VISITOR_SEQ_MAXK,VISITOR_SEQK,VISITOR_SEMK,VISITOR_SEM_MAXK,VISITOR_SEM_LastEventPair_MAXK, VISITOR_SEM_LastEventPair_K,VISITOR_ALT_MAXK,VISITOR_DIVERSITY_TC,VISITOR_DIVERSITY_TL,VISITOR_DIVERSITY_EDM,VISITOR_DIVERSITY_EDA,VISITOR_DIVERSITY_LastEventPair_TC, VISITOR_DIVERSITY_LastEventPair_TL,VISITOR_DIVERSITY_LastEventPair_EDA,VISITOR_DIVERSITY_LastEventPair_EDM); \n"
	        + "    k \t: the k parameter - only for criterion={all_sequences_maxK,all_sequences_K,sem_maxK,sem_K,alt_maxK,alt_K} (e.g., default: '2'); \n"
	        + "    typeOfVisit \t: the typeOfVisit for paths/sequences extraction - only for criterion={all_sequences_maxK,all_sequences_K,sem_maxK,sem_K,alt_maxK,alt_K} (e.g., default: 'BreadthFirstVisit' - BreadthFirstVisit,BreadthFirstVisitWithLoop,BreadthFirstVisitWithGlobalLoop,givenLengthK); \n"
	        + "    typeOfFitness \t: the typeOfFitness for paths extraction - only for criterion={diversity} (e.g., default: 'TC_absolute' - TC_absolute, TL, EDM, EDA); \n"
	        + "    N_max \t: the max paths_suite size - only for criterion={diversity} (e.g., default: '100' ); \n"
	        + "    t_initial \t: initial temperature value - only for criterion={diversity} (e.g., default: '5' ); \n"
	        + "    SuiteIt_max \t: number of paths considered in the evolution - only for criterion={diversity} (e.g., default: min('500',2*size_suite) ); \n"
	        + "    TcIt_max \t: number of iterations per path - only for criterion={diversity} (e.g., default: '10' ); \n"
	        + "    \n"
	        + "     - Alternatively, to run with the XML-based configuration use: java -jar EventSequenceGenerator runconfig [files.xml]\n"
	        + "    (Main class: eu.fittest.eventSequenceGenerator.Main) \n";
			
		String tried = "... trying with default parameter values \n";
		eventseqgen.setupExecuted=false;
		eventseqgen.executionFromProxy=false;
	
		if (args.length==12){								
			tried = "... running with user's parameter values \n";	
			eventseqgen.get_InputParams(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11]);
			System.out.println(tried+"\n");
		}else if (args.length==3){
				eventseqgen.defaultSetUp_Params();
				eventseqgen.get_InputParams(args[0], args[1], args[2]);
				tried = "... running with user's parameter values for input and default for others \n";
				System.out.println(usage+"\n"+tried+"\n");
		}
		else if ((args.length==2) && (args[0].equalsIgnoreCase("runconfig")) ){
			eventseqgen.defaultSetUp_Params();
			if (eventseqgen.loadConfigFromXml(args[1])) {
				tried = "... running with user's parameter values loaded from XML \n";
				System.out.println(usage+"\n"+tried+"\n");
			} else {
				eventseqgen.defaultSetUp_Params();
				tried = "... running with default parameter values after tried with XML\n";
				System.out.println(usage+"\n"+tried+"\n");
			}
		}else if ((args.length==1) && (args[0].equalsIgnoreCase("runconfig")) ){
			eventseqgen.defaultSetUp_Params();
			if (eventseqgen.loadConfigFromXml(CONFIG_XML)) {
				tried = "... running with user's parameter values loaded from (default) XML \n";
				System.out.println(usage+"\n"+tried+"\n");
			} else {
				eventseqgen.defaultSetUp_Params();
				tried = "... running with default parameter values after tried (default) XML\n";
				System.out.println(usage+"\n"+tried+"\n");
			}
		}else {
			eventseqgen.defaultSetUp_Params();
			tried = "... running with default parameter values \n";
			System.out.println(usage+"\n"+tried+"\n");
		}
				
		if (eventseqgen.run()==null){
			System.out.println("Wrong parameters, please, check if the inputs are correct");
		}
	}
	
	private boolean convert(String type){
		boolean converted=false;
		
		String visitorId = VisitorFactory.VISITORS.get(type);
		if (visitorId!=null){
			this.typeOfCoverage=visitorId;
			converted=true;
		}
		else {
			// gia' vero
			
		}
		return converted;
	}
	
	//input/output only parameter checks
	private void get_InputParams(String folderOriginalFSM,String folderTraces, String typeOfCoverage){
			
		   try{
			   if (!folderOriginalFSM.equals("")) this.folderOriginalFSM=folderOriginalFSM;
			   if (!folderTraces.equals("")) this.folderTraces=folderTraces;
		
			   if (typeOfCoverage.equals("")) {
				   this.typeOfCoverage=TypeOfCoverage.coverage_uniform_fromFSM.toString();
			   }
			   else {
				    if (!convert(typeOfCoverage)) {
						   if (TypeOfCoverage.valueOf(typeOfCoverage)!=null){
								//System.out.println("");
							}
							else {
								this.typeOfCoverage=TypeOfCoverage.coverage_uniform_fromFSM.toString();	
							}
					   }				    
			  }
			   
			   if ((typeOfCoverage.startsWith("VISITOR_DIVERSITY_LastEventPair_"))||( typeOfCoverage.startsWith("diversity_onlyLastEvent_") )){
				   if (typeOfCoverage.endsWith("TC")){
					   this.typeOfFitness=TypeOfFitnessFunction.TC_absolute.toString();
				   } else  if (typeOfCoverage.endsWith("TL")){
					   this.typeOfFitness=TypeOfFitnessFunction.TL.toString();
				   } else  if (typeOfCoverage.endsWith("EDM")){
					   this.typeOfFitness=TypeOfFitnessFunction.EDM.toString();
				   } else  if (typeOfCoverage.endsWith("EDA")){
					   this.typeOfFitness=TypeOfFitnessFunction.EDA.toString();
				   }
				   typeOfCoverage="diversity_onlyLastEvent_";
			   }else if ((typeOfCoverage.startsWith("VISITOR_DIVERSITY_"))||( typeOfCoverage.startsWith("diversity") )){
				   if (typeOfCoverage.endsWith("TC")){
					   this.typeOfFitness=TypeOfFitnessFunction.TC_absolute.toString();
				   } else  if (typeOfCoverage.endsWith("TL")){
					   this.typeOfFitness=TypeOfFitnessFunction.TL.toString();
				   } else  if (typeOfCoverage.endsWith("EDM")){
					   this.typeOfFitness=TypeOfFitnessFunction.EDM.toString();
				   } else  if (typeOfCoverage.endsWith("EDA")){
					   this.typeOfFitness=TypeOfFitnessFunction.EDA.toString();
				   }
				   typeOfCoverage="diversity"; //diversity
			   }
			   	   
			   setupExecuted=true; 
		   }catch(Exception e){
			   System.out.println("Wrong parameter values");
			   setupExecuted=false;
		   }
		}
	
    //all parameter checks
	private void get_InputParams(String folderOriginalFSM,String folderTraces, String folderNewEventSequences, String fileNameOfAbstractTC, String typeOfCoverage,String k,String typeOfVisit,String typeOfFitness,String N_max, String t_initial,String SuiteIt_max, String TcIt_max){
		
	   try{
		   if (!folderOriginalFSM.equals("")) this.folderOriginalFSM=folderOriginalFSM;
		   if (!folderTraces.equals("")) this.folderTraces=folderTraces;
		   if (!folderNewEventSequences.equals("")) this.folderNewEventSequences=folderNewEventSequences;
	
		   if (typeOfCoverage.equals("")) {
			   if (!folderOriginalFSM.equals("")) this.typeOfCoverage=TypeOfCoverage.coverage_uniform_fromFSM.toString();
		   }
		   else {
			  if (!convert(typeOfCoverage)) {
					   if (TypeOfCoverage.valueOf(typeOfCoverage)!=null){
							//System.out.println("");
						}
						else {
							this.typeOfCoverage=TypeOfCoverage.coverage_uniform_fromFSM.toString();	
						}
				   }

		  }
		   
		   if (!k.equals("")) {
			   this.k=new Integer(k).intValue();
			   if (this.k<2) this.k=2;
		   }	
		   if (!typeOfVisit.equals("")) this.typeOfVisit=typeOfVisit;
		   if (!fileNameOfAbstractTC.equals("")) this.fileNameOfAbstractTC=fileNameOfAbstractTC;
		   		
		   if ((typeOfCoverage.startsWith("VISITOR_DIVERSITY_LastEventPair_"))||( typeOfCoverage.startsWith("diversity_onlyLastEvent_") )){
			   if (typeOfCoverage.endsWith("TC")){
				   this.typeOfFitness=TypeOfFitnessFunction.TC_absolute.toString();
			   } else  if (typeOfCoverage.endsWith("TL")){
				   this.typeOfFitness=TypeOfFitnessFunction.TL.toString();
			   } else  if (typeOfCoverage.endsWith("EDM")){
				   this.typeOfFitness=TypeOfFitnessFunction.EDM.toString();
			   } else  if (typeOfCoverage.endsWith("EDA")){
				   this.typeOfFitness=TypeOfFitnessFunction.EDA.toString();
			   }
			   typeOfCoverage="diversity_onlyLastEvent_";
		   }else if ((typeOfCoverage.startsWith("VISITOR_DIVERSITY_"))||( typeOfCoverage.startsWith("diversity") )){
			   if (typeOfCoverage.endsWith("TC")){
				   this.typeOfFitness=TypeOfFitnessFunction.TC_absolute.toString();
			   } else  if (typeOfCoverage.endsWith("TL")){
				   this.typeOfFitness=TypeOfFitnessFunction.TL.toString();
			   } else  if (typeOfCoverage.endsWith("EDM")){
				   this.typeOfFitness=TypeOfFitnessFunction.EDM.toString();
			   } else  if (typeOfCoverage.endsWith("EDA")){
				   this.typeOfFitness=TypeOfFitnessFunction.EDA.toString();
			   }
			   typeOfCoverage="diversity"; //diversity
		   }
		   	   
		   
		   if (!N_max.equals("")) this.N_max=new Integer(N_max).intValue();
		   if (!t_initial.equals("")) this.t_initial=new Double(t_initial).doubleValue();
		   if (!SuiteIt_max.equals("")) this.SuiteIt_max=new Integer(SuiteIt_max).intValue();
		   if (!TcIt_max.equals("")) this.TcIt_max=new Integer(TcIt_max).intValue();
		
		   setupExecuted=true;
		   
	   }catch(Exception e){
		   System.out.println("Wrong parameter values");
		   setupExecuted=false;
	   }
	}
	
	
	public Vector<eu.fittest.eventSequenceGenerator.data.Path> run(){
		try{
			
			paths=new Vector<eu.fittest.eventSequenceGenerator.data.Path>();
					
			if (!setupExecuted) {
				defaultSetUp_Params();
				if (!loadConfigFromXml(CONFIG_XML)) defaultSetUp_Params();
			}
			
			if (setupExecuted) {
				eventSeqGen.setUp(folderOriginalFSM, folderTraces, folderNewEventSequences, fileNameOfAbstractTC, typeOfCoverage,k,typeOfVisit,N_max, t_initial, SuiteIt_max, TcIt_max, typeOfFitness);
				paths=eventSeqGen.run();	
			}
			
			return paths;
		}catch(Exception e){
			e.printStackTrace();
			System.out.println("Please, check if the inputs are correct");
			return null;
		}
	}
	
	private boolean loadConfigFromXml(String configFileName) {
		try{
		JAXBContext context = JAXBContext.newInstance(Xml.class);

		Unmarshaller um = context.createUnmarshaller();
		File f=new File(configFileName);
		Xml xmlConfig=null;
		if (f.exists()) xmlConfig = (Xml) um.unmarshal(new FileReader(f));
		else xmlConfig = (Xml) um.unmarshal(new FileReader(CONFIG_XML));
		
		   if (!xmlConfig.getFolderOriginalFSM().equals("")) this.folderOriginalFSM=xmlConfig.getFolderOriginalFSM();
		 		
		   if (!xmlConfig.getTypeOfCoverage().equals("")) {
			   convert(xmlConfig.getTypeOfCoverage());
		   }
		   
		  /* if (xmlConfig.getTypeOfCoverage().equals("")) {
			  // if (!this.folderOriginalFSM.equals("")) this.typeOfCoverage=TypeOfCoverage.coverage_uniform_fromFSM.toString();
		   }
		   else {
			    if (!convert(xmlConfig.getTypeOfCoverage())) {
					 /*  if (TypeOfCoverage.valueOf(xmlConfig.getTypeOfCoverage())!=null){
							//System.out.println("");
						}
						else {
							if (!folderOriginalFSM.equals("")) {
								this.typeOfCoverage=TypeOfCoverage.coverage_uniform_fromFSM.toString();	
								
							}
						}
				   }
		   }*/
		   
		   if (!xmlConfig.getFolderNewEventSequences().equals("")) this.folderNewEventSequences=xmlConfig.getFolderNewEventSequences();
		   //else this.folderNewEventSequences="output"+System.getProperty("file.separator")+this.typeOfCoverage;
		   else {
			   if ((this.folderOriginalFSM.lastIndexOf("/")>-1)&&(this.folderOriginalFSM.length()>1)) {	
				   this.folderNewEventSequences=this.folderOriginalFSM.trim().substring(this.folderOriginalFSM.trim().lastIndexOf("/")+1)+System.getProperty("file.separator")+this.typeOfCoverage.trim();
			   }
			else this.folderNewEventSequences="output"+System.getProperty("file.separator")+this.typeOfCoverage;
		   }
		   
		   if (xmlConfig.getK()>2)  this.k=xmlConfig.getK();

		   if (!xmlConfig.getTypeOfVisit().equals("")) this.typeOfVisit=xmlConfig.getTypeOfVisit();
		   if (!xmlConfig.getFileNameOfAbstractTC().equals("")) this.fileNameOfAbstractTC=xmlConfig.getFileNameOfAbstractTC();
			   
		   if ((xmlConfig.getTypeOfCoverage().startsWith("VISITOR_DIVERSITY_LastEventPair_"))||( typeOfCoverage.startsWith("diversity_onlyLastEvent_") )){
			   if (typeOfCoverage.endsWith("TC")){
				   this.typeOfFitness=TypeOfFitnessFunction.TC_absolute.toString();
			   } else  if (typeOfCoverage.endsWith("TL")){
				   this.typeOfFitness=TypeOfFitnessFunction.TL.toString();
			   } else  if (typeOfCoverage.endsWith("EDM")){
				   this.typeOfFitness=TypeOfFitnessFunction.EDM.toString();
			   } else  if (typeOfCoverage.endsWith("EDA")){
				   this.typeOfFitness=TypeOfFitnessFunction.EDA.toString();
			   }
			   this.typeOfCoverage="diversity_onlyLastEvent_";
		   }else if ((xmlConfig.getTypeOfCoverage().startsWith("VISITOR_DIVERSITY_"))||( typeOfCoverage.startsWith("diversity") )){
			   if (typeOfCoverage.endsWith("TC")){
				   this.typeOfFitness=TypeOfFitnessFunction.TC_absolute.toString();
			   } else  if (typeOfCoverage.endsWith("TL")){
				   TypeOfFitnessFunction.TL.toString();
			   } else  if (typeOfCoverage.endsWith("EDM")){
				   TypeOfFitnessFunction.EDM.toString();
			   } else  if (typeOfCoverage.endsWith("EDA")){
				   TypeOfFitnessFunction.EDA.toString();
			   }
			   this.typeOfCoverage="diversity";
		   }
		   
		   if (xmlConfig.getN_max()>1) this.N_max=xmlConfig.getN_max();
		   if (xmlConfig.getT_initial()>0) this.t_initial=xmlConfig.getT_initial();
		   if (xmlConfig.getSuiteIt_max()>1) this.SuiteIt_max=xmlConfig.getSuiteIt_max();
		   if (xmlConfig.getTcIt_max()>1) this.TcIt_max=xmlConfig.getTcIt_max();

		setupExecuted=true;   
		return true;
		}catch(Exception e){
			//e.printStackTrace();
			setupExecuted=false;  
			return false;
		}

	}
	

	
}
