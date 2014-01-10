package eu.fittest.modelInference.imu.config;

import java.io.File;

import java.util.Vector;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class LoadConfiguration {
	Config_Launcher cl=Config_Launcher.getInstace();
	
	
	public boolean loadConfiguration(){
		 try {
			 
		 
		File fXmlFile=new File(cl.configurationFileName);
		if (!fXmlFile.isFile()) {
			return false;
		}
		
		DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
		DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
		Document doc = dBuilder.parse(fXmlFile);
		doc.getDocumentElement().normalize();
		
		
		NodeList bodyChilds = doc.getChildNodes().item(0).getChildNodes();

		for (int temp = 0; temp < bodyChilds.getLength(); temp++) {
	    	
	    	Node node_Params = bodyChilds.item(temp);	
	    	
	    	if (node_Params.getNodeName().equalsIgnoreCase("parameter")){
	    		if (node_Params.getNodeType() == Node.ELEMENT_NODE) {
	    			Element param = (Element) node_Params;
	    			
	    			if (param.getAttribute("name").equals("t1_PercOfExpectedLogs")){
	    				cl.t1_PercOfExpectedLogs=(new Double(param.getAttribute("value"))).doubleValue();
	    				if (cl.t1_PercOfExpectedLogs>1) return false;
	    				
	    			} else if (param.getAttribute("name").equals("t2_PercOfExpectedLogs")){
	    				cl.t2_PercOfExpectedLogs=(new Double(param.getAttribute("value"))).doubleValue();
	    				if (cl.t2_PercOfExpectedLogs>1) return false;
	    				
	    			} else if (param.getAttribute("name").equals("ttest_PercOfExpectedLogs")){
	    				cl.ttest_PercOfExpectedLogs=(new Double(param.getAttribute("value"))).doubleValue();
	    				if (cl.ttest_PercOfExpectedLogs>1) return false;
	    				if ((cl.t1_PercOfExpectedLogs+cl.t2_PercOfExpectedLogs+cl.ttest_PercOfExpectedLogs)>1) return false;
	    				
	    			} else if (param.getAttribute("name").equals("probabilitiesCoefficientArray")){
	    				String[] listOfD=(param.getAttribute("value")).split(",");
	    				//cl.probabilitiesCoefficientArray=new double[]{};
	    				cl.probabilitiesCoefficientArray=new double[listOfD.length];
	    				for (int i = 0; i < cl.probabilitiesCoefficientArray.length; i++) {
	    					cl.probabilitiesCoefficientArray[i]=(new Double(listOfD[i].trim())).doubleValue();
	    					if (cl.probabilitiesCoefficientArray[i]<1) return false;
						}
	    				
	    			} else if (param.getAttribute("name").equals("evaluationA_iterations_perEvent")){
	    				cl.evaluationA_iterations_perEvent=new Integer(param.getAttribute("value")).intValue();
	    				if (cl.evaluationA_iterations_perEvent<1) return false;
	    				
	    			} else if (param.getAttribute("name").equals("evaluationA_fsmcoverageTimes_PerEvent")){
	    				cl.evaluationA_fsmcoverageTimes_PerEvent=new Integer(param.getAttribute("value")).intValue();
	    				if (cl.evaluationA_fsmcoverageTimes_PerEvent<1) return false;
	    				
	    			} else if (param.getAttribute("name").equals("evaluationA_maxLengthOfGeneratedTests")){
	    				cl.evaluationA_maxLengthOfGeneratedTests=new Integer(param.getAttribute("value")).intValue();
	    				if (cl.evaluationA_maxLengthOfGeneratedTests<1) return false;
	    				
	    			} else if (param.getAttribute("name").equals("evaluationA_percent4xfixed")){
	    				cl.evaluationA_percent4xfixed=new Double(param.getAttribute("value")).doubleValue();
	    				if (cl.evaluationA_percent4xfixed<-1.0) return false;
	    				
	    			} else if (param.getAttribute("name").equals("evaluationA_distanceAtWhichSelectEvent")){
	    				cl.evaluationA_distanceAtWhichSelectEvent=new Integer(param.getAttribute("value")).intValue();
	    				if (cl.evaluationA_distanceAtWhichSelectEvent<1) return false;
	    				
	    			} else if (param.getAttribute("name").equals("alpha")){
	    				cl.alpha=new Double(param.getAttribute("value")).doubleValue();
	    				if (cl.alpha<0) return false;
	    				
	    			} else if (param.getAttribute("name").equals("X")){
	    				cl.X=new Integer(param.getAttribute("value")).intValue();
	    				if (cl.X<1) return false;
	    				
	    			} else if (param.getAttribute("name").equals("maxFilePemutations")){
	    				cl.maxFilePemutations=new Integer(param.getAttribute("value")).intValue();
	    				if (cl.maxFilePemutations<1) return false;
	    				
	    			} else if (param.getAttribute("name").equals("maxloop")){
	    				cl.maxloop=new Integer(param.getAttribute("value")).intValue();
	    				if (cl.maxloop<1) return false;
	    				
	    			} else if (param.getAttribute("name").equals("folderPath")){
	    				cl.folderPath=param.getAttribute("value");
	    				if (cl.folderPath==null) return false;
	    				else if (cl.folderPath.equalsIgnoreCase("")) return false;
	    				else {
	    					File tmp=new File(cl.folderPath);
	    					if (!tmp.isDirectory()) return false;
	    				}
	    				
	    			} else if (param.getAttribute("name").equals("folderPathRq3")){
	    				cl.folderPathRq3=param.getAttribute("value");
	    				if (cl.folderPathRq3==null) return false;
	    				else if (cl.folderPathRq3.equalsIgnoreCase("")) return false;
	    				else {
	    					File tmp=new File(cl.folderPathRq3);
	    					if (!tmp.isDirectory()) return false;
	    				}
	    				
	    			} else if (param.getAttribute("name").equals("folderPathAutomaticCalibration")){
	    				cl.folderPathAutomaticCalibration=param.getAttribute("value");
	    				if (cl.folderPathAutomaticCalibration==null) return false;
	    				else if (cl.folderPathAutomaticCalibration.equalsIgnoreCase("")) return false;
	    				/*else {
	    					File tmp=new File(cl.folderPathAutomaticCalibration);
	    					if (!tmp.isDirectory()) return false;
	    				}*/
	    				
	    			} else if (param.getAttribute("name").equals("folderPathAutomaticCalibration_subDir")){
	    				cl.folderPathAutomaticCalibration_subDir=param.getAttribute("value");
	    				if (cl.folderPathAutomaticCalibration_subDir==null) return false;
	    				else if (cl.folderPathAutomaticCalibration_subDir.equalsIgnoreCase("")) return false;
	    				/*else {
	    					File tmp=new File(cl.folderPathAutomaticCalibration_subDir);
	    					if (!tmp.isDirectory()) return false;
	    				}*/
	    				
	    			} else if (param.getAttribute("name").equals("folderPathAutomaticCalibration_testDir")){
	    				cl.folderPathAutomaticCalibration_testDir=param.getAttribute("value");
	    				if (cl.folderPathAutomaticCalibration_testDir==null) return false;
	    				else if (cl.folderPathAutomaticCalibration_testDir.equalsIgnoreCase("")) return false;
	    				/*else {
	    					File tmp=new File(cl.folderPathAutomaticCalibration_testDir);
	    					if (!tmp.isDirectory()) return false;
	    				}*/
	    				
	    			} else if (param.getAttribute("name").equals("folderPathXNewLogsCheck1")){
	    				cl.folderPathXNewLogsCheck1=param.getAttribute("value");
	    				if (cl.folderPathXNewLogsCheck1==null) return false;
	    				else if (cl.folderPathXNewLogsCheck1.equalsIgnoreCase("")) return false;
	    				/*else {
	    					File tmp=new File(cl.folderPathXNewLogsCheck1);
	    					if (!tmp.isDirectory()) return false;
	    				}*/
	    				
	    			} else if (param.getAttribute("name").equals("folderPathXNewLogsCheck2")){
	    				cl.folderPathXNewLogsCheck2=param.getAttribute("value");
	    				if (cl.folderPathXNewLogsCheck2==null) return false;
	    				else if (cl.folderPathXNewLogsCheck2.equalsIgnoreCase("")) return false;
	    				/*else {
	    					File tmp=new File(cl.folderPathXNewLogsCheck2);
	    					if (!tmp.isDirectory()) return false;
	    				}*/
	    				
	    			} else if (param.getAttribute("name").equals("folderPathXNewLogsCheck3")){
	    				cl.folderPathXNewLogsCheck3=param.getAttribute("value");
	    				if (cl.folderPathXNewLogsCheck3==null) return false;
	    				else if (cl.folderPathXNewLogsCheck3.equalsIgnoreCase("")) return false;
	    				/*else {
	    					File tmp=new File(cl.folderPathXNewLogsCheck3);
	    					if (!tmp.isDirectory()) return false;
	    				}*/
	    				
	    			}else if (param.getAttribute("name").equals("fsm2dotFileName_prefix")){
	    				cl.fsm2dotFileName_prefix=param.getAttribute("value");
	    				if (cl.fsm2dotFileName_prefix==null) return false;
	    				else if (cl.fsm2dotFileName_prefix.equalsIgnoreCase("")) return false;
	    				
	    			} else if (param.getAttribute("name").equals("eventsToCheck")){
	    				
	    				getEvent(param);
	    				
	    			} else if (param.getAttribute("name").equals("TforRQ3")){
	    				String[] listOfD=(param.getAttribute("value")).split(",");
	    				//cl.probabilitiesCoefficientArray=new double[]{};
	    				cl.TforRQ3=new int[listOfD.length];
	    				for (int i = 0; i < cl.TforRQ3.length; i++) {
	    					cl.TforRQ3[i]=(new Integer(listOfD[i].trim())).intValue();
	    					if (cl.TforRQ3[i]<1) return false;
						}
	    			} else if (param.getAttribute("name").equals("XforRQ3")){
    				String[] listOfD=(param.getAttribute("value")).split(",");
    				//cl.probabilitiesCoefficientArray=new double[]{};
    				cl.XforRQ3=new int[listOfD.length];
    				for (int i = 0; i < cl.XforRQ3.length; i++) {
    					cl.XforRQ3[i]=(new Integer(listOfD[i].trim())).intValue();
    					if (cl.XforRQ3[i]<1) return false;
					}
    			}	
	    				
	    				
	    		}
	    	}

		
		}
		
		return true;
		
		} catch (Exception e) {
			System.out.println("Configuration NOT loaded from XML ... the default one is used!");
			   e.printStackTrace();
			   return false;
		}		
	}
		 
		 
	private void getEvent(Element param){
		
		
		NodeList selectedEvent=param.getElementsByTagName("selectedEvent");
		
		if (selectedEvent==null) {
			// ...
		}
		else if (selectedEvent.getLength()==0){
			// ...
		}
		else {
			cl.eventsTocheck=new Vector<String[]>();
			
			String[] tmpSelectedEvent;
			NodeList sBeforeTag;
			Element eventProperty;
			NodeList eventTag;
			NodeList sAfterTag;
			
		  for (int i_sel = 0;  i_sel < selectedEvent.getLength();  i_sel++) {
			  tmpSelectedEvent=new String[3];
			  
			eventProperty = (Element) selectedEvent.item(i_sel);
			
			sBeforeTag=eventProperty.getElementsByTagName("stateBefore");
			if (sBeforeTag.item(0).getNodeType() == Node.ELEMENT_NODE) {
    			Element sBeforeTagElement = (Element) sBeforeTag.item(0);
    			tmpSelectedEvent[0]=sBeforeTagElement.getAttribute("value");
				}
			
			eventTag=eventProperty.getElementsByTagName("event");
			if (eventTag.item(0).getNodeType() == Node.ELEMENT_NODE) {
    			Element eventTagElement = (Element) eventTag.item(0);
    			tmpSelectedEvent[1]=eventTagElement.getAttribute("value");
    			//System.out.println(" tmpSelectedEvent[1]="+tmpSelectedEvent[1]);
				}
			
			sAfterTag=eventProperty.getElementsByTagName("stateAfter");
			if (sAfterTag.item(0).getNodeType() == Node.ELEMENT_NODE) {
    			Element sAfterTagElement = (Element) sAfterTag.item(0);
    			tmpSelectedEvent[2]=sAfterTagElement.getAttribute("value");
				}
			
			cl.eventsTocheck.add(tmpSelectedEvent.clone());
			
		  }
		}
		
		
		
	}
}
