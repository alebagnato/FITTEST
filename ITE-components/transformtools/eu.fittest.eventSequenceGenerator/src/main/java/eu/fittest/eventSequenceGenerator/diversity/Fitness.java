/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fittest.eventSequenceGenerator.diversity;

import eu.fittest.eventSequenceGenerator.data.*;


import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.Vector;
import java.util.Enumeration; 
import java.util.Hashtable;

/**
 *
 * @author Alessandro
 *
 */
public class Fitness {
	
	
	public double fitnessComputation(String typeOfFitness, FSM fsm, Vector<Path> suite){
		
		try{
		
		if (typeOfFitness.equals(TypeOfFitnessFunction.TC_absolute.toString())) {
			return coverageMeasure_absolute(fsm,suite);
		}else if (typeOfFitness.equals(TypeOfFitnessFunction.TL.toString())) {
			return pathsLenghtMeasure(fsm,suite);
/*		}else if (typeOfFitness.equals(TypeOfFitnessFunction.PDM)) {
			
		}else if (typeOfFitness.equals(TypeOfFitnessFunction.PDA)) {
*/		
		}else if (typeOfFitness.equals(TypeOfFitnessFunction.EDM.toString())) {
			return eventDiversityMeasure_min(fsm,suite);
		}else if (typeOfFitness.equals(TypeOfFitnessFunction.EDA.toString())) {
			return eventDiversityMeasure_average(fsm,suite);
		}else if (typeOfFitness.equals(TypeOfFitnessFunction.EDATL.toString())) {
			return edatl(fsm,suite);
		}else if (typeOfFitness.equals(TypeOfFitnessFunction.EDATC.toString())) {
			return edatc(fsm,suite);
		}
		return -1;
		}catch(Exception e){
			e.printStackTrace();
			return -1;
		}
	}
	
// COVERAGE
	
	/**
	 * Compute coverage-based fitness
	 * @param fsm
	 * @param suite
	 * @return
	 */
	double coverageMeasure_absolute(FSM fsm, Vector<Path> suite){
		Edge edge;
		Set<Edge> edgesCovered = new HashSet<Edge>();
		for (Path path : suite) {
			for (int index = 0; index < path.getEdges().size(); index++) {
				edge=path.getEdges().get(index);
				edgesCovered.add(edge);
			}
		}
		return (new Double(edgesCovered.size())).doubleValue();
	}
	
// LENGTH
	
	/**
	 * Compute test-case length based fitness
	 * @param fsm
	 * @param suite
	 * @return
	 */
	double pathsLenghtMeasure(FSM fsm, Vector<Path> suite){
		double avgLength=computeAvgLenght(suite);
		double pathLength=0;
		double sum=0;
		double tl=-1;
		for (Path path : suite) {
			pathLength=(new Double(path.getEdges().size()).doubleValue());
			sum=sum+((pathLength-avgLength)*(pathLength-avgLength));
		}
		if (sum>0) {
			tl=Math.sqrt( sum/(suite.size()) );
			return tl;
		}
		return -1;
	}
	
	double computeAvgLenght(Vector<Path> suite){
		double sum=0;
		for (Path path : suite) {
			sum=sum+(new Double(path.getEdges().size()).doubleValue());
		}
		if (sum>0){
			return ( sum/ (new Double(suite.size())).doubleValue() );
		}
		return -1;	
	}
	
//EVENT FREQUENCY  - MIN
	
	/**
	 * Compute event diversity average fitness
	 * @param fsm
	 * @param suite
	 * @return
	 */
	double eventDiversityMeasure_min(FSM fsm, Vector<Path> suite){
		Hashtable<String,Double> edgesFreqs_suite=computeEventExecutionFrequency(suite);
		
		double size=new Double(edgesFreqs_suite.size()).doubleValue();
		double sum=0;
		double sum_path=0;
		double sum_suite_min=-1;
		double sum_path_min=-1;
		Enumeration<String> edgeLabels;
		
		String edgeLabel_e;
		double freq_e;
		Hashtable<String,Double> edgesFreqs_path;
		
		for (Path path : suite) {
			edgesFreqs_path=computeEventExecutionFrequency(path);
			edgeLabels = edgesFreqs_path.keys();
			sum =0;
			sum_path=0;
			
			while(edgeLabels.hasMoreElements()) {
				edgeLabel_e = (String) edgeLabels.nextElement();
				freq_e=edgesFreqs_path.get(edgeLabel_e);
			
				for (int i = 0; i < suite.size(); i++) {
					sum=sum+( (freq_e - getEventFreq_InPath(suite,path,edgeLabel_e,i))*(freq_e - getEventFreq_InPath(suite,path,edgeLabel_e,i)) );
				}
			
			sum_path= Math.sqrt(sum/size);
			if (sum_path_min==-1) sum_path_min=sum_path;
			else if (sum_path<sum_path_min) sum_path_min=sum_path;
			}
		sum_suite_min=sum_suite_min+sum_path_min;
		}
		
		return sum_suite_min;
	}
	
	double getEventFreq_InPath(Vector<Path> suite, Path currentPath, String edgeLabel_e, int index){
		Path path2;
		Hashtable<String,Double> edgesFreqs_path2;
		
		if (index<=suite.size()){
			path2=suite.get(index);
		
			if (!path2.equals(currentPath)){
			
				edgesFreqs_path2=computeEventExecutionFrequency(path2);

				if (edgesFreqs_path2.get(edgeLabel_e)!=null){
					return edgesFreqs_path2.get(edgeLabel_e).doubleValue();
				}
			} 
		}
		return -1;
	}
	
	
// EVENT FREQUENCY  - AVERAGE
		
	/**
	 * Compute event diversity average fitness
	 * @param fsm
	 * @param suite
	 * @return
	 */
	double eventDiversityMeasure_average(FSM fsm, Vector<Path> suite){
		Hashtable<String,Double> edgesFreqs_suite=computeEventExecutionFrequency(suite);
		Hashtable<String,Double> edgesFreqs_suite_avg;
			
		Hashtable<String,Double> edgesFreqs_path;
		
		Enumeration<String> edgeLabels;
		String edgeLabel_e;
		double freq_e;
		double sum=0;
		
		edgeLabels = edgesFreqs_suite.keys();
		while(edgeLabels.hasMoreElements()) {
			edgeLabel_e = (String) edgeLabels.nextElement();
			freq_e=edgesFreqs_suite.get(edgeLabel_e);
			sum=sum+freq_e;
		} 
		edgesFreqs_suite_avg=new Hashtable<String,Double>();
		edgeLabels = edgesFreqs_suite.keys();
		while(edgeLabels.hasMoreElements()) {
			edgeLabel_e = (String) edgeLabels.nextElement();
			freq_e=edgesFreqs_suite.get(edgeLabel_e);
			edgesFreqs_suite_avg.put(edgeLabel_e, freq_e/sum);
		} 
		
		double sum_suite=-1;
		for (Path path : suite) {
			edgesFreqs_path=computeEventExecutionFrequency(path);
			edgeLabels = edgesFreqs_path.keys();
			sum=0;
			while(edgeLabels.hasMoreElements()) {
				edgeLabel_e = (String) edgeLabels.nextElement();
				freq_e=edgesFreqs_path.get(edgeLabel_e);
				sum=sum+( (freq_e - edgesFreqs_suite_avg.get(edgeLabel_e))*(freq_e - edgesFreqs_suite_avg.get(edgeLabel_e)) );
			} 
			sum_suite=sum_suite+Math.sqrt(sum/path.getEdges().size());
		}
		
		return sum_suite;
	}
	
	Hashtable<String,Double> computeEventExecutionFrequency(Path path){
		Hashtable<String,Double> edgesCovered_Freq = new Hashtable<String,Double>();
		double execFreq=0;
		Edge edge;
		
		for (int index = 0; index < path.getEdges().size(); index++) {
				edge=path.getEdges().get(index);
				if (edgesCovered_Freq.containsKey(edge.getEvent())) {
					execFreq=edgesCovered_Freq.get(edge.getEvent()).doubleValue();
					execFreq=execFreq+1.0;
					edgesCovered_Freq.put(edge.getEvent(), new Double(execFreq));
				}else {
					edgesCovered_Freq.put(edge.getEvent(), new Double(1));
				}
			}
		return edgesCovered_Freq;
	 }
	
	Hashtable<String,Double> computeEventExecutionFrequency(Vector<Path> suite){
		
		Edge edge;
		Hashtable<String,Double> edgesCovered_Freq = new Hashtable<String,Double>();	
		double execFreq=0;
		
		for (Path path : suite) {
			for (int index = 0; index < path.getEdges().size(); index++) {
				edge=path.getEdges().get(index);
				if (edgesCovered_Freq.containsKey(edge.getEvent())) {
					execFreq=edgesCovered_Freq.get(edge.getEvent()).doubleValue();
					execFreq=execFreq+1.0;
					edgesCovered_Freq.put(edge.getEvent(), new Double(execFreq));
				}else {
					edgesCovered_Freq.put(edge.getEvent(), new Double(1));
				}
				
			}
		}
		
		
		return edgesCovered_Freq;	
	}
	
	
	// EDA-TL
	/**
	 * MuiltiObjective: event diversity average fitness + test case length
	 * @param fsm
	 * @param suite
	 * @return
	 */
	double edatl(FSM fsm, Vector<Path> suite){
		double eda=eventDiversityMeasure_average(fsm,suite);
		double tl=pathsLenghtMeasure(fsm, suite);
		double maxLength=0;
		double sum=0;
		for (Path path : suite) {
			sum=(new Double(path.getEdges().size()).doubleValue());
			if (sum>maxLength) maxLength=sum;
		}
		double uppercaseEDA=suite.size();
		double uppercaseTL=maxLength;
		double fit1=eda/uppercaseEDA;
		double fit2=tl/uppercaseTL;
		double fit=(fit1+fit2)/2;
		return fit;
	}
	
	// EDA-TC
	/**
	 * MuiltiObjective: event diversity average fitness + fsm coverage
	 * @param fsm
	 * @param suite
	 * @return
	 */
	double edatc(FSM fsm, Vector<Path> suite){
		double eda=eventDiversityMeasure_average(fsm,suite);
		double tc=coverageMeasure_absolute(fsm, suite);
		double uppercaseEDA=suite.size();
		double uppercaseTC=0;
		
		Collection<Node> nodes=fsm.getNodes();
		for (Iterator iterator = nodes.iterator(); iterator.hasNext();) {
			Node node = (Node) iterator.next();
			uppercaseTC=uppercaseTC+node.getSucc().size();
		}
		
		double fit1=eda/uppercaseEDA;
		double fit2=tc/uppercaseTC;
		double fit=(fit1+fit2)/2;
		return fit;
	}
	
	
	
}
