package eu.fittest.eventSequenceGenerator.diversity;

import eu.fittest.eventSequenceGenerator.data.Edge;

import eu.fittest.eventSequenceGenerator.data.FSM;
import eu.fittest.eventSequenceGenerator.data.Node;
import eu.fittest.eventSequenceGenerator.data.Path;
import eu.fittest.eventSequenceGenerator.data.WritePaths;
import eu.fittest.eventSequenceGenerator.semanticInteraction.SEMchecker;
import eu.fittest.eventSequenceGenerator.semanticInteraction.SEMsextractor_onlyLastEvent;
import eu.fittest.eventSequenceGenerator.data.*;
import eu.fittest.eventSequenceGenerator.utility.PathsUtils;

import java.util.Vector;
import java.util.List;


/**
 * It implements
 * @author Alessandro
 *
 */
public class SA_onlyLastEvent {
	
	int N_max=100;
	FSM fsm=null;
	int k=2;
	double t_initial=5;
	int SuiteIt_max=500; //(2*sizeOf(suite))
	int TcIt_max=10;
	String originalFsmPath="";
	String outPathFolder=""; 
	String fileName="";
	
	SEMsextractor_onlyLastEvent semSeqs_onlyLastEvent=new SEMsextractor_onlyLastEvent();
	PathsUtils pathUtils=new PathsUtils();
	Fitness fitness=new Fitness();
	Vector<Path> suite_sem=null;
	WritePaths wp=new WritePaths();
	double alpha=0.95;
	int SuiteIt=0;
	int TcIt=0;
	double t=0;
	Path currentPath;
	String typeOfFitness=TypeOfFitnessFunction.TC_absolute.toString();
	
	void setupParams(int N_max,String originalFsmPath,String outPathFolder, String fileName,int k,double t_initial,int SuiteIt_max,int TcIt_max,String typeOfFitness){
		this.N_max=N_max;
		this.originalFsmPath=originalFsmPath;
		this.outPathFolder=outPathFolder;
		this.fileName=fileName;
		this.k=k;
		this.t_initial=t_initial;
		this.SuiteIt_max=SuiteIt_max;
		this.TcIt_max=TcIt_max;
		this.typeOfFitness=typeOfFitness;
		fsm = new FSM(originalFsmPath);
	}
	
	public  Vector<Path> run(int N_max,String originalFsmPath,String outPathFolder, String fileName,int k,double t_initial,int SuiteIt_max,int TcIt_max,String typeOfFitness){
		setupParams(N_max,originalFsmPath,outPathFolder, fileName, k,t_initial,SuiteIt_max,TcIt_max,typeOfFitness);
		
		initialzation();
		//System.out.println("1 - initialization done");
		if (SuiteIt_max>0){
			if (suite_sem.size()>0){ 
				evolution();
				wp.printPaths(suite_sem,outPathFolder,fileName);
			
			}
		} else wp.printPaths(suite_sem,outPathFolder,fileName);
		
		return suite_sem;
	}

	void initialzation(){
		//usare una stima?
		suite_sem=semSeqs_onlyLastEvent.get_allSequences_K(true, originalFsmPath, k, TypeOfVisit.givenLengthK.toString());
		 
		 //System.out.println("sizesuite="+suite_sem.size());
		 
		 Vector<Path> suite1_sem=null;
		 if (suite_sem.size()>N_max){
			 suite1_sem=pathUtils.randomSample(suite_sem,N_max);
			 suite_sem=new Vector<Path>();
			 Path p;
			 for (Path path : suite1_sem) {		 
				 p=new Path();
				 p.copy(path);
				 suite_sem.add(p);	
			 }
		 }
		 SuiteIt=0; 
		 //if (suite_sem.size()*2<SuiteIt_max) SuiteIt_max=suite_sem.size()*2;
		 
	}
	
	void evolution(){
		double prob=0;
		double rand=0;
		Edge n=null;
		int index=0;
		Path currentPath1;
		Vector<Path> suite1_sem=null;
		Edge m;
		Node s0;
		
		//int internalCount=0;
		
	do{
		TcIt=0;
		t=t_initial;
		
		index=pathUtils.getRandomIndex(suite_sem.size(),false);
				
		currentPath=suite_sem.get(index);
		m=currentPath.getEdges().get(currentPath.getEdges().size()-1);
		if (currentPath.getEdges().size()>2) s0=currentPath.getEdges().get(currentPath.getEdges().size()-3).getTarget();
		else s0=fsm.getStartNode();
		//System.out.println("SuiteIt = "+SuiteIt);
		
		System.out.println(" evolution number (SuiteIt) ="+SuiteIt+"  suite size="+suite_sem.size());
	
		//internalCount=0;
		//while (TcIt<TcIt_max){
		while (TcIt<TcIt_max){
			//internalCount++;
			
			//System.out.println("2 m="+m.getEvent()+" s0="+s0.getLabel());
			//System.out.println(" 	internal evolution ="+internalCount);
			n=getNextSemInteractEvents(fsm,m,s0);
						
			if (n!=null){ 
				//System.out.println("3");
				currentPath1=addOneEdge(currentPath,n);
				if (suite_sem.size()<N_max){
					//System.out.println("4");
					suite1_sem=addCurrentPath1(suite_sem,currentPath1);
				}else {
					//System.out.println("5");
					suite1_sem=replatePathWithCurrentPath1(suite_sem,currentPath,currentPath1);
				}
				double fit1=fitness(suite1_sem);
				double fit=fitness(suite_sem);
				
				if (fit1 > fit){
					suite_sem=pathUtils.copyPaths(suite1_sem);
				}else {
					prob = exp(t,fit1,fit);
					rand=pathUtils.getRandomDouble_0_1();
					if (rand<prob){
						suite_sem=pathUtils.copyPaths(suite1_sem);
					}
					//System.out.println("fit1="+fit1+" fit="+fit+" prob="+prob);
				}
				t=t*alpha;
				if (t<0.01) t=t_initial;
			}
			TcIt++;
				
			
		} //while
	SuiteIt++;
	} while (SuiteIt<SuiteIt_max);

		
	}
	
	double fitness(Vector<Path> paths){
		return fitness.fitnessComputation(typeOfFitness, fsm, paths);
		//return 0;
	}
	
	double exp(double t,double fit1,double fit){
		//System.out.println("t="+t+" fit1="+fit1+" fit="+fit);
		double exp=Math.exp((-(fit1-fit))/t);
		return exp;
	}
	
	//utils
	
	Vector<Path> addCurrentPath1(Vector<Path> currentSuite, Path path){
		 Vector<Path> newSuite=new Vector<Path>();
		 Path p;
		 for (Path cPath : currentSuite) {
			 p=new Path();
			 p.copy(cPath);
			 newSuite.add(p);
		}
		 if (!pathUtils.isContained(newSuite, path)) newSuite.add(path);
		 return newSuite;
	}
	
	Vector<Path> replatePathWithCurrentPath1(Vector<Path> currentSuite, Path oldPath,Path newPath){
		 Vector<Path> newSuite=new Vector<Path>();
		 Path p;
		 for (Path cPath : currentSuite) {
			 p=new Path();
			 p.copy(cPath);
			 if (p.equals(oldPath)) newSuite.add(newPath);
			 else newSuite.add(p);
		}
		 return newSuite;
	}
	
	Path addOneEdge(Path path,Edge e){
		Path p=new Path();
		p.copy(path);
		p.add(e);
		return p;
	}
	
	Edge getNextSemInteractEvents(FSM fsm1, Edge e1, Node s0){
		//System.out.println("e1="+e1.getEvent()+" s0="+s0.getLabel());
		List<Edge> outsEdges=e1.getTarget().getSucc();
		Edge e2=null;
		
		if (outsEdges.size()==0) return null;
			
		int index=0;
		
		for (int i = 0; i < outsEdges.size()*9; i++) {
			index=pathUtils.getRandomIndex(outsEdges.size(), true);
			e2=outsEdges.get(index);
			//System.out.println(" - e2="+e2.getEvent()+" s2="+e2.getTarget().getLabel());
			//System.out.println("2c");
			if (SEMchecker.isSEM(fsm1, e1, e2, s0, e2.getTarget())){
				//System.out.println("2d");
				return e2;
			}
			//else System.out.println("2e");
		}
		
		
		
		/*
		int[] indexes=pathUtils.getRandomIndexes(outsEdges.size());
		
		for (int i = 0; i < indexes.length; i++) {
			
			if (outsEdges.size()>=indexes[i]) {
				e2=outsEdges.get(indexes[i]);
				
				if (SEMchecker.isSEM(fsm, e1, e2, s0, e2.getTarget())){
					System.out.println("2d");
					return e2;
				}
			}
		}*/
		return null;
		
	}
	
}
