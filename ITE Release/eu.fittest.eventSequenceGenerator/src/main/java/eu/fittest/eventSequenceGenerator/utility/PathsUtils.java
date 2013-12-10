package eu.fittest.eventSequenceGenerator.utility;

import eu.fittest.eventSequenceGenerator.data.*;

import java.util.List;
import java.util.Vector;

import eu.fittest.modelInference.fsmInference.utility.Utility;

/**
*
* @author Alessandro
*
*/
public class PathsUtils {

	Utility utils=new Utility();
	
	public int getRandomIndex(int N_max, boolean nonzero){
		int n=utils.randomInt(N_max, nonzero);
		if (n<0) n=n*(-1);
		return n;
	}
	public double getRandomDouble_0_1(){
		return Math.random();
	}
	
	public boolean isContained(Vector<Path> paths, Path pathToBeChecked){
		for (Path path : paths) {
			if (path.equals(pathToBeChecked)) return true;
		}
		return false;
	}
	
	public Vector<Path> copyPaths(Vector<Path> current){
		Vector<Path> newVersion=new Vector<Path>();
		Path p;
		for (Path pCurrent : current) {
			p=new Path();
			p.copy(pCurrent);
			newVersion.add(p);
		}
		return newVersion;
	}
	
	public Vector<Path> randomSample(Vector<Path> suite_sem, int N_max){
		Vector<Path> sampledSuite_sem=new Vector<Path>();
		int[] indexes=getRandomIndexes(N_max);
		for (int i = 0; i < indexes.length; i++) {
			if (suite_sem.size()>=indexes[i]) sampledSuite_sem.add(suite_sem.get(indexes[i]));
		}
		return sampledSuite_sem;
	}
	
	public int[] getRandomIndexes(int N_max){
		int[] indexes=new int[N_max];
		Vector<Integer> indexesV=new Vector<Integer>();
		int index;
		
		for (int j = 0; j < N_max; j++) {
			index=utils.randomInt(N_max, false);
			if (!iscontained(indexes,index)){
				//indexes[indexes.length]=index;
				indexesV.add(new Integer(index));
			}
		}
		
		 indexes=new int[indexesV.size()];
		 for (int i = 0; i < indexesV.size(); i++) {
			 indexes[i]=indexesV.get(i);
		}
		return indexes;
	}
	
	boolean iscontained(int[] indexes, int index){
		int i=0;
		while (i<indexes.length){
			if (indexes[i]==index) return true;
			i++;
		}
		return false;
	} 
	
	
	public boolean exists(Vector<Path> paths, Path ptobechecked){
		for (Path path : paths) {
			if (path.equals(ptobechecked)) return true;
		}
		return false;
	}
}
