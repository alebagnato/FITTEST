package eu.fittest.modelInference.imu.java_MKV;

import Jama.Matrix;

/**
 * test the 'steady state probability' algorithm
 * 
 * @author Alessandro Marchetto
 *
 */
public class test1 {
	SteadyStateProbability steadystatemat;
	
	public static void main(String[] args) { 
		test1 t=new test1();
		//t.test1();
		t.test2();
	}
	
	public void test1(){
		steadystatemat=new SteadyStateProbability();
		
		int N = 3;
      double[][] transition = { { 0.2, 0.8, 0},
        						  { 0.4, 0, 0.6},
        						  { 0.5, 0.5, 0}
                                 };
 
		
        Matrix x=steadystatemat.computeSteadyStateMatrix_byEqSolving(N, transition);
        steadystatemat.print(x);
        double[][] xarray=x.getArray();
		for (int i = 0; i < xarray.length; i++) {
			System.out.println(xarray[i][0]);
		}


	}
	
	public void test2(){
		steadystatemat=new SteadyStateProbability();
		
		int N = 3;
		/*double[][] transition = { { 0.2, 0.8, 0},
        						  { 0.4, 0, 0.6},
        						  { 0.5, 0.5, 0}
                                 };
        */
      double[][] transition = { { 0.2, 0.8, 0},
				  				{ 0.4, 0, 0.6},
				  				{ 0.5, 0.5, 0}
               };
		
        Matrix x=steadystatemat.computeSteadyStateMatrix_byEqSolving(N, transition);
        steadystatemat.print(x);
        double[][] xarray=x.getArray();
		for (int i = 0; i < xarray.length; i++) {
			System.out.println(xarray[i][0]);
		}
		double max=0;
		double tmp=0;
		
		double[][] matarray=x.getArray();
		for (int i = 0; i < matarray.length; i++) {
			
			for (int j = 0; j < transition[i].length; j++) {
				if ((matarray[i][0]!=0)&&(transition[i][j]!=0)){
					tmp=(new Double(1).doubleValue())/(matarray[i][0]*transition[i][j]);
					if (max<tmp) max=tmp;
				}
					
			}
			
		}
		
		System.out.println("max="+max);


	}
	
}
