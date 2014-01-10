package eu.fittest.modelInference.imu.java_MKV;

import Jama.Matrix;
import Jama.EigenvalueDecomposition;
import java.util.*;

/**
 * 
 * 
 * @author 
 *
 */
public class SteadyStateProbability {

    public static void main(String[] args) { 
    	SteadyStateProbability st=new SteadyStateProbability();
    	st.setupTransition();
    }
    	
    
    public void setupTransition() {
    	//the state transition matrix
    	int N = 3;
    	double[][] transition = { { 0.2, 0.8, 0},
        						  { 0.4, 0, 0.6},
        						  { 0.5, 0.5, 0}
                                 };
    	computeSteadyStateMatrix_byIterations(N,transition);
    }
    
    public void print(Matrix x){
    	x.print(9, 6);
    }
    
       
    public Matrix computeSteadyStateMatrix_byIterations(int N, double[][] transition) { 
        // compute using 256 iterations of power method
        Matrix A = new Matrix(transition);
        A = A.transpose();
        Matrix x = new Matrix(N, 1, 1.0 / N); 
        for (int i = 0; i < 256; i++) {
            x = A.times(x);
            x = x.times(1.0 / x.norm1());       
        }
        //x.print(9, 6);
        //print(x);
        return x;
    }
    
      
    public Matrix computeSteadyStateMatrix_byEqSolving(int N, double[][] transition) { 
    	
    	Matrix A = new Matrix(transition);
        A = A.transpose();
        Matrix x = new Matrix(N, 1, 1.0 / N); // initial guess for eigenvector
       

        // If ergordic, stationary distribution = unique solution to Ax = x
        // up to scaling factor.
        // We solve (A - I) x = 0, but replace row 0 with constraint that
        // says the sum of x coordinates equals one
        Matrix B = A.minus(Matrix.identity(N, N));
        for (int j = 0; j < N; j++)
            B.set(0, j, 1.0);
        Matrix b = new Matrix(N, 1);
        b.set(0, 0, 1.0);
        x = B.solve(b);
        //x.print(9, 6);
        //print(x);
        return x;
    }

}


