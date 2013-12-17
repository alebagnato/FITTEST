package coveragecollector;

/**
  * Universitat Politecnica de Valencia 2013
  * Camino de Vera, s/n
  * 46022 Valencia, Spain
  * www.upv.es    
  */

/**
  * author: Urko Rueda Molina
  * version 1.0
  * package coveragecollector
  */
  
public class CvgValue {
	int cvgSize; // how many cvg meassures?
	int cvgCount; // how many times has been covered?
	boolean isCvg; // has been covered by current meassure?
	public CvgValue() {
		reset();
	}
	public void reset() {
		this.cvgSize = 1;
		this.cvgCount = 0;
		this.isCvg = false;
	}
}