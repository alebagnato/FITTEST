package eu.fittest.fbk.efsm2ct.log2efsm.common;

public class StateMapping {

	private String outType;
	private String outSymbol;
	private String outPredFmt;

	public String getOutType() {
		return outType;
	}

	public void setOutType(String outType) {
		this.outType = outType;
	}

	public String getOutSymbol() {
		return outSymbol;
	}

	public void setOutSymbol(String outSymbol) {
		this.outSymbol = outSymbol;
	}

	public String getOutPredFmt() {
		return outPredFmt;
	}

	public void setOutPredFmt(String outPredFmt) {
		this.outPredFmt = outPredFmt;
	}

	@Override
	public String toString() {
		return "StateMapping [outType=" + outType + ", outSymbol=" + outSymbol + ", outPredFmt=" + outPredFmt + "]";
	}

}
