package eu.fbk.se.fsm.transformer;

public interface IProgressListener {
	public void start(int totalWorkLoad);
	public void progress(int workload);
	public void finish();
	
}
