package eu.fbk.se.transform;

public interface IProgressListener {
	public void start(int totalWorkLoad);
	public void progress(int workload);
	public void finish();
	
}
