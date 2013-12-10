package eu.fittest.ucl.eventinf.api;

public interface ModelInferenceListener {
	public void onStart();
	public void progress(double percent, String message);
	public void onStop();
}
