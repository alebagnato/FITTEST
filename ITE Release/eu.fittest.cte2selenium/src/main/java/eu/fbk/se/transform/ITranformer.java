package eu.fbk.se.transform;

public interface ITranformer {
	
	public void register(IProgressListener listener);
	
	public boolean transform(String cteFile, String outputFolder, boolean validTestOnly) throws TransformException;
	
	public boolean transform(String cteFile, String domainInputFile, String outputFolder, boolean validTestOnly) throws TransformException;
}
