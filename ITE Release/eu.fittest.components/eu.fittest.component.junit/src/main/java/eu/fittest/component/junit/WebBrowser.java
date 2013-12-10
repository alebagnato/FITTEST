package eu.fittest.component.junit;

public enum WebBrowser {
	Firefox("*firefox"), IE("*iexplore"), Chrome("*chrome"), Safari("*safari"), Opera("*opera"), GoogleChrome("*googlechrome");
	
	private String _command;
	
	private WebBrowser(String command){
		_command = command;
	}
	
	@Override
	public String toString() {
		return _command;
	}
}
