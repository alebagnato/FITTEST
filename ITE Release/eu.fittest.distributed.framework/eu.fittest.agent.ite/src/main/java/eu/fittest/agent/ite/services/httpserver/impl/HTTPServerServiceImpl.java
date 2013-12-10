package eu.fittest.agent.ite.services.httpserver.impl;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;


import eu.fittest.agent.ite.services.httpserver.spec.IHTTPServerService;
import eu.fittest.common.core.constants.FITTESTConstants;
import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.server.spec.IServerService;
import eu.fittest.common.core.service.AbstractService;
import eu.fittest.common.util.NetworkUtils;
import org.jibble.simplewebserver.SimpleWebServer;


public class HTTPServerServiceImpl extends AbstractService implements IHTTPServerService {
    
    private SimpleWebServer _httpServer;

    
    public HTTPServerServiceImpl() throws FITTESTException {
    	assert(System.getProperty(FITTESTConstants.FITTEST_ITE_HTTP_DIR)!=null);
    	assert(System.getProperty(FITTESTConstants.FITTEST_ITE_HTTP_PORT)!=null);
    	assert(System.getProperty(FITTESTConstants.FITTEST_SUT_AGENT_JNLP_FILENAME)!=null);
    	assert(System.getProperty(FITTESTConstants.FITTEST_SUT_AGENT_JAR_FILENAME)!=null);
    }

    
    
    public String getName() {
        return IHTTPServerService.class.getName();
    }
    
    private void createHTMLPage(String targetDir, String htmlFileName, String jnlpFileName) throws FITTESTException{
    	String htmlcontent = "<HTML><HEAD><TITLE>The FITTEST Agent</TITLE></HEAD>\n"+
    						"<BODY><P>You can download and execute the <A href=\""+jnlpFileName+"\">FITTEST Agent</A>.</P></BODY></HTML>";
		try {
			PrintWriter writer = new PrintWriter(new File(targetDir+System.getProperty("file.separator")+htmlFileName));
			writer.println(htmlcontent);
			writer.close();
		} catch (FileNotFoundException e) {
			throw new FITTESTException(e.getMessage());
		}
    }
  
    /*
    private void createJNLPFile(String targetDir, String jnlpFileName, String jarName, String mainClassName, String localIP, int httpPort, int serverPort) throws FITTESTException{
    	String jnlpcontent = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
    			"<jnlp spec=\"1.0+\" codebase=\"http://"+localIP+":"+httpPort+"/\" href=\""+jnlpFileName+"\">\n" +
    			"<information><title>FITTEST Agent</title>\n" +
    			"<vendor>FITTEST Consortium</vendor>\n" +
    			"<shortcut><menu submenu=\"FITTEST\"/><desktop/></shortcut>\n" +
    			"<icon kind=\"splash\" href=\"splash.png\"/><icon kind=\"shortcut\" href=\"desktopshortcut.jpg\" width=\"32\" height=\"32\"/><icon kind=\"shortcut\" href=\"menushortcut.jpg\" width=\"16\" height=\"16\"\n/>" +
    			"<offline-allowed/>\n" + 
    			"</information>\n" +
    			"<resources>\n<j2se version=\"1.6+\" href=\"http://java.sun.com/products/autodl/j2se\"/>\n";
    	
    	String[] jarNames =jarName.split(File.pathSeparator); 
    	for(int i=0;i<jarNames.length;i++){
    			jnlpcontent+="<jar href=\""+jarNames[i]+"\"";
    			if(i==0) jnlpcontent+=" main=\"true\" ";
    			jnlpcontent+="/>\n";
    	}
    			
    			
		jnlpcontent += "<property name=\""+FITTESTConstants.FITTEST_ITE_ADDRESS+"\" value=\""+localIP+"\"/>\n" +
		"<property name=\""+FITTESTConstants.FITTEST_ITE_PORT+"\" value=\""+serverPort+"\"/>\n</resources>" +
		"<application-desc name=\"FITTEST Agent\" main-class=\""+mainClassName+"\">\n" +
		"</application-desc><update check=\"always\"/><security><all-permissions/></security></jnlp>";
    	try {
			PrintWriter writer = new PrintWriter(new File(targetDir+System.getProperty("file.separator")+jnlpFileName));
			writer.println(jnlpcontent);
			writer.close();
		} catch (FileNotFoundException e) {
			throw new FITTESTException(e.getMessage());
		} 
    }
    */
    
    /**
     * Create the java web start file
     * 
     * modified by @author cdnguyen to include the GUI enabling control
     * 
     * @param targetDir
     * @param jnlpFileName
     * @param jarName
     * @param mainClassName
     * @param localIP
     * @param httpPort
     * @param serverPort
     * @throws FITTESTException
     */
    private void createJNLPFile(String targetDir, String jnlpFileName, String jarName, String mainClassName, String localIP, int httpPort, int serverPort) throws FITTESTException{
    	String jnlpcontent = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
    			"<jnlp spec=\"1.0+\" codebase=\"http://"+localIP+":"+httpPort+"/\" href=\""+jnlpFileName+"\">\n" +
    			"<information><title>FITTEST Agent</title>\n" +
    			"<vendor>FITTEST Consortium</vendor>\n" +
    			"<shortcut><menu submenu=\"FITTEST\"/><desktop/></shortcut>\n" +
    			"<icon kind=\"splash\" href=\"splash.png\"/><icon kind=\"shortcut\" href=\"desktopshortcut.jpg\" width=\"32\" height=\"32\"/><icon kind=\"shortcut\" href=\"menushortcut.jpg\" width=\"16\" height=\"16\"\n/>" +
    			"<offline-allowed/>\n" + 
    			"</information>\n" +
    			"<resources>\n\t<j2se version=\"1.6+\" href=\"http://java.sun.com/products/autodl/j2se\"/>\n";
    	
    	String[] jarNames =jarName.split(File.pathSeparator); 
    	for(int i=0;i<jarNames.length;i++){
    			jnlpcontent+="\t<jar href=\""+jarNames[i]+"\"";
    			if(i==0) jnlpcontent+=" main=\"true\" ";
    			jnlpcontent+="/>\n";
    	}
    			
		jnlpcontent += "\t<property name=\""+FITTESTConstants.FITTEST_ITE_ADDRESS+"\" value=\""+localIP+"\"/>\n" +
		"\t<property name=\""+FITTESTConstants.FITTEST_ITE_PORT+"\" value=\""+serverPort+"\"/>\n" +
		"\t<property name=\""+FITTESTConstants.FITTEST_SUT_AGENT_UI_ENABLED+"\" value=\""+FITTESTConstants.DEFAULT_FITTEST_SUT_AGENT_UI_ENABLED+"\"/>\n</resources>\n" +
		"<application-desc name=\"FITTEST Agent\" main-class=\""+mainClassName+"\">\n" +
		"</application-desc>\n" +
		"<update check=\"always\"/>\n" +
		"<security><all-permissions/></security>\n" +
		"</jnlp>";
    	try {
			PrintWriter writer = new PrintWriter(new File(targetDir+System.getProperty("file.separator")+jnlpFileName));
			writer.println(jnlpcontent);
			writer.close();
		} catch (FileNotFoundException e) {
			throw new FITTESTException(e.getMessage());
		} 
    }
    
	
	public void start() throws FITTESTException {
		String httpdir = System.getProperty(FITTESTConstants.FITTEST_ITE_HTTP_DIR);
        Integer intHttpport = new Integer(System.getProperty(FITTESTConstants.FITTEST_ITE_HTTP_PORT)); 
            
        createHTMLPage(httpdir, "index.html", System.getProperty(FITTESTConstants.FITTEST_SUT_AGENT_JNLP_FILENAME));
		createJNLPFile(httpdir,System.getProperty(FITTESTConstants.FITTEST_SUT_AGENT_JNLP_FILENAME),
				System.getProperty(FITTESTConstants.FITTEST_SUT_AGENT_JAR_FILENAME),
				System.getProperty(FITTESTConstants.FITTEST_SUT_AGENT_MAINCLASS_NAME),
        		NetworkUtils.getLocalIP(), intHttpport, _registry.findService(IServerService.class).getServerPort());
        
        try {
            _httpServer = new SimpleWebServer(new File(httpdir), intHttpport);
            _httpServer.start();
        } catch (NumberFormatException e) {
            throw new  FITTESTException(e.getMessage());
        } catch (IOException e) {
            throw new  FITTESTException(e.getMessage());
        }
	}



	public void stop() throws FITTESTException {
		_httpServer.stopServer();
	}

}
