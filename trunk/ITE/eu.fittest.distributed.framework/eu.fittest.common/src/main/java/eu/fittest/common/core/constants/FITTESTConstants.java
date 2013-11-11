package eu.fittest.common.core.constants;

import java.io.File;




public interface FITTESTConstants {
    
    public static final Integer MIN_THREAD_POOL_SIZE = 40;
    public static final Integer MAX_THREAD_POOL_SIZE = 60;

    
    public static final String[] CIPHER_SUITE = new String[]{"SSL_DH_anon_WITH_RC4_128_MD5"};

    
    public static final String FITTEST_ITE_HTTP_PORT = "fittest.ite.http.port";

    
    public static final String FITTEST_ITE_HTTP_DIR = "fittest.ite.http.dir";

    
    public static final String FITTEST_SERVER_PORT_START = "fittest.ite.server.port.start";

    
    public static final String FITTEST_SERVER_PORT_RANGE = "fittest.ite.server.port.range";
    
    public static final String FITTEST_SERVER_PORT_INCREMENT = "fittest.service.server.port.increment";

    public static final String FITTEST_SUT_AGENT_JAR_FILENAME = "fittest.agent.sut.jar.filename";
    
    public static final String FITTEST_SUT_AGENT_MAINCLASS_NAME = "fittest.agent.sut.mainclass.name";
    
    
    public static final String DEFAULT_FITTEST_ITE_HTTP_DIR = "./www";

    
    public static final Integer DEFAULT_FITTEST_ITE_HTTP_PORT = 8008;

    
    public static final Integer DEFAULT_FITTEST_ITE_SERVER_PORT_START = 37600;

    
    public static final Integer DEFAULT_FITTEST_ITE_SERVER_PORT_RANGE = 200;
    public static final Integer DEFAULT_FITTEST_SERVER_PORT_INCREMENT = 100;


    // Added by Cu to control the GUI of the client agents
	public static final String FITTEST_SUT_AGENT_UI_ENABLED = "fittest.agent.sut.ui.enabled.name";
	public static final String DEFAULT_FITTEST_SUT_AGENT_UI_ENABLED = "false"; // true or false

    
    public static final String FITTEST_SUT_AGENT_JNLP_FILENAME = "fittest.agent.sut.jnlp.filename";

    
    public static final String DEFAULT_FITTEST_SUT_AGENT_JNLP_FILENAME = "FITTESTAgent.jnlp";
    
    public static final String DEFAULT_FITTEST_SUT_AGENT_JAR_FILENAME = "eu.fittest.agent.sut.jar";

    
    public static final String DEFAULT_FITTEST_SUT_AGENT_MAINCLASS = "eu.fittest.agent.Main";

    
    public static final String FITTEST_AGENT_COMPONENT_PORT_START = "fittest.agent.component.port.start";

    
    public static final String FITTEST_ITE_ADDRESS = "fittest.ite.address";

    
    public static final String FITTEST_ITE_PORT = "fittest.ite.port";

    
    public static final String DEFAULT_FITTEST_JAXB_CONTEXT = "eu.fittest.common.core.xml";

    
    public static final Integer REGISTRATION_LEASE_DURATION = 3600;//seconds

    
    public static final String FITTEST_SERVICE_FILETRANSFER_BASEDIR = "fittest.service.filetransfer.basedir";

    
    public static final String DEFAULT_FITTEST_SERVICE_FILETRANSFER_BASEDIR = new File(System.getProperty("java.io.tmpdir")).toURI().toString();

    // by urueda
    public static final long FITTEST_ASYNCHRONOUS_UPLOADS_INTERVAL = 10000; // miliseconds
    
    public static final long DEFAULT_CHECK_LEASE_PERIOD = 600000;//milliseconds
    
    public static final long RENEW_LEASE_BEFORE = 5000;//seconds
    
    public static final String DEFAULT_FITTEST_AGENT_COMPONENT_DEPLOYMENT_BASEDIR = "components";

	public static final String FITTEST_AGENT_PORT = "fittest.agent.port";

	public static final long DEFAULT_AUTOREGISTRATION_DELAY = 60000;
	
	public static final String FITTEST_SUT_AGENT_AUTOREGISTRATION_DELAY = "fittest.agent.sut.autoregistration.delay";

	public static final String FITTEST_SETTINGS_DIR = new File(System.getProperty("user.home")).toURI().toString()+".fittest/";


}
