private Flexstore flexstore = new Flexstore();

@Rule public TestName name = new TestName();
	
	
@Before
public void starup() throws Throwable {
	
	if (!LoggerManager.initialized) {
		LoggerManager.initialize();
	}	
	
	Logger logger = Logger.getLogger(LoggerManager.LOGGER);
	
	logger.log(Level.INFO, "**** TEST:"+name.getMethodName());
	flexstore.startup();

}

@After
public void shutdown() throws Throwable {

	flexstore.shutdown();

}

