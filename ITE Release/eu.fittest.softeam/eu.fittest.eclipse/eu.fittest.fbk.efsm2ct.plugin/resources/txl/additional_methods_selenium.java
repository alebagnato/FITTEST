private FlashApplication driver;

public _Constructor_() throws Exception
{
	super(new File(JTest.class.getClassLoader().getResource("test.properties").toURI()));
}

@Before
public void setUpPageDriver() throws Exception  {
        driver = getFlashApp();
}

