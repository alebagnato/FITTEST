private FlashApplication driver;

public _Constructor_() throws Exception
{
        super(new File("test.properties"));
}

@Before
public void setUpPageDriver() throws Exception  {
        driver = getFlashApp();
}

