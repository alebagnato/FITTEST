<?php

require_once 'PHPUnit/Framework.php';

class PropertiesTest extends PHPUnit_Framework_TestCase 
{
    function testGetProperty() 
    {
     	require_once ('Properties.php');
		$properties = create_Properties("# comment\njava.system=hello");
        $this->assertEquals('hello', $properties->getProperty("java.system"));
    }

    function testGetPropertyDefault() 
    {
     	require_once ('Properties.php');
    	$properties = create_Properties("# comment\njava.system=hello");
        $this->assertEquals('default', $properties->getProperty("java.none", "default"));
    }


    function testgetPropertyNull() 
    {
     	require_once ('Properties.php');
    	$properties = create_Properties("# comment\n");
        $this->assertNull($properties->getProperty("java.system"));
    }
    
    function testMultiLine() 
    {
     	require_once ('Properties.php');
    	$properties = create_Properties("# comment\njava.system=hello\\\n world");
        $this->assertEquals("hello\n world", $properties->getProperty("java.system"));
    }

}
function create_Properties($txt)
{
     	$fname = tempnam(sys_get_temp_dir(),"config");
     	$handle = fopen($fname,"w");
     	fwrite($handle,$txt);
     	fclose($handle);     	
    	$properties = new Properties($fname);
    	unlink($fname);
    	return $properties;
}    	
?>
