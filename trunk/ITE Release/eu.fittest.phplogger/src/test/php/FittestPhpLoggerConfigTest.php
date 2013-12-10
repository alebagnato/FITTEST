<?php

require_once 'PHPUnit/Framework.php';

class FittestPhpLoggerConfigTest extends PHPUnit_Framework_TestCase 
{
    
    function testRootNoConf() 
    {
     	require_once ('FittestPhpLoggerConfig.php');
     	$tmpdir = setupTmpDir(array());

  		$conf = new FittestPhpLoggerConfig();		
		$this->assertFalse($conf->isActive());

    	rrmdir($tmpdir);
    }

    function testRootWithConfTrue() 
    {
     	require_once ('FittestPhpLoggerConfig.php');
     	$tmpdir = setupTmpDir(array(),0, PROP_ENABLED."=true");

  		$conf = new FittestPhpLoggerConfig();		
		$this->assertTrue($conf->isActive());

    	rrmdir($tmpdir);
    }
    
    function testGetLoggingDirectory() 
    {
     	require_once ('FittestPhpLoggerConfig.php');
     	$tmpdir = setupTmpDir(array(),0, PROP_DIR."=/some/./directory");

  		$conf = new FittestPhpLoggerConfig();		
		$this->assertEquals("/some/./directory",$conf->getLoggingDirectory());

    	rrmdir($tmpdir);
    }

    function testRootWithConfFalse() 
    {
     	require_once ('FittestPhpLoggerConfig.php');
     	$tmpdir = setupTmpDir(array(),0, PROP_ENABLED."=false");

  		$conf = new FittestPhpLoggerConfig();		
		$this->assertFalse($conf->isActive());

    	rrmdir($tmpdir);
    }
   function testRootWithConfDirNoFile() 
    {
     	require_once ('FittestPhpLoggerConfig.php');
     	$tmpdir = setupTmpDir(array(),0, NULL);

  		$conf = new FittestPhpLoggerConfig();		
		$this->assertFalse($conf->isActive());

    	rrmdir($tmpdir);
    }

   function testSubWithConf() 
    {
     	require_once ('FittestPhpLoggerConfig.php');
     	$tmpdir = setupTmpDir(array("a","b","c","d"),2, PROP_ENABLED."=true");

  		$conf = new FittestPhpLoggerConfig();		
		$this->assertTrue($conf->isActive());

    	rrmdir($tmpdir);
    }

}

function setupTmpDir($path, $conflevel = -1, $conftext = null)
{
     	$tmpdir = tmpdir();
     	if(count($path) > 0)
     	{
		 	$scriptdir = $tmpdir.DIRECTORY_SEPARATOR.implode(DIRECTORY_SEPARATOR, $path);
			mkdir($scriptdir, 0777, true);
		}
		else
		{
			$scriptdir = $tmpdir;
		}
		
		$scriptfile = $scriptdir.DIRECTORY_SEPARATOR."dummy.php";		
		fclose(fopen($scriptfile,"w"));		
		$_SERVER['SCRIPT_FILENAME'] = $scriptfile;
		
		if($conflevel >=0 && $conflevel <= count($path))
		{
			$path2 = array_slice($path,0,$conflevel);
			$confdir = $tmpdir.DIRECTORY_SEPARATOR.implode(DIRECTORY_SEPARATOR,$path2).DIRECTORY_SEPARATOR.FITTEST;
	    	mkdir($confdir);
			if(!is_null($conftext))
			{
		    	$conffile = $confdir.DIRECTORY_SEPARATOR.CONFFILE;
		    	file_put_contents($conffile,$conftext);
			}
		}
		return $tmpdir;
}

function tmpdir()
{
     	$tmpfname = tempnam(sys_get_temp_dir() , "tmp");
     	unlink($tmpfname);
     	mkdir($tmpfname);
     	return $tmpfname;
}

 function rrmdir($dir) {
   if (is_dir($dir)) {
     $objects = scandir($dir);
     foreach ($objects as $object) {
       if ($object != "." && $object != "..") {
         if (filetype($dir."/".$object) == "dir") rrmdir($dir."/".$object); else unlink($dir."/".$object);
       }
     }
     reset($objects);
     rmdir($dir);
   }
 }

?>
