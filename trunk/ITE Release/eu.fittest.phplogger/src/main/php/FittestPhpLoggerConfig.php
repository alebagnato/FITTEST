<?php
/*
phploggercomponent.logging.enabled whose values can be true/false
phploggercomponent.logging.dir
*/
require_once('Properties.php');

define("FITTEST", ".fittest");
define("CONFFILE","phploggercomponent.conf");
define("PROP_ENABLED", "phploggercomponent.logging.enabled");
define("PROP_DIR", "phploggercomponent.logging.dir");

class FittestPhpLoggerConfig
{
	private $config;
	
	function FittestPhpLoggerConfig()
	{
		if($confFile = self::findConfigFile())
		{
			$this->config = new Properties($confFile);
		}
		else
		{
			$this->config = NULL;
		}
	}
	
	function isActive()
	{
		return !is_null($this->config) && $this->config->getProperty(PROP_ENABLED) == "true";
	}

	function getLoggingDirectory()
	{
		return is_null($this->config)? NULL : $this->config->getProperty(PROP_DIR);
	}
	
	private static function findConfigFile()
	{
		$scriptFile = realpath ($_SERVER['SCRIPT_FILENAME']);

		$dir = dirname($scriptFile);
		while(true)
		{
			$dir2 = dirname($dir);
			if($dir == $dir2)
			{
				return FALSE;
			}
			else 
			{
				$confFile = $dir.DIRECTORY_SEPARATOR.FITTEST.DIRECTORY_SEPARATOR.CONFFILE;
				if(is_file($confFile) && is_readable($confFile) )
				{
					return $confFile;
				}
			}
			$dir = $dir2;
		}		
	}
	
}

?>