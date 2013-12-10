<?php
set_include_path(dirname(__FILE__).PATH_SEPARATOR.get_include_path());

require_once('FittestPhpLoggerConfig.php');

$config = new FittestPhpLoggerConfig();
if($config->isActive() && is_dir($config->getLoggingDirectory()))
{
	FittestPhpLogger::activateLogging($config);
}

class FittestPhpLogger
{
	static $logdir;
	static $logfile_handle = FALSE;
	static $php_end; /* manages end of php activity */
	
	static function startLog()
	{
		self::$php_end = FALSE;
		self::$logfile_handle = new XmlWriter();
		self::$logfile_handle->openMemory();
		self::$logfile_handle->startDocument("1.0", "utf-8", "yes");
		self::$logfile_handle->startElementNS(NULL, "phplog","http://fittest.eu/phplog");
		self::$logfile_handle->writeAttributeNS("xsi","schemaLocation","http://www.w3.org/2001/XMLSchema-instance", "http://fittest.eu/phplog phplog.xsd ");
	}
		 

	static function writeRequestEntry()
	{
		self::$logfile_handle->startElement ( "request");
		
		self::$logfile_handle->writeElement ( "method", $_SERVER['REQUEST_METHOD'] );
		self::$logfile_handle->writeElement ( "uri", $_SERVER['REQUEST_URI'] );
		self::$logfile_handle->writeElement ( "querystring", $_SERVER['QUERY_STRING'] );
		self::$logfile_handle->startElement("headers");
		self::writeHeader ( "UserAgent: ".$_SERVER['HTTP_USER_AGENT'] );
		self::$logfile_handle->endElement ();
		

		self::$logfile_handle->startElement ( "get");
		self::writeArray($_GET);
		self::$logfile_handle->endElement ();

		self::$logfile_handle->startElement ( "post");
		self::writeArray($_POST);
		self::$logfile_handle->endElement ();

		self::$logfile_handle->startElement ( "cookie");
		self::writeArray($_COOKIE);
		self::$logfile_handle->endElement ();
		
		self::$logfile_handle->endElement ();		
	}
	
	static function endLog()
	{
		self::$logfile_handle->endElement();
		self::$logfile_handle->endDocument();		
		$logtxt = self::$logfile_handle->outputMemory(TRUE);
		$logfile = self::$logdir .DIRECTORY_SEPARATOR. self::filename();
		file_put_contents($logfile, $logtxt);
	}
	
	static function filename()
	{  
		$time = isset($_SERVER['REQUEST_TIME']) ? $_SERVER['REQUEST_TIME'] : time() ;
		$ip = $_SERVER['REMOTE_ADDR'];
		$port = $_SERVER['REMOTE_PORT'];
		return "$ip-$port-$time.xml";
	}
	
	static function shutdown_callback()
	{ 
		ob_flush();
        flush();
        ob_end_flush();

        self::output_callback("", PHP_OUTPUT_HANDLER_END); /* force php output end */

        self::writeResponseEntry();
		self::endLog();
	}
	
	static function writeHeader($header)
	{
		self::$logfile_handle->writeElement("header", $header);
	}
	
	static function writeResponseEntry()
	{
		self::$logfile_handle->startElement ( "response");
		
		self::$logfile_handle->startElement("headers");
		/*if(function_exists('apache_response_headers'))
		{
			foreach(apache_response_headers() as $name => $value)
			{
				self::writeHeader ( "$name: $value" );
			}
		}
		else*/
		{
			foreach(headers_list() as $header)
			{
				self::writeHeader ( $header );
			}
		}
		self::$logfile_handle->endElement ();
		
		self::$logfile_handle->endElement ();		
	}

	static function output_callback($content, $flag)
	{ 
		if ($php_end)
		{
			return FALSE; // end of PHP already processed
		}
		
		if($flag & PHP_OUTPUT_HANDLER_START)
		{
			self::$logfile_handle->startElement("data");
		}
		
		self::$logfile_handle->text(base64_encode($content)) ;
		
		if($flag & PHP_OUTPUT_HANDLER_END)
		{
			self::$logfile_handle->endElement();
			self::$php_end = TRUE;
		}	
		return FALSE;
	}
	
	static function activateLogging($config)
	{
		self::$logdir = $config->getLoggingDirectory();
		self::startLog();
		self::writeRequestEntry();
		register_shutdown_function("FittestPhpLogger::shutdown_callback");
		ob_start("FittestPhpLogger::output_callback");

		/*runkit_function_copy("setcookie", "setcookie_original");
		runkit_function_redefine("setcookie"
                        , '$name,$value,$expire,$path,$domain,$secure, $httponly'
                        , 'return setcookie_redefined($name,$value,$expire,$path,$domain,$secure, $httponly);'
                        );
*/
		
	}

	static function writeArray($value)
	{
		foreach( $value as $index => $entry)
		{
			self::$logfile_handle->startElement("entry");
			self::$logfile_handle->writeAttribute("index", "$index");
			self::writeValue($entry);
			self::$logfile_handle->endElement();
		}	
	}
	static function writeValue($value)
	{
		if(is_null($value))
		{
			self::$logfile_handle->writeElement ( "null");
		}
		else if(is_string($value))
		{
			self::$logfile_handle->writeElement ( "string", $value );
		}
		else if(is_bool($value))
		{
			self::$logfile_handle->writeElement ( "bool", "$value" );
		}
		else if(is_int($value))
		{
			self::$logfile_handle->writeElement ( "int", "$value" );
		}
		else if(is_float($value))
		{
			self::$logfile_handle->writeElement ( "float", "$value" );
		}
		else if(is_object($value))
		{
			self::$logfile_handle->startElement("object");
			self::$logfile_handle->writeAttribute("classname", get_class($value));
			foreach( get_object_vars($value) as $name => $entry)
			{
				self::$logfile_handle->startElement("property");
				self::$logfile_handle->writeAttribute("name", $name);
				self::writeValue($entry);
				self::$logfile_handle->endElement();
			}
			self::$logfile_handle->endElement();
		}
		else if(is_array($value))
		{
			self::$logfile_handle->startElement("array");
			self::writeArray($value);
			self::$logfile_handle->endElement();
		}
	}

}
?>