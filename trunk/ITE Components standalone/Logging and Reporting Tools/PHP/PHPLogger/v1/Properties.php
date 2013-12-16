<?php
/**
 * @author Arthur Baars
 * @version 1.0
 * @package phplogger
 */
 
/**
 * Parser for Java Properties File
 * @package phplogger
 * @subpackage classes
 */ 
class Properties
{
    /**
    * Construct a Properties object.
    * @param string $filename 
    * 
    */
	function Properties($filename)
	{
		$txt = file_get_contents($filename);
		$this->data = self::parse_properties($txt);
	}
	
	public function getProperty($name,$default = NULL)
    {
        if (array_key_exists($name, $this->data)) 
        {
            return $this->data[$name];
        }
		else
		{
       		return $default;
        }
    }
    
	private static function parse_properties($txtProperties) 
	{
 		$result = array();
		// $lines = split("\n", $txtProperties);
		$lines = explode("\n", $txtProperties);
 		$key = "";
 		
 		$isWaitingOtherLine = false;
 		foreach($lines as $i=>$line) 
 		{
 			if(empty($line) || (!$isWaitingOtherLine && strpos($line,"#") === 0)) continue;

 			if(!$isWaitingOtherLine) 
 			{
 				$key = substr($line,0,strpos($line,'='));
 				$value = substr($line,strpos($line,'=') + 1, strlen($line));
 			}
 			else 
 			{
 				$value .= $line;
 			}
 			/* Check if ends with single '\' */
 			if(strrpos($value,"\\") === strlen($value)-strlen("\\")) 
 			{
				$value = substr($value, 0, strlen($value)-1)."\n";
				$isWaitingOtherLine = true;
			}
			else 
			{
				$isWaitingOtherLine = false;
			}
 			$result[$key] = $value;
 			unset($lines[$i]);
 		}
 		return $result;
	}
}
?>