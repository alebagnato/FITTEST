<?php
function shutdown()
{
$File="/watt/coverage.txt";
$fh = fopen($File,'a') or die("Error: cant open file");
//$oldcov = unserialize(file_get_contents($File));

  $cov = xdebug_get_code_coverage();
$appname = $_GET['wattappname'];
$traceid = $_GET['watttraceid'];
$requestid = $_GET['wattrequestid'];
$linktype = $_GET['wattlinktype'];

foreach ($cov as $fname => $fcov)
  {  
    foreach($fcov as $fline => $value)
       {  $line = $appname.",".$traceid.",".$requestid.",".$fname.",".$fline.",\n";
          fputs($fh,$line) or die ("error writing"); 
              
       }
   }
 
}
register_shutdown_function('shutdown');
xdebug_start_code_coverage();
?>
