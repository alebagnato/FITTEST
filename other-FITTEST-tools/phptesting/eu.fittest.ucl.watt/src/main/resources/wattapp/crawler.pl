# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Copyright (c) 2010-2050, Nadia Alshahwan (nadia.alshahwan@gmail.com). All rights reserved. This program and the accompanying materials are made available under the terms of the 3-Clause BSD License which accompanies this distribution, and is available at http://www.opensource.org/licenses/BSD-3-Clause. 
# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

use Thread::Queue;
use Time::HiRes qw(gettimeofday tv_interval);
use Getopt::Long;
use process;
use mydbi;
use DBinterface;
use HttpEngine;

$t0 = [gettimeofday];
our $wattFolder = "";
our $dbUser = "";
our $dbPassword = "";
our $folder = "";
our $username = "";
our $password = "";
our $appname= "";
our $login="";
our $input="";
our $userinput="";
our $compare="";
our %nolinkpages =();
$result = Getopt::Long::GetOptions('dbUser=s' => \$dbUser,'dbPassword=s' => \$dbPassword,'wattApp=s' => \$wattFolder,'app=s' => \$appname,'login'=>\$login,'input=s'=>\$input,'userinput'=>\$userinput,'compare=s'=>\$compare,'user=s' => \$username,'password=s' => \$password,'url=s' =>\$URL,'folder=s' =>\$folder,'reset' =>\$reset,'time=i' =>\$time);

if(!$input)
{$input='random';}

if(!$compare)
{$compare='structure';}

if (!$time)
{$time = 600;}

$reset = 'y'; 
open(MYFILE, ">".$wattFolder."/coverage.txt");
close (MYFILE);

if (!$URL)
{die "No URL provided in parameters";}

my $filter= getfilter($URL);


$subfolder = $filter;
$subfolder =~ s/\//-/g;
if (!$folder)
{$folder = $wattFolder."/downloads/".$subfolder.$appname;}
else
{$folder = $folder.'/'.$subfolder;}

if ($reset)
{
  my $status = system("mysql -u".$dbUser." -p".$dbPassword." $appname < ".$wattFolder."/watt.sql");
  $status = system("mysqldump -u".$dbUser." -p".$dbPassword." $appname > ".$wattFolder."/$appname.sql");
  $status = system("mysql -u".$dbUser." -p".$dbPassword." $appname < ".$wattFolder."/reset.sql");
  if (($status >>=8) != 0){
   die "failed to intialize database";}
   
   if (-d $folder)
   {system('rm','-r',$folder);} 
}

unless (-d $folder)
{mkdir($folder,0777) ||  die "can't create downloads folder:$folder";}

#our %ignorehidden = ('pageTitle','y','id','y','faqId','y','pageNum','y');

my $q = new Thread::Queue;
our $dbh =getconnection($dbUser, $dbPassword, $appname);
#get all database handles
our $sthdefval = gethanddefval();
our $sthoptval = gethandoptval();
our $sthinputs = gethandinputs();
our $sthoptions = gethandoptions();
our $sthlogin = gethandlogin();
our $sthmaxreqid = gethandmaxreqid();
our $sthinstrace = gethandinstrace();
our $sthinsreq = gethandinsreq();
our $sthstop = gethandstop();
our $sthinslink = gethandinslink();
our $sthforminfo = gethandforminfo();
our $sthinsform = gethandinsform();
our $sthinsformmore = gethandinsformmore();
our $sthcountforms = gethandcountforms();
our $sthinsinput = gethandinsinput();
our $sthmaxselect = gethandmaxselect();
our $sthinsformdata = gethandinsformdata();
our $sthinsformselect = gethandinsformselect();
our $sthupdformlogin = gethandupdformlogin();
our $sthupdformseq = gethandupdformseq();
our $sthupdformclassid = gethandupdformclassid();
our $sthtargetseq = gethandtargetseq();
our $sthformclassidseq = gethandformclassidseq();
our $sthtargetcounts = gethandtargetcounts();
our $sthtargetcount = gethandtargetcount();
our $sthseqs = gethandseqs();
our $sthlinkclass = gethandlinkclass();
our $sthformseq = gethandformseq();
our $sthmaxtarget = gethandmaxtarget();
our $sthmaxformclassid = gethandmaxformclassid();
our $sthinsseqsim = gethandinsseqsim();

our $appid = insertapp($URL,$appname);

our $browser = LWP::UserAgent->new( agent => 'Perlprogram/0.1', keep_alive => 1,requests_redirectable => [ 'GET', 'HEAD', 'POST' ] );
$browser->timeout(60);
$browser->cookie_jar( {} );
my $traceid = 0;

my $initialized = 'n';
#while ((mincalls() || !$traceid))
while (tv_interval ($t0)< $time)
{ 
  $traceid++;
  my $dbtry = 0;
  while ($dbtry <10 && $initialized eq 'n')
  {
    my $status = system("mysql -u".$dbUser." -p".$dbPassword." $appname < ".$wattFolder."/$appname.sql");
    if (($status >>=8) != 0)
    {$dbtry++;}
    else 
    {$initialized = 'y';}
  }
  if ($initialized eq 'n' && $appname ne 'phpsysinfo' )
  {die "failed to intialize database $status $!";}

  $browser->cookie_jar( {} );
  my $tracesize = 0;
  my $requestid = insertrequest($traceid);
  my $linkclass = processlink($URL);
  my @link = ($URL,-1,'get',$requestid,$linkclass,NULL);

  while(@link && tv_interval ($t0)< $time && $requestid <50)
  {
    $url = @link[0];
    $callingid = @link[1];
    $method = @link[2];
    $requestid = @link[3];
    $formclass = @link[4];
    $f = @link[5];
    %formfields = %$f;

    @link= processpage($requestid,$url,$callingid,$traceid,$filter,$method,$formclass,\%formfields);
  }
}

if (-e "$wattFolder/coverage.txt" && !(-z "$wattFolder/coverage.txt")) {

  my $sthupload = $dbh->prepare("load data infile '".$wattFolder."/coverage.txt' into table coverage fields terminated by ',' lines terminated by '\n'") or die $dbh->errstr;
  $sthupload->execute() or die $sthupload->errstr();
}

$elapsed = tv_interval ($t0)*1000;

print "End of crawling\n";
#close handles
$sthdefval->finish();
$sthoptval->finish();
$sthinputs->finish();
$sthoptions->finish();
$sthlogin->finish();
$sthmaxreqid->finish();
$sthinstrace->finish();
$sthinsreq->finish();
$sthstop->finish();
$sthinslink->finish();
$sthforminfo->finish();
$sthinsform->finish();
$sthinsformmore->finish();
$sthcountforms->finish();
$sthinsinput->finish();
$sthmaxselect->finish();
$sthinsformdata->finish();
$sthinsformselect->finish();
$sthupdformlogin->finish();
$sthupdformseq->finish();
$sthupdformclassid->finish();
$sthtargetseq->finish();
$sthformclassidseq->finish();
$sthseqs->finish();
$sthtargetcounts->finish();
$sthtargetcount->finish();
$sthlinkclass->finish();
$sthformseq->finish();
$sthmaxtarget->finish();
$sthmaxformclassid->finish();
$sthinsseqsim->finish();
$elapsed = tv_interval ($t0)*1000;

$dbh->disconnect  or warn $dbh->errstr;

print "Elapsed time: $elapsed milliseconds\n";






