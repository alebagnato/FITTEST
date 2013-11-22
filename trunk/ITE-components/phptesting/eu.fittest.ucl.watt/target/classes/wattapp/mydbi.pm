use DBI;
use DBD::mysql;


sub getconnection{
my $dsn = 'dbi:mysql:'.$_[2].':localhost:3306'; 
 
# set the user and password 
my $user = $_[0]; 
my $pass = $_[1]; 
 
# now connect and get a database handle  
my $dbh = DBI->connect($dsn, $user, $pass,{AutoCommit => 1,RaiseError => 0,PrintError => 0}) 
 or warn "Can not connect to the DB: $DBI::errstr\n"; 
return $dbh;
}
sub gethanddefval{

my $sth= $dbh->prepare("select defval from forminputs where formclass= ? and inputname=? ") or die $dbh->errstr;

return $sth;
}
sub gethandoptval{

my $sth= $dbh->prepare("select optionval from formselects where formclass=?  and selectname=? ") or die $dbh->errstr;

return $sth;
}

sub gethandinputs{

my $sth= $dbh->prepare("select inputname,type,defval from forminputs where formclass=?") or die $dbh->errstr;

return $sth;
}

sub gethandoptions{

my $sth= $dbh->prepare("select selectname,optionval from formselects where formclass =?") or die $dbh->errstr;

return $sth;
}

sub gethandlogin{

my $sth= $dbh->prepare("select login from forms where formclass=?") or die $dbh->errstr;

return $sth;
}

sub gethandmaxreqid{

my $sth= $dbh->prepare("select max(requestid) from traces where appname='$appname' and traceid=?") or die $dbh->errstr;

return $sth;
}

sub gethandinstrace{

my $sth= $dbh->prepare("insert into traces (appname,traceid,requestid) values ('$appname',?,?)") or die $dbh->errstr;

return $sth;
}

sub gethandinsreq{

my $sth= $dbh->prepare("insert into requests values ('$appname',?,?,?,?,?,?,?,?,?,?,?) 
") or die $dbh->errstr;

return $sth;
}

sub gethandstop{

my $sth= $dbh->prepare("select count(*)  from calls where met=0") or die $dbh->errstr;

return $sth;
}
sub gethandinslink{

my $sth= $dbh->prepare("insert into links values ('$appname',?,?)") or die $dbh->errstr;

return $sth;
}

sub gethandforminfo{

my $sth= $dbh->prepare("select formclass,formclassid,formseq,login from forms where formclass=? ") or die $dbh->errstr;

return $sth;
}

sub gethandinsform{

my $sth= $dbh->prepare("insert into forms (appname,formname,formclass,login,sourcepage) values ('$appname',?,?,?,?)  ") or die $dbh->errstr;

return $sth;
}

sub gethandinsformmore{

my $sth= $dbh->prepare("insert into forms (appname,formname,formclass,login,sourcepage,formclassid,formseq) values ('$appname',?,?,?,?,?,?)  ") or die $dbh->errstr;

return $sth;
}
sub gethandcountforms{

my $sth= $dbh->prepare("select count(*) from forms where formclass=? and sourcepage=?") or die $dbh->errstr;

return $sth;
}

sub gethandinsinput{

my $sth= $dbh->prepare("insert into forminputs (formclass,inputname,type,defval) values (?,?,?,?)") or die $dbh->errstr;

return $sth;
}

sub gethandmaxselect{

my $sth= $dbh->prepare("select max(selectid) from formselects") or die $dbh->errstr;

return $sth;
}

sub gethandinsformdata{

my $sth= $dbh->prepare("insert into formdata values ('$appname',? ,?,?,?)") or die $dbh->errstr;

return $sth;
}

sub gethandinsformselect{

my $sth= $dbh->prepare("insert into formselects (formclass,selectname,optionval) values (? ,?,?)") or die $dbh->errstr;

return $sth;
}

sub gethandupdformlogin{

my $sth= $dbh->prepare("update forms set login='y' where formclass=? and appname='$appname'") or die $dbh->errstr;

return $sth;
}

sub gethandupdformseq{

my $sth= $dbh->prepare("update forms set formseq=? where formclass=? and appname='$appname'") or die $dbh->errstr;

return $sth;
}

sub gethandupdformclassid{

my $sth= $dbh->prepare("update forms set formclassid=? where formclass=? and appname='$appname'") or die $dbh->errstr;

return $sth;
}

sub gethandtargetseq{

my $sth= $dbh->prepare("select distinct targetpage from requests where seq=? and appname='$appname'") or die $dbh->errstr;

return $sth;
}

sub gethandformclassidseq{

my $sth= $dbh->prepare("select distinct formclassid from forms where formseq=? and appname='$appname'") or die $dbh->errstr;

return $sth;
}

sub gethandseqs{

my $sth= $dbh->prepare("select distinct seq from requests where seq<>? and appname='$appname'") or die $dbh->errstr;

return $sth;
}

sub gethandtargetcounts{

my $sth= $dbh->prepare("select targetpage,count(*) c from requests where linkclass=? and appname='$appname' and sourcepage=? group by targetpage order by c") or die $dbh->errstr;

return $sth;
}

sub gethandtargetcount{

my $sth= $dbh->prepare("select count(*) from requests where targetpage=? and appname='$appname'") or die $dbh->errstr;

return $sth;
}

sub gethandlinkclass{

my $sth= $dbh->prepare("select distinct linkclass from requests where sourcepage=? and appname='$appname'") or die $dbh->errstr;

return $sth;
}

sub gethandformseq{

my $sth= $dbh->prepare("select distinct formseq from forms where formseq<>? and appname='$appname'") or die $dbh->errstr;

return $sth;
}

sub gethandmaxtarget{

my $sth= $dbh->prepare("select max(targetpage) from requests where appname='$appname'") or die $dbh->errstr;

return $sth;
}

sub gethandmaxformclassid{

my $sth= $dbh->prepare("select max(formclassid) from forms where appname='$appname'") or die $dbh->errstr;

return $sth;
}

sub gethandinsseqsim{

my $sth= $dbh->prepare("insert into seqsim (appname,seq1,seq2,lcss,sim) values ('$appname',?,?,?,?)") or die $dbh->errstr;

return $sth;
}
1;
