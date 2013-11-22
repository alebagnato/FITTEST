

use mydbi;


sub insertapp{


my $URL= shift || die "error passing parameter";
my $appname = shift || die "apname not passed";

my $sth = $dbh->prepare("select appid from applications where appurl = '$URL' and appname='$appname' ");

      $sth->execute() or die  $sth->errstr(); 
      my $exists = $sth->fetchrow_array();

      if ($exists>0)
        {return $exists;} 


      my $sth = $dbh->prepare("insert into applications (appurl,appname) values ('$URL','$appname');");

      $sth->execute() or die  $sth->errstr(); 


      $sth = $dbh->prepare('select LAST_INSERT_ID()');

      $sth->execute() or die  $sth->errstr();


      my ($id) = $sth->fetchrow_array();

 
      $sth->finish();


      return $id;
}

sub selectradio{


my $formclass = shift || die "form class missing";
my $inputname = shift || die "input name not passed";



      $sthdefval->execute($formclass,$inputname) or die  $sthdefval->errstr(); 
      my @values = ();
      while (my ($value) = $sthdefval->fetchrow_array())
       { if ($value)
         { 
           push(@values,$value);
          }

         }
      my $ind;
      if((@values-1) ==0)
       {$ind = 0;}
      else
       {$ind = round(rand(@values-1));}
      
      my $val =   @values[$ind];    
      
      return $val;

      


}


sub selectcheckbox{


my $formclass = shift || die "form class missing";
my $inputname = shift || die "input name not passed";


      $sthdefval->execute($formclass,$inputname) or die  $sthdefval->errstr(); 
      my @values = ();
      while (my ($value) = $sthdefval->fetchrow_array())
       { if ($value)
         { 
           push(@values,$value);
          }

         }
      
       push(@values,'');
      my $ind;
      if((@values-1) ==0)
       {$ind = 0;}
      else
       {$ind = round(rand(@values-1));}
      
      my $val =   @values[$ind];    
      
      return $val;
     
}

sub selectoption{


my $formclass = shift || die "form class missing";
my $inputname = shift || die "input name not passed";

my $sth = $dbh->prepare("select optionval from formselects where formclass= '$formclass'  and selectname='$inputname' ");

      $sthoptval->execute($formclass,$inputname) or die  $sthoptval->errstr();
      my $value;
      my @values = ();
      while (($value) = $sthoptval->fetchrow_array())
       {  if (defined $value)
           {push(@values,$value);}
          
         }
      my $ind;
      if((@values-1) ==0)
       {$ind = 0;}
      else
       {$ind = round(rand(@values-1));}
      
      my $val =   @values[$ind];    
      
      return $val;
}

sub getformfields{


my $formclass = shift || die "form class missing";

my %Fields = ();


      $sthinputs->execute($formclass) or die  $sthinputs->errstr(); 

      while (my @forminput = $sthinputs->fetchrow_array())
       { if (@forminput)
         { 
          $Fields{@forminput[0]} = [@forminput[1],@forminput[2]];}

         }
      
 


 
      $sthoptions->execute($formclass) or die  $sthoptions->errstr(); 
      

      while (my @formselects = $sthoptions->fetchrow_array())
       { if (@formselects)
         {$Fields{@formselects[0]} = ['select',@formselects[1]];}

         }

      
      $sthlogin->execute($formclass) or die  $sth->errstr(); 
      
      my ($loginform) = $sthlogin->fetchrow_array();
      
      return ($loginform,\%Fields);



}

sub insertrequest {

my $traceid = shift || die "traceid not passed";
    
    $sthmaxreqid->execute($traceid) or die  $sthmaxreqid->errstr();
    
    my ($requestid) = $sthmaxreqid->fetchrow_array();
    if ($requestid)
      {$requestid++;}
    else
      {$requestid = 1;}
    
    
    $sthinstrace->execute($traceid,$requestid) or die  $sthinstrace->errstr();
    
    return $requestid;
}


sub insertrequestdetails {
my $traceid = shift || die "trace id not passed";
my $requestid = shift || die "request id not passed";
my $URL = shift || die "url not passed";
my $responsecode = shift || die "response code not passed";
my $filename = shift || die "filename not passed";
my $linktype = shift || die "link type not passed";
my $targetpage = shift || die "target page not passed";
my $callingid = shift || die "source page not passed";
my $method = shift || die "method not passed";
my $formclass = shift || die "formclass not passed";
my $seq = shift;
   
    $sthinsreq->execute($traceid,$requestid,$URL,$method,$responsecode,$linktype,$filename,$targetpage,$callingid,$seq,$formclass) or die  $sthinsreq->errstr();
    
   
}

sub mincalls{


    $sthstop->execute()or die  $sthstop->errstr();
    
my ($calls) = $sthstop->fetchrow_array();
    

   return $calls;

}

sub savelink{
my $url = shift || die "url not passed";
my $sourcepage = shift || die "source page not passed";
  
    $sthinslink->execute($url,$sourcepage);
    

}
sub insertform{
my $formclass     = shift || die "formclass not passed";
my $sourcepage = shift || die "sourcepage not passed";
my $formname   = shift;


    $sthforminfo->execute($formclass) or die  $sthforminfo->errstr();

   my ($exists,$formid,$formseq,$login) = $sthforminfo->fetchrow_array();

   if (!$exists)
     {$login = 'n';
  
    $sthinsform->execute($formname,$formclass,$login,$sourcepage) or die  $sthinsform->errstr();

 }
  else {
    
    $sthcountforms->execute($formclass,$sourcepage) or die  $sthcountforms->errstr();

    
    my ($count) = $sthcountforms->fetchrow_array();
    
    if (!$count)
      { 
    $sthinsformmore->execute($formname,$formclass,$login,$sourcepage,$formid,$formseq) or die  $sthinsformmore->errstr();
}   

   }
   

    return $exists;
}


sub insertinput {


my $formclass = shift;
my $name   = shift;
my $type   = shift;
my $defval = shift;


$defval =~ s/\'//g;
$defval =~ s/\"//g; 


   
    $sthinsinput->execute($formclass,$name,$type,$defval) or die  $sthinsinput->errstr();

}

sub getnextselectid{


         
         $sthmaxselect->execute()  or die  $sthmaxselect->errstr(); 


         my ($selectid) = $sthmaxselect->fetchrow_array();

         $selectid++;

         
         return $selectid;    


}

sub savefields{

my $traceid = shift || die "trace id not passed";
my $requestid = shift || die "requestid not passed";
my $f = shift || die "fields not passed";
my %Fields = %$f;

my $field;
      for $field ( keys %Fields )
       {  
          $sthinsformdata->execute($traceid,$requestid,$field,$Fields{$field})  or die  $sthinsformdata->errstr(); 

         }

   
}

sub insertselect{
my $formclass =shift || die "form class not passed";
my $name   =shift || die "name not passed";
my $optval =shift || die "option value not passed";

if ($optval eq 'zeroval'){$optval = 0;}

         
         $sthinsformselect->execute($formclass,$name,$optval)  or die  $sthinsformselect->errstr(); 
         
}





sub setformtologin{

my $formclass = shift || die "missing parameter form class";

         $sthupdformlogin->execute($formclass) or die  $sthupdformlogin->errstr(); 
        

}

sub updateformseq{

my $formclass = shift || die "missing parameter form class";my $formseq = shift || die "missing parameter form seq";


        
         $sthupdformseq->execute($formseq,$formclass) or die  $sthupdformseq->errstr(); 
         

}

sub updateformclassid{

my $formclass = shift || die "missing parameter form class";my $formclassid = shift || die "missing parameter form seq";


         
         $sthupdformclassid->execute($formclassid,$formclass) or die  $sthupdformclassid->errstr(); 
         
}

sub insertuserinput{


my $inputid  = shift || die "no parameter";
my $inputvalue = shift;

$inputvalue =~ s/\s$//;

if (  $inputvalue ne '')

 { 
   my $sth = $dbh->prepare("insert into userinput (inputid,inputvalue) values ($inputid,'$inputvalue'); ");

   $sth->execute() or warn  $sth->errstr();
     $sth->finish();

}


}


sub seqexists{
my $seq = shift || die "seq not passed";



      $sthtargetseq->execute($seq) or die  $sthtargetseq->errstr();


      my ($class) = $sthtargetseq->fetchrow_array();
 
     

     return $class;

}

sub formseqexists{
my $seq = shift || die "seq not passed";



      $sthformclassidseq->execute($seq) or die  $sthformclassidseq->errstr();


      my ($class) = $sthformclassidseq->fetchrow_array();
 
 
     return $class;

}

sub getseqs{
my $seq = shift || die "seq not passed";


      $sthseqs->execute($seq) or die  $sthseqs->errstr();

      my @seqs = ();
      while (my ($seq) = $sthseqs->fetchrow_array())
       { if ($seq)
         { 
          push(@seqs,$seq);}

         }
      
 
    

     return @seqs;

}
sub getexpectedtarget{
my $link = shift || die "link not passed";
my $sourcepage = shift || die "sourcepage not passed";


      $sthtargetcounts->execute($link,$sourcepage) or die  $sthtargetcounts->errstr();

      my ($targetpage,$count) = $sthtargetcounts->fetchrow_array();
      
 
 
     return $targetpage;


}

sub getexpectedtargets{
my $sourcepage = shift || die "sourcepage not passed";


my $sth= $dbh->prepare("select linkclass,targetpage,count(*) c from requests where appname='$appname' and sourcepage=$sourcepage group by targetpage,linkclass order by c") or die $dbh->errstr;

 $sth->execute() or die  $sth->errstr();
my %expectedtargets=();
while (my @row = $sth->fetchrow_array())

{ if ( !(exists $expectedtargets{@row[0]}))
   {$expectedtargets{@row[0]} = @row[1];} 


}

$sth->finish();
return \%expectedtargets;

}

sub getexpectedformtarget{
my $formclass = shift || die "link not passed";
my $sourcepage = shift || die "sourcepage not passed";


my $sth= $dbh->prepare("select formclassid from forms where formclass='$formclass'") or die $dbh->errstr;

 $sth->execute() or die  $sth->errstr();

my ($formclassid) = $sth->fetchrow_array();


 $sth= $dbh->prepare("select targetpage,count(*) from requests where appname='$appname' and linkclass in (select formclass from forms where formclassid=$formclassid) and  sourcepage=$sourcepage group by targetpage") or die $dbh->errstr;

 $sth->execute() or die  $sth->errstr();

my ($targetpage,$count) = $sth->fetchrow_array();

$sth->finish();


     return $targetpage;


}

sub getexpectedtargetcount{
my $targetpage = shift || die "target page not passed";
#my $sourcepage = shift || die "sourcepage not passed";

      $sthtargetcount->execute($targetpage) or die  $sthtargetcount->errstr();

      my ($count) = $sthtargetcount->fetchrow_array();

#my $sth= $dbh->prepare("select count(*) from requests where targetpage=$targetpage and sourcepage=$sourcepage") or die $dbh->errstr;

 #$sth->execute() or die  $sth->errstr();

#my ($localcount) = $sth->fetchrow_array();
      
 
 #    return ($count,$localcount);
 return $count;

}
sub getvisitedlinks{
my $sourcepage = shift || die "seq not passed";


      $sthlinkclass->execute($sourcepage) or die  $sthlinkclass->errstr();

      my @links = ();
      while (my ($link) = $sthlinkclass->fetchrow_array())
       { if ($link)
         { 
          push(@links,$link);}

         }
      

     return @links;

}
sub getformseqs{
my $seq = shift || die "seq not passed";


      $sthformseq->execute($seq) or die  $sthformseq->errstr();

      my @seqs = ();
      while (my ($seq) = $sthformseq->fetchrow_array())
       { if ($seq)
         { 
          push(@seqs,$seq);}

         }
      
 

     return @seqs;

}

sub getseqclass{
my $seq = shift || die "seq not passed";


      $sthtargetseq->execute($seq) or die  $sthtargetseq->errstr();

      my ($class) = $sthtargetseq->fetchrow_array();
       

     return $class;

}

sub getformseqclass{
my $seq = shift || die "seq not passed";


      $sthformclassidseq->execute($seq) or die  $sthformclassidseq->errstr();

      my ($class) = $sthformclassidseq->fetchrow_array();
       

     return $class;

}
sub getnewclass{


      $sthmaxtarget->execute() or die  $sthmaxtarget->errstr();

      my ($class) = $sthmaxtarget->fetchrow_array();
     
      if ($class)
       {$class++;}
      else
       {$class=1;}
    
     return $class;

}

sub getnewformclassid{


      $sthmaxformclassid->execute() or die  $sthmaxformclassid->errstr();

      my ($class) = $sthmaxformclassid->fetchrow_array();
     
      if ($class)
       {$class++;}
      else
       {$class=1;}

     return $class;

}
sub savesim {
my $seq = shift || die "seq not passed";
my $seq1 = shift || die "seq 1 not passed";
my $LCSS = shift || die "LCSS not passed";
my $sim = shift || die "similarity measure not passed";

if ($sim == -1)
 {$sim = 0;}

    
    $sthinsseqsim->execute($seq,$seq1,$LCSS,$sim) or die  $sthinsseqsim->errstr();
    
    
   
}
sub formvisited{
my $formclass = shift || die "formclass not passed";
my $sourcepage = shift || die "sourcepage not passed";

my $sth= $dbh->prepare("select formclassid from forms where formclass='$formclass'") or die $dbh->errstr;

 $sth->execute() or die  $sth->errstr();

my ($formclassid) = $sth->fetchrow_array();

$sth= $dbh->prepare("select count(*) from requests where appname='$appname' and linkclass in (select formclass from forms where formclassid=$formclassid) and  sourcepage=$sourcepage") or die $dbh->errstr;

 $sth->execute() or die  $sth->errstr();

my ($count) = $sth->fetchrow_array();

$sth->finish();

return $count;

}
1;
