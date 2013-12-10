use mydbi;
use DBinterface;

my $dbh =mydbi::getconnection();

my @formids = DBinterface::getformids($dbh);


foreach $formid (@formids)
 { if ($formid)
    {  my $fields = DBinterface::getenumoffields($dbh,$formid);
      
       my $unbounded = DBinterface::getunbounded($dbh,$formid);

       print "formid $formid, fields $fields, unbounded $unbounded\n";
       DBinterface::updateFieldNums($dbh,$formid,$fields+1,$unbounded+1);



    }

  }