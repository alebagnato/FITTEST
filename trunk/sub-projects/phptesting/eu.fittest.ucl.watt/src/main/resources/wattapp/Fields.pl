# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Copyright (c) 2010-2050, Nadia Alshahwan (nadia.alshahwan@gmail.com). All rights reserved. This program and the accompanying materials are made available under the terms of the 3-Clause BSD License which accompanies this distribution, and is available at http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these results has received funding from the European Community`s Seventh Framework Programme (FP7/2007-2013) under the grant agreement  FP7-257574 FITTEST.
# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

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