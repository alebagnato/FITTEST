# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Copyright (c) 2010-2050, Nadia Alshahwan (nadia.alshahwan@gmail.com). All rights reserved. This program and the accompanying materials are made available under the terms of the 3-Clause BSD License which accompanies this distribution, and is available at http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these results has received funding from the European Community`s Seventh Framework Programme (FP7/2007-2013) under the grant agreement  FP7-257574 FITTEST.
# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



        use URI;



sub getjavascriptlinks{

$content  = shift || die "parameter empty";
$base_url = shift || die "parameter empty";
$pattern  = shift || die "parameter empty";


my @jslinks = ();

$content = lc($content);

$content =~ s/\s+//g;


$content =~ s/\"/\'/g;


my $curpos= 0;

if ($pattern eq 'window.open(')
   {$endpattern = ')';}
else 
   {$endpattern = ';';}


while ( $curpos< length($content))
{
   $wpos = index ($content,$pattern,$curpos);
 
   if ($wpos==-1)
    { 
        return @jslinks;
        }     
    else  
     { 
      $spos = $wpos + length($pattern);
      $epos = index($content,$endpattern,$spos);

      $link = substr $content,$spos,$epos-$spos;
     # get rid of other parameters

      $comma = index($link,",",0); 
      if ($comma != -1)
        {$link = substr $link,0,$comma;}


     # get rid of quotes 
     if ($link =~ /^\'/  and $link =~ /\'$/)
     { 
       $link =~ s/\'//g;

     # ignore variables

 

      if ($link !~ m/@/ and $link !~ m/\.(jpg|jpeg|bmp|gif|mp3|css|js)$/ and $link =~ m/($filter)/ )
         { 
          # add base
          # add to array
          
           push(@jslinks,URI->new_abs($link, $base_url));
                     
         }
      }
      $curpos = $epos +1;
     } 
     

}
return @jslinks;
}
1;

