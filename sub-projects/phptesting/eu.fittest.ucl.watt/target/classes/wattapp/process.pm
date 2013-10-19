	use LWP::Simple;
	use LWP::UserAgent;
	use HTTP::Request;
	use HTTP::Response;
        use HTML::Parse;
        use HTML::FormatText;
	use HTML::LinkExtor; 
        use URI;
        use DBinterface;
        use HTML::TreeBuilder;
        use forms;
        use javascript;
        use mydbi;
        use URI::Escape;
        use Digest::MD5 qw(md5_hex);
        use Encode;
        use Math::Round;
        use HTML::TokeParser;
        use Algorithm::LCSS qw( LCSS CSS CSS_Sorted);
  my %links = ();
  my $filter;

  sub callback {

     my($tag, %tmplinks) = @_;
  #   return unless $tag eq 'a';
     return if $tag eq 'img';
   #  print "$tag,   @{[%tmplinks]}[1]\n";  
     return unless @{[%tmplinks]}[1] =~ m/($filter)/;
   
     return if @{[%tmplinks]}[1] =~ m/@/;
     return if @{[%tmplinks]}[1] =~ m/#/;
     return if $tag eq 'form';
     return if @{[%tmplinks]}[1] =~ m/\.(jpg|jpeg|bmp|gif|mp3|css|js|png)$/;
     $links{@{[%tmplinks]}[1]} = 1;
 #    push(@links,@{[%links]}[1]);
  }

sub getfilter{
my $URL = shift || die "no url";

if ($URL =~  m!http://!)
   {$URL= substr $URL,7;}


if ($URL =~ /\//)
{  
   my $last=rindex $URL, "\/";
   return substr $URL,0,$last+1; #url without file name
   
}

else
{  
    my $first = index $URL,'.';
    my $second= index $URL,'.',$first+1;
    if ($first == -1 && $second == -1)
       {return $URL;}
    else
       {return substr $URL,$first+1,$second-$first;} #significat part between www and domain
 }
}

sub processpage{
        my $requestid = shift || die "request id not passed";
        my $URL = shift || die "no url";
        my $callingid = shift || die "calling id not passed";
        my $traceid = shift || die "trace id not passed";
        $filter = shift || die "parameter error in process page";
        my $method = shift || die "method not passed";
        my $formclass = shift || die "form class not passed";
        my $f = shift || die "parameter error in process page";
        my %Fields = %$f;
        my @patterns = ('window.open(','window.location.href=');
        my @newlinks = ();
        %links = ();
        @link = ();
      
       my $response;
       # call url or form
         

            if ( $URL =~m/\?/)
               {$callURL = $URL."&wattappname=".$appname."&watttraceid=".$traceid."&wattrequestid=".$requestid;}
            else
               {$callURL = $URL."?wattappname=".$appname."&watttraceid=".$traceid."&wattrequestid=".$requestid;}
           if (%Fields)
              { $callURL = $callURL."&wattlinktype=form";}
        
          if (lc($method) eq 'get')
              {$response = $browser->get($callURL,\%Fields);}
            else
              {
               $response = $browser->post($callURL,\%Fields);}
            
       if($response->is_error())
        {print "\n", $response->status_line;}  
        # check if page has been visited before based on text difference
        my $seq = '';
        my $contents = $response->content();
        my $plain_text;
        my $md5_hash;

       # save html file to file system
        open (MYFILE, ">>$folder/$appname-$traceid-$requestid.html");
        print MYFILE $contents;
        close (MYFILE);

        if ($compare eq 'structure') 
            {
           #  my $temptree = HTML::TreeBuilder->new(); 
           #  $temptree->ignore_text(true);
           #  $temptree->parse($contents)->eof;
           #  @vals = $temptree->look_down('_tag','input','type',qr/(text|password|textarea)/);
           #  foreach (@vals)
           #   {$_->attr('value','');}
           #   $plain_text=$temptree->as_HTML();
           my $stream = HTML::TokeParser->new(\$contents);
           
           while (my $token = $stream->get_token){
              if ($token->[0] eq 'S' && @$token->[1] ne 'option' ){
                if ($token->[1] eq 'input')
                   {$seq = $seq.$token->[2]{'name'}.'-';}
                else
                   {$seq = $seq.@$token->[1].'-';}
               }
             
             } 
           $seq =~ s/-$//g; 
           if (!$seq)
            {$seq = '-';}

           my $class = seqexists($seq);
           if (!$class)
             {$class = calculatesim($seq); }
           
           $md5_hash = $class;
             
}
        else
             {my $temptree = HTML::TreeBuilder->new(); 

             $temptree->parse($contents)->eof;
             $plain_text=encode_utf8($temptree->as_text());
             
             $md5_hash = md5_hex($plain_text);
             $temptree->delete();
}



       
        my $linktype="";
        if (%Fields)
           {$linktype="form";}
        else
           {$linktype="link";}

 

        # insert new url to table
        insertrequestdetails($traceid,$requestid,$URL,$response->code( ),"$folder/$appname-$traceid-$requestid.html",$linktype,$md5_hash,$callingid,$method,$formclass,$seq);


        # process page by extracting all links and forms
        my $base = $response->base;

        my ($page_parser) = HTML::LinkExtor->new(\&callback,$base);
     
	$page_parser->parse($contents)->eof;
 #	@links = $page_parser->links;
        
# process forms - extract content

        my $tree = HTML::TreeBuilder->new(); 
        $tree->implicit_tags(0);
            $tree->parse($contents)->eof;
      
     
        my @forms = $tree->look_down("_tag","form");
           
        my @javascript = $tree->look_down("_tag","script");
        my @metas = $tree->look_down("_tag","meta");
        my $meta =HTML::TreeBuilder->new();

        my @metalinks = ();

           
        foreach (@metas)
        {
          if ($_)
           {  my $meta =$_;
              if($meta->attr('http-equiv') eq 'refresh')
              {  my $cont =$meta->attr('content');
                 my $ind0 = (index $cont,'URL')+4;
                 my $ind1 = index $cont,';',$ind0;
                 if ($ind1 == -1)
                   {$ind1 = length($cont);}
                 my $mlink= substr $cont,$ind0, ($ind1-$ind0);
                 $mlink =~ s/url=//;
                 push(@metalinks,URI->new_abs($mlink, $base));
             
                 }
              }
         }         


        my $metalinks = @metalinks;

        if ($metalinks >0)
         { foreach (@metalinks)
            { if ($_)
               { $links{$_} = 1;
                } }
           }
       $meta->delete();
       foreach my $pattern (@patterns)
        { 
          my @jslinks = getjavascriptlinks($response->as_string,$base,$pattern);
          my $jslinks = @jslinks;
          if ($jslinks >0)
           {foreach (@jslinks)
            { if ($_)
               { $links{$_} = 1;
                } }}
        }




      
     # choose to process a link or a form

        %Fields = ();
        @links = ();
        for my $tmpl (keys %links)
           { push (@links,$tmpl);}

        my %formlist = ();
        for (my $i=0; $i<@forms ; $i++)
  #      foreach  my $tmpform (@forms)
         { if (@forms[$i])
            {my ($tmpurl,$tmpmethod,$tmpformclass,$tmpformseq) = processform($base,@forms[$i],$md5_hash);

              if ($tmpformseq)
                {my $tmpformclassid=calculateformsim($tmpformseq);
                 updateformclassid($tmpformclass,$tmpformclassid);}
              if ($tmpurl && $tmpmethod && ($tmpurl =~ m/localhost/))
                {$formlist{$tmpformclass} = $i;}
              else
                 {delete @forms[$i];}
                } 
              
          }
        my %canidatelinks =();
        foreach my $l (@links)
         { if ($l)
            { my $tmpurl = processlink($l);
              savelink($tmpurl,$md5_hash);
              $canidatelinks{$tmpurl} = $l;}}
        my %canidatelinkstmp = %canidatelinks; 
        my $choice;
        if (@links && @forms)
          { $choice = rand(1); }
         elsif (@links)
            {$choice =0;}
         elsif (@forms)
            {$choice =1;}
         else {$nolinkpages{$md5_hash} = 'y';
               return @link;}
      my $requestid = insertrequest($traceid);
      my $formclass;
      my $ind;
      my $newurl;
        if ($choice <0.5)
         { 
           if (@links == 1)
            {$ind = 0;}
           else
            {@visitedlinks = getvisitedlinks($md5_hash);
             foreach $vl (@visitedlinks)
               { if (exists $canidatelinks{$vl})
                   { delete $canidatelinks{$vl};}}
             if (%canidatelinks)
               {@links = ();
                foreach my $k (keys %canidatelinks)
                  { push (@links,$canidatelinks{$k});}
            
                }

             
             
             if (@links ==1)
               {$ind = 0;}
             else
               {
             
             $ind = round(rand(@links-1));
                } }
        
           $newurl = @links[$ind];           
           $method = 'get';
           $formclass = processlink($newurl);
           
            }
        else
         {  my $index=-1;
            if (@forms == 1)
             {$index = 0;}
           else
             { my @tempforms = @forms;

              
               foreach my $f (keys %formlist)
                { if (formvisited($f,$md5_hash))
                    { delete @tempforms[$formlist{$f}];}
                 }
                if (@tempforms)
                  { @forms = ();
                    foreach $frm (@tempforms)
                     {if ($frm)
                      {push (@forms,$frm);} }
                   }
#                else
#                  { my %calls =();
                   
#                    foreach my $f (keys %formlist)
#                    { 
#                      my $expectedtarget = getexpectedformtarget($f,$md5_hash);
#                      my $targetcount = getexpectedtargetcount($expectedtarget);
#                      $calls{$formlist{$f}} = $targetcount;
                      
#                       }
#                      $ind = rand(1);
#                      my $p = 0;
#                      foreach my $i (keys %calls)
#                       { my $sum = 0;
#                         my $c = $calls{$i};
#                        foreach my $i2 (keys %calls)
#                        {  if ($i2 != $i)
#                          { $sum = $sum+ ($c/$calls{$i2});}
#                         }
 
#                        $p = $p + (1 /(1+ $sum));

#                       if ( $ind <= $p)
#                        { $index = $i;
#                          last;}    
#                         }} 

             }
           if ($index ==-1 && @forms !=1)
             {$index = round(rand(@forms-1));}
           elsif (@forms ==1)
             {$index = 0;}
           
           $newform = @forms[$index];
           
           ($newurl,$method,$formclass,$formseq) = processform($base,$newform,$md5_hash);

            if ($formseq)
              {my $formclassid=calculateformsim($formseq);
               updateformclassid($formclass,$formclassid);}

            my $fields = prepareform($formclass);
            %Fields = %$fields;
            savefields($traceid,$requestid,\%Fields);
           } 

        $newurl =~ s/#$//;
        if ($newurl =~ m/watt/)
           { my @tmp = split(/\?/,$newurl);
             my @tmp2 = split(/&/,@tmp[1]);
             $newurl = @tmp[0].'?';
             foreach (@tmp2)
              { if($_)
                { if ($_ !~ /watt/)
                    { $newurl = $newurl.$_.'&';}
                 }
               }
         }
        @link = ($newurl,$md5_hash,$method,$requestid,$formclass,\%Fields); 
       
$tree->delete; 
       return @link;

 
}

sub processlink{
my $l = shift || die "no link passed";

my @tmp = split /\?/,$l;
              my $tmpurl = @tmp[0];
              my @tmpurl2 = split /#/,$tmpurl;
              $tmpurl = @tmpurl2[0];      
                      
              if (@tmp[1])
               {my @params=();
                my @tmp2 = split /&/,@tmp[1];
                foreach (@tmp2)
                 { if ($_)
                   { my @tmp3 = split /=/,$_;
                     push (@params,@tmp3[0]);}
                  }
                @params = sort (@params);
                foreach (@params)
                 { if ($_)
                    {
                     $tmpurl = $tmpurl.'&'.$_;
                     }
                  }
                }
             

return $tmpurl;
}

sub calculateformsim{
my $seq = shift || die "seq not passed";
my @seq = split(/-/,$seq);

my $exists = formseqexists($seq);

if ($exists)
   {return $exists;} 
   
@seqs = getformseqs($seq);
my $class=0;
foreach $seq1 (@seqs){
   if ($seq1)
    { my @seq1 = split(/-/,$seq1);
     
      my $LCSSref = LCSS(\@seq, \@seq1);
      my $LCSS = join('-',@$LCSSref);
      if (!$LCSS)
        {$LCSS = '-';}    
      my $sim = 2*@$LCSSref/(@seq+@seq1);
      if ($sim > 0.98)
        {$class = getformseqclass($seq1);}
      if ($sim == 0)
       {$sim = -1;}
      savesim($seq,$seq1,$LCSS,$sim);

     }
}

if (!$class)
 { $class = getnewformclassid();}


return $class;

}
sub calculatesim{
my $seq = shift || die "seq not passed";
my @seq = split(/-/,$seq);

@seqs = getseqs($seq);
my $class = 0; 
 
foreach $seq1 (@seqs){
   if ($seq1)
    { my @seq1 = split(/-/,$seq1);
     
#      my $LCSSref = CSS_Sorted(\@seq, \@seq1);
#      my $LCSSsize=0;
#      my $LCSS = '';
      my $LCSSref = LCSS(\@seq, \@seq1);
      my $LCSS = join('-',@$LCSSref);

#      foreach  (@$LCSSref)
#       { my @CSS = @$_;
#         if (@CSS >= (@seq/3) && @CSS >= (@seq1/3))
#           { $LCSS = $LCSS. join('-',@CSS).'--';
#             $LCSSsize =+ @CSS;
#             } 
#        }
    #  my $LCSS = join('-',@$LCSSref);
      if (!$LCSS)
        {$LCSS = '-';}    
#      my $sim = 2*$LCSSsize/(@seq+@seq1);
      my $sim = 2*@$LCSSref/(@seq+@seq1);
     
      if ($sim > 0.98)
        {$class = getseqclass($seq1);}
      if ($sim == 0)
       {$sim = -1;}
      savesim($seq,$seq1,$LCSS,$sim);

     }
}

if (!$class)
 { $class = getnewclass();}


return $class;
}
1;
