# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Copyright (c) 2010-2050, Nadia Alshahwan (nadia.alshahwan@gmail.com). All rights reserved. This program and the accompanying materials are made available under the terms of the 3-Clause BSD License which accompanies this distribution, and is available at http://www.opensource.org/licenses/BSD-3-Clause. 
# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

use Data::Random qw(:all);
use forminputs;
use DBinterface;
use HTML::TreeBuilder;

sub processform{
my $base_url = shift || die "no base url passed";
my $form = shift || die "no form is passed";
my $sourcepage = shift || die "no sourcepage passed";
my $formname = "";
my $formaction = "";
my $formseq = "";
my $formtree =HTML::TreeBuilder->new();
      $formtree = $form;

      $formname = $formtree->attr('name');
      if ($formname eq '')
       { $formname = $formtree->attr('id');

        }

#get action
      my $formaction = $formtree->attr('action');
      $formaction =~ s/\\n/ /g;

      $formaction =~ s/\'/ /g;

      my $submittype = $formtree->attr('method');
      $formseq = $formname.'-'.$formaction.'-'.$submittype.'-';

          #  my $temptree = HTML::TreeBuilder->new(); 
          #   $temptree->ignore_text(true);
          #   $temptree->parse($formtree->as_HTML())->eof;
             my @vals = $formtree->look_down('_tag','input','type',qr/(text|password|textarea)/);
             foreach (@vals)
              {$_->attr('value','');}
             my $plain_text=$formtree->as_HTML();
         #    $temptree->delete();
             my $formclass = md5_hex($plain_text);

#store in database

      $formaction = URI->new_abs($formaction, $base_url);

      my $exists = insertform($formclass,$sourcepage,$formname);

      if (!$exists)
        {my @inputs = $formtree->look_down('_tag','input');


 $formseq=$formseq.processinputs(\@inputs,$formclass).'-'; 
         
my @textarea = $formtree->look_down('_tag','textarea');
         if (@textarea)
          {$formseq=$formseq.processtextarea(\@textarea,$formclass).'-';} 
         

         my @selects = $formtree->look_down('_tag','select');
         if (@selects)
          {$formseq=$formseq.processselects(\@selects,$formclass).'-';} 
         $formseq =~ s/-$//g; 
         $formseq =~ s/-$//g;
         if ($formseq eq "--")
          {die "error $plain_text\n";} 
         updateformseq($formclass,$formseq);
         
         }      else { $formseq ="";}
#$formtree->delete();
#$form->delete();

return ($formaction,$submittype,$formclass,$formseq);   
    

}

sub randstring
{
   my $choice = rand(1);
	my $max= shift || die "no max length is passed";
	my @chars=('a'..'z','0'..'9','_');
	my $str;
	$max--;
        $choice = 0;
        if ($choice<0.5)
        { 
        foreach (1..$max) 
	{
		$str.=$chars[rand @chars];
	}
	}
        elsif ($choice < 0.90)
          { $str = round(rand(1000000));}
        elsif ($choice<0.95){ 
            foreach (1..1+round(rand(20))) 
	     {
		$str.=$chars[rand @chars];
	      }
               $str.='@';
            foreach (1..1+round(rand(20))) 
	     {
		$str.=$chars[rand @chars];
	      }
               $str.='.';
            foreach (1..2+round(rand(1))) 
	     {
		$str.=$chars[rand @chars];
	      }

             

            }else
             { my $date = rand_date();
               my @tmp = split /-/,$date;
               if (rand(1) <0.75)
                 { $str = @tmp[1].'/'.@tmp[2].'/'.@tmp[0];
                 }
               else{
                   $str = @tmp[1].'/'.@tmp[2].'/'.substr(@tmp[0],2);}
               }
      


return $str;
}

sub assignvalues{
    my $formclass = shift || die "no form class passed";
    my $p  = shift || die "no params hash passed";
    my $loginform = shift || die "no form type passed";
    my $inputname;
    my %params = %$p;
    my %Fields = ();

   
   for my $inputname ( keys %params ) {
    if ($params{$inputname} [0] eq 'password')
       {$l = round(rand(1));
         if ($login && $l)
          {$Fields{$inputname} = $password;}
        elsif($input eq 'empty')
          {$Fields{$inputname} ="";}
        elsif($input eq 'random')
          {$Fields{$inputname} =randstring(int(rand(10))+1);}
        elsif($input eq 'default')
         {$Fields{$inputname} = $params{$inputname} [1]; }
       }
    elsif ($params{$inputname} [0] eq 'checkbox')
       { $Fields{$inputname} = selectcheckbox($formclass,$inputname);}
    elsif($params{$inputname} [0] eq 'radio')
       { $Fields{$inputname} = selectradio($formclass,$inputname);}
    elsif($params{$inputname} [0] eq 'select')
       { $Fields{$inputname} = selectoption($formclass,$inputname);
         if ($Fields{$inputname} eq 'zero')
            {$Fields{$inputname} =0;}}
    elsif($params{$inputname} [0] eq 'text')
       { 
         if ($loginform eq 'y' && $login && $l)
          { $Fields{$inputname} = $username;}
        elsif($input eq 'empty')
          {$Fields{$inputname} ="";}
        elsif($input eq 'random')
          {$Fields{$inputname} =randstring(int(rand(20))+1);}
        elsif($input eq 'default')
         {$Fields{$inputname} = $params{$inputname} [1]; }
          } 
    elsif($params{$inputname} [0] eq 'submit')
       {$Fields{$inputname} = $params{$inputname} [1]; }
    elsif($params{$inputname} [0] eq 'textarea')
       {if($input eq 'empty')
          {$Fields{$inputname} ="";}
        elsif($input eq 'random')
          {$Fields{$inputname} =randstring(int(rand(20))+1);}
        elsif($input eq 'default')
         {$Fields{$inputname} = $params{$inputname} [1]; } }
    elsif($params{$inputname} [0] eq 'hidden')
       {$Fields{$inputname} = $params{$inputname} [1]; }
  }  
  
  return %Fields;  
}

sub prepareform{


my $formclass = shift || die "form class missing";


#get form fields 
my ($loginform,$f) = getformfields($formclass);
my %formfields = assignvalues($formclass,$f,$loginform);


return \%formfields;
}
1;
