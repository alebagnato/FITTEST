# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Copyright (c) 2010-2050, Nadia Alshahwan (nadia.alshahwan@gmail.com). All rights reserved. This program and the accompanying materials are made available under the terms of the 3-Clause BSD License which accompanies this distribution, and is available at http://www.opensource.org/licenses/BSD-3-Clause. 
# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


use HTML::TreeBuilder;
use DBinterface;

sub processinputs{


my $inputs = shift || die "no inputs passed";
my $formclass = shift || die "no formclass passed";

my $type="";
my $input =HTML::TreeBuilder->new();
my $login = 'n';
my $inputseq = "";
my $name = "";
my $defval="";
foreach (@$inputs)
{  
 
    if ($_)
     {$input = $_;
     
      $type = $input->attr('type');

      $name = $input->attr('name');
      if ($name eq '')
        { $name = $input->attr('id');
         }
      
      $defval= $input->attr('value');
      
     
      insertinput($formclass ,$name,$type,$defval);

      $inputseq = $inputseq.$name.'-'.$type.'-';
      if (($type eq 'hidden' && !(exists $ignorehidden{$name} )) || $type eq 'radio' || $type eq 'checkbox' || $type eq 'submit')
        {$inputseq = $inputseq.$defval.'-';}

      if ($type eq "password")
        { if ($login eq 'y')
              {$login = 'n';}
          else
             {$login ='y';}}



      }

      
  if ($login eq 'y')
   { setformtologin($formclass);}

}
#$input->delete();

return $inputseq;
}

sub processtextarea{


my $textareas = shift || die "no textarea passed";
my $formclass = shift || die "no formclass passed";

my $type="textarea";
my $textarea =HTML::TreeBuilder->new();
my $textareaseq = "";
my $name = "";
my $defval="";
foreach (@$textareas)
{  
 
    if ($_)
     {$textarea = $_;
     
   
      $name = $textarea->attr('name');
      if ($name eq '')
        { $name = $textarea->attr('id');
         }
      
      $defval= $textarea->attr('value');
      
     
      insertinput($formclass ,$name,$type,$defval);

      $textareaseq = $textareaseq.$name.'-'.$type.'-';
    

      }

      


}


return $textareaseq;
}


sub processselects{

my $selects = shift;
my $formclass = shift;

my $select =HTML::TreeBuilder->new();
my $option =HTML::TreeBuilder->new();

my $selectseq = "";
foreach (@$selects)
{
    if ($_)
     {$select = $_;
         
     

      my $name = $select->attr('name');
      $selectseq = $selectseq.$name.'-';
      

      $name =~ s/\'/ /g;


      my @options= $select->look_down('_tag','option');

      foreach (@options)
       {
         if ($_)
        {$option = $_;


         my $optval = $option->attr('value');

         $optval =~ s/\'/ /g;
      
         if (!$optval && $optval == 0) {$optval = 'zeroval';}
         $selectseq=$selectseq.'option-';
         insertselect($formclass,$name,$optval);

          }
          }



      }

# $select->delete();
# $option->delete();    

}

return $selectseq;
}
1;
