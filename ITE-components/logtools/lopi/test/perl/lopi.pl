#!/usr/bin/perl

use strict;
use warnings;
use Switch;

#
# This script defines set of parameters and features that the lopi tool can provide 
#

# #################################################################################
# Configuration variables specific for each project 
 #################################################################################
# Since the tool provides three key features (inference, reduction and comparison), 
# there should be respectively three groups of options. From the GUI side it can be 
# seen as a combo-box which has three values: 'inference', 'reduction' and 'comparison'.
# Depending on the chosen value the corresponding group of options should become activated.
# #################################################################################


# the variable specifies in which instance the tool is going to be used
# its value can be 'inference', 'reduction' and 'comparison'
# Default: "" 
my $chooseTheTool = "comparison" ;

# -----------------------
# Inference Options
# -----------------------

# the folder containing logs that are to be used for rewrite rule inference
# Required
# Default: ""
my $logsFolderInfer = "./inference/" ;

# the variable contains those log files, residing in the folder $logsFolderInfer, that
# are to be used for the rewrite rule inference
# Required
# Default: ""
my @logFilesInfer = ("flexstore1.log", "flexstore2.log") ;

# the variable defines filtering strategy that is applied to the witness set resulted from the inference
# Default: ""
my $filterFunc = "";


# the variable defines the abstraction applied to the logs before inference
# Default: ""
my $absFunc = "" ; 

# the file containing rewrite rules resulted from the inference
# Required
# Default: ""
my $rewRulesInfer = "rules.pat" ;

# -----------------------
# Reduction Options 
# -----------------------


# the folder containing logs that are to be reduced
# Required
# Default: ""
my $logsFolderRew = "./reduction/" ;

# the set of rewrite rules (potentially resulted after inference) to be used for reduction
# Required
# Default: ""
my $rewRulesRew = "rules.pat" ;

# the variable contains those log files residing in the folder $logsFolderRew that
# are to be used reduced
# Required
# Default: ""
my @logFilesRew = ("flexstore1.log", "flexstore2.log") ;


# the logs resulted out of the reduction process
# Default: ""
my @logFilesOutRew = ("flexstore1.rlog", "flexstore2.rlog") ;

# -----------------------
# Comparison Group 
# -----------------------

# first input set of rewrite rules
# Required
# Default: ""
my $rewRulesCom1 = "./comparison/witSet1.out" ;

# second input set of rewrite rules
# Required
# Default: ""
my $rewRulesCom2 = "./comparison/witSet2.out" ;

# the set of rewrite rules resulted from the cross checking of the first two
# Required
# Default: ""
my $rewRulesRes = "./comparison/witSet.diff" ;

# ------------------------------------------
# Configuration vars which are global for whole ITE
# ------------------------------------------

# full path to the lopi program/tool:
my $lopi = "lopi" ;

# ------------------------------------------
# Some help functions
# ------------------------------------------

sub makeFiles {
    my @args = @_ ;
    my $folder = $args[0] ;
    my @files  = @args[1..$#args] ;
    my $res    = "" ;
    my $i ; 
    foreach $i (@files){
	$res = "$res" . "$folder".  "$i "
    }
    $res = substr($res, 0, -1) ;
    return "\"$res\"" ;
}

# ------------------------------------------
# The Main part, where the commands are 
# invoked with the set of generated options
# ------------------------------------------

switch($chooseTheTool){
    case "inference"  {system($lopi 
			      . " -i" 
			      . " --in-logs=" . makeFiles($logsFolderInfer, @logFilesInfer)			      
			      . " --rew-rules=" . $logsFolderInfer . $rewRulesInfer  
			      . " --filter-rules=" . $filterFunc 
			      . " --abs-logs=" . $absFunc)
    }
    case "reduction"  {system($lopi 
			      . " -r" . 
			      " --in-logs="  . &makeFiles($logsFolderRew, @logFilesRew) 		
			      . " --rew-rules=" . $rewRulesRew
			      . " --out-logs=" . &makeFiles($logsFolderRew, @logFilesOutRew))
    }
    case "comparison" {system($lopi
			      . " -c" 
			      . " --cross-check-rules=" . "\"" 
			      . $rewRulesCom1 . " " 
			      . $rewRulesCom2 . " " 
			      . $rewRulesRes . "\""
			   )
    }
    else              {print "the value of \$chooseTheTool is \"$chooseTheTool\", which should be either \"inference\" or \"reduction\" or \"comparison\".\n"}
}

