#!/bin/perl

$dir = $ENV{'MIDTERM_MODEL_HOME'};
$call = "Rscript $dir\\dmi\\quantile.R";

#$numArgs = $#ARGV + 1;
#print "thanks, you gave me $numArgs command-line arguments.\n";

foreach $argnum (0 .. $#ARGV) {

   $call=$call." $ARGV[$argnum]";

}
#print "$call\n";
system "$call";