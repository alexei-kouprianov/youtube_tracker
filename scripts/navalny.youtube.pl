#!/usr/bin/env perl
use warnings;
use LWP::Simple;
use utf8;
use strict;

# Start your script below this line;

# Fetcher block

my $sec;
my $min;
my $hour;
my $mday;
my $mon;
my $year;
my $wday;
my $yday;
my $isdst;

open (SRC00, '</home/tinea/Documents/H_et_S/Projects/github/youtube_tracker/scripts/urllist.txt') or die $!;
open (TGT00, '>>/home/tinea/Documents/H_et_S/Projects/github/youtube_tracker/downloads/pages.raw.txt') or die $!;
while(<SRC00>){
	print TGT00 get($_);
	}
close TGT00;
close SRC00;

# Gratia tibi ago, https://stackoverflow.com/questions/1814196/quickly-getting-to-yyyy-mm-dd-hhmmss-in-perl

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time);

my $now = sprintf("%04d-%02d-%02d %02d:%02d:%02d", $year+1900, $mon+1, $mday, $hour, $min, $sec);

# Extractor block

open (SRC01, '</home/tinea/Documents/H_et_S/Projects/github/youtube_tracker/downloads/pages.raw.txt') or die $!;
open (TGT01, '>>/home/tinea/Documents/H_et_S/Projects/github/youtube_tracker/downloads/pages.processed.01.txt') or die $!;
	while (<SRC01>) {
		s/ \- YouTube//g;
		s/(?=\"LIKE\")|(?=\"DISLIKE\")/\n/g;
		print TGT01 $_;
		}
close SRC01;
close TGT01;

my $title;
my $views;
my $likes;
my $dislikes;

open (SRC02, '</home/tinea/Documents/H_et_S/Projects/github/youtube_tracker/downloads/pages.processed.01.txt') or die $!;
open (TGT02, '>>/home/tinea/Documents/H_et_S/Projects/github/youtube_tracker/downloads/pages.processed.02.txt') or die $!;
	while (<SRC02>) {
		s/(?<=[0-9])\xc2\xa0(?=[0-9][0-9][0-9])//g;
			if($_ =~ m/^.*\<title\>(.*?)\<\/title\>.*/a){
			print TGT02 "$now\t$1\t";
			}
			elsif($_ =~ m/^.*\{\"viewCount\"\:\{\"simpleText\"\:\"(\d.*?) \D*\"\}.*/a){
			print TGT02 $1,"\t";
			}
			elsif($_ =~ m/^.*\"LIKE\"\}\,.*?\{\"accessibilityData\"\:\{\"label\"\:\"(\d.*?) \D*\"\}\.*}/a){
			print TGT02 $1,"\t";
			}
			elsif($_ =~ m/^.*\"DISLIKE\"\}\,.*?\{\"accessibilityData\"\:\{\"label\"\:\"(\d.*?) \D*\"\.*}\}/a){
			print TGT02 $1;
			}
			elsif($_ =~ m/^.*\<\/html\>.*/a){
			print TGT02 "\n";
			}
		}
close SRC02;
close TGT02;



my @files_to_remove = qw/\/home\/tinea\/Documents\/H_et_S\/Projects\/github\/youtube_tracker\/downloads\/pages.raw.txt \/home\/tinea\/Documents\/H_et_S\/Projects\/github\/youtube_tracker\/downloads\/pages.processed.01.txt/;

unlink @files_to_remove;
