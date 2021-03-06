# Compare.pm
#
# Time-stamp: <Sat Jun 24 21:53:25 CDT 2017 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>,  4 May 2016
#

#----------------------------------------------------------------
# Description:
# Compare two EKBs

#----------------------------------------------------------------
# History
# 2016/05/04 v0.1	lgalescu
# - Created.
# 2016/05/05 v1.0	lgalescu
# - First working version.
# 2016/05/07 v1.1	lgalescu
# - Fixed bugs, other updates.
# 2016/05/16 v1.2	lgalescu
# - Fully converted to OO interface.
# 2016/05/18 v1.2.1	lgalescu
# - Fixed bug
# 2016/05/26 v1.3	lgalescu
# - Updated to use new OO interface for EKB.pm.
# 2016/05/26 v1.3	lgalescu
# - Updated to use new OO interface for EKB.pm.
# 2016/06/02 v1.4	lgalescu
# - Added option to ignore the predicate type.
# 2016/06/06 v1.5	lgalescu
# - Added option to skip normalization (saves time on big EKBs).
# - Added span defaults when :start/:end are missing.
# - Fixed bugs (corner cases).
# 2016/07/07 v1.6	lgalescu
# - Added ability to perform ont checks (allows match when one type is-a
#   the other). Added option to control this behavior.
# 2016/09/17 v1.7	lgalescu
# - Added assertion counts to summary
# 2016/09/18 v2.0	lgalescu
# - Fixed use of deep matching option, which was buggy.
# - Now using deep matching option for all proper args.
# 2017/02/03 v2.0.1	lgalescu
# - Adapted to work as part of EKB TRIPS component
# 2017/03/04 v2.0.2	lgalescu
# - Fixed: issue causing an uninitialized value warning in &cmp_args.
# 2017/03/24 v2.1.0	lgalescu
# - Added option to ignore uttnum diffs (assumes they are identical even
#   when they're not).
# 2017/04/21 v2.1.1	lgalescu
# - Fixed bug.
# 2017/04/28 v2.1.2	lgalescu
# - Synced w/ EKB.pm interface.
# 2017/05/17 v2.2.0	lgalescu
# - Code cleanup.
# - Fixed a bug.
# - Added possibility of comparing "ungrounded" EKBs (no text).
# - Changed representation of diffs. 
# 2017/05/25 v2.2.1	lgalescu
# - Updated for new format of inevent features. For now, this is backwards
#   compatible.
# 2017/06/04 v2.3.0	lgalescu
# - Added a number of set utilities.
# - Changed default dbid comparison to use set overlap rather than strict equality.
# 2017/06/11 v2.4.0	lgalescu
# - Now keeping track of some diff details.
# - Diff output includes highlights (summary indicators of where diffs are).
# - Fixed some bugs.
# - Fixed order of diffs in output so deletions tend to appear before insertions.
# 2017/06/13 v2.4.1	lgalescu
# - Added test for features in events.
# 2017/06/24 v2.4.2	lgalescu
# - Added precision/recall to summary

# TODO (in order of importance and/or urgency):
# - try to find node "substitutions"
# - profile: it can be slow on big EKBs!
# - there are quite a few FIXMEs and TODOs below
# - improve how diffs are set and shown
#   - one easy solution: add trackers to EKB elements!
# - consider whether it might be appropriate to add a similarity score 
#   and obtain the best alignment through global optimization over x_map

package EKB::Compare;

$VERSION = '2.4.2';

use strict 'vars';
use warnings;

use Data::Dumper;
use Algorithm::Diff qw(sdiff);
use List::Util qw(min max all any first uniq);
use XML::LibXML;
use Test::Deep::NoTest qw(eq_deeply set subsetof);

use EKB;
use EKB::Match;
use Ont::BioEntities;
use Ont::BioEvents;
use util::Log;

our @Options = qw/normalize
		  ignore_text
		  same_uttnums
		  ignore_dbid
		  ignore_pred_type
		  attributes
		  uttnums
		  strict_ont_match/;

# note about extraction mappings ($self->{x_map})
# idea:
#  $id1 ~~ $id2 iff $x_map{ref}{$id1}{$id2}==1 [=> $x_map{hyp}{$id2}{$id1}==1]
#  $id1 !~ $id2 iff $x_map{ref}{$id1}{$id2}==0 [=> $x_map{hyp}{$id2}{$id1}==0]

# bio ontologies
my $ont_events = Ont::BioEvents->new();
my $ont_bioents = Ont::BioEntities->new();


### constructor

sub new {
  my $class = shift;
  if (@_ < 2) {
    FATAL "Constructor EKB::Compare->new() must have at least two arguments";
  }
  my ($ekb1, $ekb2, @call_opts) = @_;
  # some checks
  my $valid = eval { $ekb1->isa("EKB") && $ekb2->isa("EKB") };
  if (!$valid) {
    ERROR "Failed to create %s object%s.", $class, ($@ ? " ($@)" : "");
    return undef;
  }
  if (@call_opts % 2) {
    FATAL "Options must be name=>value pairs. Got: %s", Dumper(\@call_opts);
  }
  # options: set defaults
  my %options = _default_options();
  # ... and read the caller-defined ones
  my %opts = @call_opts;
  while (my($key, $val) = each %opts) {
    my $lkey = lc($key);
    ERROR "Unrecognised option: $key"
      unless (any {$_ eq $lkey} @Options);
    $options{$lkey} = $val;
  }
  DEBUG 2, "options at init: %s", Dumper(\%options);
  my $self = {
	      # ekb1: reference EKB; a EKB object
	      ekb1 => $ekb1,
	      # ekb2: test EKB; a EKB object
	      ekb2 => $ekb2,
	      # options: options for compare()
	      options => \%options,
	      # result: 1 = match, 0 = no match
	      result => undef,
	      # diffs: structure w/ detailed diffs
	      diffs => { attributes => undef,
			 sentences => undef,
			 assertions => undef,
		       },
	      # summary at assertion level
	      summary => { ref => 0, # count in ekb1
			   hyp => 0, # count in ekb2
			   eql => 0, # count of assertions common to ekb1 and ekb2
			   del => 0, # count of assertions unique in ekb1
			   ins => 0, # count of assertions unique in ekb2
			   sub => 0, # count of assertions unique in ekb2
			   p => 0.0, # precision = (eql/hyp)
			   r => 0.0, # recall = (eql/ref)
			 },
	      # sentence mapping from ekb1 to ekb2
	      s_map => {},
	      # EKB assertion mapping from ekb1 to ekb2 (sparse crosstab)
	      x_map => {}
	     };
  bless $self, $class;
}


### getters and setters

sub _default_options {
  return
    ( normalize => 1,
      ignore_text => 0,
      same_uttnums => 0,
      ignore_dbid => 0,
      ignore_pred_type => 0,
      strict_ont_match => 0,
      attributes => [],
      uttnums => [],
    );
}

# get/set options in bulk
sub options {
  my $self = shift;
  if (@_) {
    my %opts = @_;
    while (my ($opt, $val) = each(%opts)) {
      $self->option($opt, $val);
    }
  }
  my @opts = %{ $self->{options} }
    if $self->{options};
  return @opts;
}

# get/set individual option
sub option {
  my $self = shift;
  return __option($self->{options}, @_);
}

sub ekb1 {
  my $self = shift;
  return $self->{ekb1};
}

sub ekb2 {
  my $self = shift;
  return $self->{ekb2};
}

sub result {
  my $self = shift;
  if (@_) {
    $self->{result} = shift;
  }
  return $self->{result};
}

sub diffs {
  my $self = shift;
  return $self->{diffs};
}

sub summary {
  my $self = shift;
  return $self->{summary};
}


### comparison between two EKBs (main function of this package/class)

# note: this returns 1 if EKBs match, 0 otherwise
sub compare {
  my $self = shift;

  if ($self->option('normalize')) {
    $self->ekb1()->normalize();
    $self->ekb2()->normalize();
  }

  if ($self->option('uttnums')) {
    $self->ekb1()->filter({ sentences => $self->option('uttnums') });
  }

  my @tests =
    (
     # ekb meta info
     $self->cmp_ekb_attributes(),
     
     # inputs (by sentence)
     $self->option('ignore_text')
     || $self->cmp_ekb_input(),

     # EKB assertions (by sentence)
     $self->cmp_ekb_assertions()
    );
  
  $self->result(0+min(@tests));
}


### function for matching EKB attributes

sub cmp_ekb_attributes {
  my $self = shift;

  my @attrs = @{$self->option('attributes')};
  return 1 unless @attrs;
  return min( map {$self->cmp_ekb_attribute($_)} @attrs );
}

sub cmp_ekb_attribute {
  my $self = shift;
  my ($attr) = @_;

  my $v1 = $self->ekb1()->get_attr($attr);
  my $v2 = $self->ekb2()->get_attr($attr);
  return 1 if __assert_eq($v1, $v2);
  $self->add_diff('attributes', { attr => $attr,
				  ref => $v1 // "",
				  hyp => $v2 // "" });
  0;
}


### function for matching inputs (sentence mapping)

sub cmp_ekb_input {
  my $self = shift;

  my @sentences1 = $self->ekb1()->get_sentences();
  DEBUG 1, "REF sentences: %s", @sentences1;
  if (0) { # obsolete -- now we filter in advance (see &compare)
    # apply only_uttnums filter
    my $uttnums = $self->option('uttnums');
    my $have_uttnums = (scalar(@$uttnums) > 0);
    if ($have_uttnums) {
      @sentences1 =
	grep { my $id = $_->getAttribute('id');
	       first { $id eq $_} @$uttnums }
	@sentences1;
      DEBUG 2, "Doing only %s", "@sentences1";
    }
  }
  # are we assuming sentences are the same?
  if ($self->option('same_uttnums')) {
    foreach my $s (@sentences1) {
      my $sid1 = $s->getAttribute('id');
      $self->{'s_map'}{$sid1} = $sid1;
    }
    return 1;
  }
    
  my @sentences2 = $self->ekb2()->get_sentences();
  DEBUG 1, "HYP sentences: %s", "@sentences2";
  # TODO: should compare text from paragraph instead!
  my @s_texts1 = map { $_->textContent } @sentences1;
  my @s_texts2 = map { $_->textContent } @sentences2;
  my @diffs = sdiff( \@s_texts1, \@s_texts2 );
  DEBUG 3, "sentence diffs: %s", Dumper(\@diffs);
  my ($i1, $i2) = (0, 0);
  my $result = 1;
  foreach my $diff (@diffs) {
    DEBUG 3, "diff: %s", Dumper($diff);
    my $d = $diff->[0];
    my ($sid1, $sid2);
    if ($i1 < scalar(@sentences1)) {
      $sid1 = $sentences1[$i1]->getAttribute('id');
    }
    if ($i2 < scalar(@sentences2)) {
      $sid2 = $sentences2[$i2]->getAttribute('id');
    }
    if ($d eq 'u') { # unchanged
      $self->{'s_map'}{$sid1} = $sid2;
      $i1++; $i2++;
    } elsif ($d eq '-') { # deletion
      $self->add_diff('sentences', { ref => $sid1 });
      $i1++;
      $result = 0;
    } elsif ($d eq '+') { # insertion
      if ($self->option('uttnums')) {
	# ignore if we restrict to specific uttnums
	# TODO: should still include insertions between matches!
      } else {
	$self->add_diff('sentences', { hyp => $sid2 });
	$result = 0;
      }
      $i2++;
    } elsif ($d eq 'c') { # changed
      $self->add_diff('sentences', { ref => $sid1,
				     hyp => $sid2 });
      $i1++;
      $i2++;
      $result = 0;
    }
  }
  DEBUG 2, "cmp_ekb_input => $result";
  $result;
}


### function for matching all EKB assertions

# N.B.: assertions are compared only for matching sentences
sub cmp_ekb_assertions {
  my $self = shift;

  DEBUG 1, "s_map: %s", Dumper($self->{'s_map'});

  # sentences to work on
  my $s_map = $self->{'s_map'};
  my @uttnums = keys(%$s_map);

  if ((! $self->option('ignore_text')) and @uttnums) {
    all { $_ == 1 }
      map { $self->cmp_ekb_assertions_for_utt($_) }
      sort { $a <=> $b }
      @uttnums;
  } else {
    $self->cmp_ekb_assertions_all();  
  }
}

### match all EKB assertions
sub cmp_ekb_assertions_all {
  my $self = shift;

  my @items1 = $self->ekb1()->get_assertions();
  my @items2 = $self->ekb2()->get_assertions();

  my $count1 = scalar(@items1);
  my $count2 = scalar(@items2);

  $self->summary->{ref} += $count1;
  $self->summary->{hyp} += $count2;
  
  { 
    local $Data::Dumper::Terse = 1;
    local $Data::Dumper::Indent = 0;
    DEBUG 1, "comparing assertions: n=<%d, %d>", $count1, $count2;
  }
  
  # $self->reset_x_map();
  my $result = $self->cmp_ekb_items(\@items1, \@items2);
    
  # items unique to ekb1
  my @uniq1 =
    grep { ! $self->has_match($_, 'ref') }
    @items1;
  foreach my $i1 (@uniq1) {
    DEBUG 1, "Deletion: %s ", $i1->getAttribute('id');
    $self->add_diff('assertions', { ref => $i1->getAttribute('id') });
    $result = 0;
  }
  my $deletions = scalar(@uniq1);
  $self->summary->{del} += $deletions;
  $self->summary->{eql} += ($count1 - $deletions);

  # items unique to ekb2
  my @uniq2 =
    grep { ! $self->has_match($_, 'hyp') }
    @items2;
  foreach my $i2 (@uniq2) {
    DEBUG 1, "Insertion: %s ",$i2->getAttribute('id');
    $self->add_diff('assertions', { hyp => $i2->getAttribute('id') });
    $result = 0;
  }
  $self->summary->{ins} += scalar(@uniq2);

  # precision and recall
  $self->summary->{p} =
    $self->summary->{hyp} ? ($self->summary->{eql} / $self->summary->{hyp}) : 0.0;
  $self->summary->{r} =
    $self->summary->{ref} ? ($self->summary->{eql} / $self->summary->{ref}) : 0.0;
 
  # done!
  {
    DEBUG 3, "x_map:\n %s", Dumper($self->{'x_map'});
  }
    
  $result;
}

### match all EKB assertions for a sentence

# sid1: sentence id in ekb1
sub cmp_ekb_assertions_for_utt {
  my $self = shift;
  my ($sid1) = @_;

  my $sid2 = $self->{'s_map'}{$sid1};

  my @items1 =
    sort { _span_cmp(_get_span($a), _get_span($b)) }
    $self->ekb1()->get_assertions({ uttnum => $sid1 });
  my @items2 =
    $self->ekb2()->get_assertions({ uttnum => $sid2 });

  my $count1 = scalar(@items1);
  my $count2 = scalar(@items2);

  $self->summary->{'ref'} += $count1;
  $self->summary->{'hyp'} += $count2;
  
  { 
    local $Data::Dumper::Terse = 1;
    local $Data::Dumper::Indent = 0;
    DEBUG 1, "comparing assertions for utt pair <%s, %s>: n=<%d, %d>",
      $sid1, $sid2, $count1, $count2;
  }
  
  # $self->reset_x_map();
  my $result = $self->cmp_ekb_items(\@items1, \@items2);

  # items unique to ekb1
  my @uniq1 =
    grep { ! $self->has_match($_, 'ref') }
    @items1;
  foreach my $i1 (@uniq1) {
    my $span1 = _get_span($i1);
    # susbstitution?
    
    
    DEBUG 1, "Deletion: [%d, %d] %s ", $span1->[0], $span1->[1], $i1->getAttribute('id');
    $self->add_diff('assertions', { ref => $i1->getAttribute('id'),
				    sid => $sid1,
				    start => $span1->[0],
				    end => $span1->[1] });
    $result = 0;
  }
  my $deletions = scalar(@uniq1);
  $self->summary->{del} += $deletions;
  $self->summary->{eql} += ($count1 - $deletions);

  # items unique to ekb2
  my @uniq2 =
    sort { _span_cmp(_get_span($a), _get_span($b)) }
    grep { ! $self->has_match($_, 'hyp') }
    @items2;
  foreach my $i2 (@uniq2) {
    my $span2 = _get_span($i2);
    DEBUG 1, "Insertion: [%d, %d] %s ", $span2->[0], $span2->[1], $i2->getAttribute('id');
    $self->add_diff('assertions', { hyp => $i2->getAttribute('id'),
				    sid => $sid1,
				    start => $span2->[0],
				    end => $span2->[1] });
    $result = 0;
  }
  $self->summary->{ins} += scalar(@uniq2);

  # precision and recall
  $self->summary->{p} =
    $self->summary->{hyp} ? ($self->summary->{eql} / $self->summary->{hyp}) : 0.0;
  $self->summary->{r} =
    $self->summary->{ref} ? ($self->summary->{eql} / $self->summary->{ref}) : 0.0;
  
  # done!
  {
    DEBUG 3, "x_map:\n %s", Dumper($self->{'x_map'});
  }
    
  $result;
}


### generic function for matching two lists of EKB items

# TODO: some form of global optimization. for this i'd have to consider scoring
# matches (just match or no-match is not enough)
sub cmp_ekb_items {
  my $self = shift;
  my ($list1, $list2, $options) = @_;

  my $result = 1;

  # the following is an optimization that's expensive, and has not
  # demonstrated (yet) any benefit, so i'm not using it 
  if (0) {
    # to maximize chances we're getting the easy ones right,
    # we proceed in the order of
    # min{_span_dist(a,b) ; b \in L2}, where a \in \L1
    my @spans2 =  map { _get_span($_) } @$list2;

    my @sorted_list1 =
      map { $list1->[$_->[0]] }
      sort { $a->[1] <=> $b->[1] }
      map { my $i = $_;
	    my $span1 = _get_span($list1->[$i]);
	    [ $i, min (map { _span_distance($span1, $_) } @spans2) ]
	  }
      0..$#$list1;

    DEBUG 2, "sorted: %s", "@sorted_list1";
  }
    
  # we keep track here of temporary matches in @$list2 (ids)
  my %tmp_matched2 = ();
  foreach my $i1 (@$list1) {
    my $span1 = _get_span($i1);
    my $x_type = $i1->nodeName;
    my @candidates =
      # sort candidates in span-match order
      sort { _span_distance($span1, _get_span($a))
	     <=>
	     _span_distance($span1, _get_span($b)) }
      # select candidates with matching spans
      grep { _span_overlap($span1, _get_span($_)) == 1 }
      # eliminate candidates we matched already in @$list1
      grep { ! exists $tmp_matched2{$_->getAttribute('id')} }
      # eliminate candidates we know don't match
      grep { ! $self->not_matched($i1, $_) }
      # eliminate incompatible types
      grep { $x_type eq $_->nodeName }
      @$list2;

    {
      local $Data::Dumper::Terse = 1;
      local $Data::Dumper::Indent = 0;
      my @ids2 =
	map {[$_->getAttribute('id'), _get_span($_)]} @candidates;
      DEBUG 2, "span1: [%d,%d] %s %s => %s",
	$span1->[0], $span1->[1],
	$x_type, $i1->getAttribute('id'), Dumper(\@ids2);
      DEBUG 3, "candidates for %s:\n%s",
	$i1->getAttribute('id'), "@candidates";
    }
	
    # look for a match
    my $match =
      first { $self->cmp_ekb_item($i1, $_, $options) }
      @candidates;
    if ($match) {
      # keep track of this id
      # TODO: optimization: if item has a match, no need to add it
      $tmp_matched2{$match->getAttribute('id')} = 1;
    } elsif (__option_is($options, 'deep', 0)) {
      # short-circuit if we're not interested in a deep match
      return 0;
    } else {
      $result = 0;
    }
  }
  DEBUG 2, "cmp_ekb_items ==> %s", $result;
  $result;
}


### generic function for matching two EKB items

# TODO: if this is called from a different package, we must set %_x_map!
sub cmp_ekb_item {
  my $self = shift;
  my ($i1, $i2, $options) = @_;
    
  # shortcut if we'd looked at any of this before
  return 1 if $self->matched($i1, $i2);
  return 0 if ($self->has_match($i1, 'ref')
	       || $self->has_match($i2, 'hyp')
	       || $self->not_matched($i1, $i2));

  {
    DEBUG 2, "matching [deep=%s]: %s %s ~~ %s",
      __option($options, 'deep'),
      $i1->nodeName,
      $i1->getAttribute('id'), $i2->getAttribute('id');
  }
    
  my $aname = $i1->nodeName;

  return 0 unless ($aname eq $i2->nodeName);

  return $self->cmp_TERM($i1, $i2, $options) if ($aname eq TERM);
  return $self->cmp_EVENT($i1, $i2, $options) if ($aname eq EVENT);
  return $self->cmp_EPI($i1, $i2, $options) if ($aname eq EPI);
  return $self->cmp_CC($i1, $i2, $options) if ($aname eq CC);
  return $self->cmp_MODALITY($i1, $i2, $options) if ($aname eq MODALITY);
}


### functions for matching extractions

# TODO: add drum-terms?
sub cmp_TERM {
  my $self = shift;
  my ($i1, $i2, $options) = @_;

  __assert_eq($i1->nodeName, "TERM") && __assert_eq($i2->nodeName, "TERM")
    or return 0;
  
  # overlap test; if this fails, we bail
  my $result = _span_overlap(_get_span($i1), _get_span($i2));

  # contents test
  my %tests = ();
  if ($result) {
    # type test; the only one performed for shallow matches
    $tests{'type'} = $self->cmp_ont_slot($i1, $i2, 'type');
  
    unless (__option_is($options, 'deep', 0)) {
      $tests{'dbid'} = $self->cmp_attribute_aslist($i1, $i2, 'dbid', '|')
	unless $self->option('ignore_dbid');
      $tests{'name'} = $self->cmp_text_slot($i1, $i2, 'name');
      $tests{'mods'} = $self->cmp_mods($i1, $i2);
      $tests{'features'} = $self->cmp_features($i1, $i2);
      $tests{'mutation'} = $self->cmp_mutation($i1, $i2);
      $tests{'aggregate'} = $self->cmp_aggregate($i1, $i2);
      $tests{'components'} = $self->cmp_components($i1, $i2);
      $tests{'members'} = $self->cmp_members($i1, $i2);
    }
  }
    
  $result &&= min(values(%tests));
  DEBUG 3, "result($i1,$i2,deep=%s) ==> %s", __option($options,'deep'), $result;
  DEBUG 3, "tests ==> %s", Dumper(\%tests);
  $self->record_result($i1, $i2, $result, \%tests)
      unless (__option_is($options, 'deep', 0) and $result);
  DEBUG 2, "==> %s", $result;
  $result;
}

sub cmp_EVENT {
  my $self = shift;
  my ($i1, $i2, $options) = @_;

  __assert_eq($i1->nodeName, "EVENT") && __assert_eq($i2->nodeName, "EVENT")
    or return 0;
    
  # overlap test; if this fails, we bail
  my $result = _span_overlap(_get_span($i1), _get_span($i2));
  
  # contents test
  my %tests = ();
  if ($result) {
    # type test; the only one performed for shallow matches
    $tests{'type'} = $self->cmp_ont_slot($i1, $i2, 'type');

    unless (__option_is($options, 'deep', 0)) {
      $tests{'negation'} = $self->cmp_text_slot($i1, $i2, 'negation');
      $tests{'polarity'} = $self->cmp_text_slot($i1, $i2, 'polarity');
      $tests{'modality'} = $self->cmp_text_slot($i1, $i2, 'modality');
      $tests{'epistemic-modality'} 
	= $self->cmp_pseudo_args($i1, $i2, 'epistemic-modality', { deep => 0 });
      $tests{'mods'} = $self->cmp_mods($i1, $i2);
      $tests{'features'} = $self->cmp_features($i1, $i2);
      $tests{'aggregate'} = $self->cmp_aggregate($i1, $i2);
      $tests{'predicate'} = $self->cmp_predicate($i1, $i2);
      # FIXME: $result &&= # now we add details in cmp_args
      $tests{'args'} = $self->cmp_args($i1, $i2, { deep => 0 });
      $tests{'site'} = $self->cmp_pseudo_args($i1, $i2, 'site');
      $tests{'location'} = $self->cmp_pseudo_args($i1, $i2, 'location');
      $tests{'from-location'} = $self->cmp_pseudo_args($i1, $i2, 'from-location');
      $tests{'to-location'} = $self->cmp_pseudo_args($i1, $i2, 'to-location');
      $tests{'cell-line'} = $self->cmp_pseudo_args($i1, $i2, 'cell-line');
    }
  }

  $result &&= min(values(%tests));
  DEBUG 3, "result($i1,$i2,deep=%s) ==> %s", __option($options,'deep'), $result;
  DEBUG 3, "tests ==> %s", Dumper(\%tests);
  $self->record_result($i1, $i2, $result, \%tests)
    unless (__option_is($options, 'deep', 0) and $result);
  DEBUG 2, "==> %s", $result;
  $result;
}

sub cmp_CC {
  my $self = shift;
  my ($i1, $i2, $options) = @_;

  __assert_eq($i1->nodeName, "CC") && __assert_eq($i2->nodeName, "CC")
    or return 0;

  # overlap test; if this fails, we bail
  my $result = _span_overlap(_get_span($i1), _get_span($i2));
  
  # contents test
  my %tests = ();
  if ($result) {
    # type test; the only one performed for shallow matches
    $tests{'type'} = $self->cmp_ont_slot($i1, $i2, 'type');
  
    unless (__option_is($options, 'deep', 0)) {
      $tests{'negation'} = $self->cmp_text_slot($i1, $i2, 'negation');
      $tests{'polarity'} = $self->cmp_text_slot($i1, $i2, 'polarity');
      $tests{'modality'} = $self->cmp_text_slot($i1, $i2, 'modality');
      $tests{'args'} = $self->cmp_args($i1, $i2, { deep => 0 });
    }
  }
  
  $result &&= min(values(%tests));
  $self->record_result($i1, $i2, $result, \%tests)
      unless (__option_is($options, 'deep', 0) and $result);
  DEBUG 2, "==> %s", $result;
  $result;
}

sub cmp_EPI {
  my $self = shift;
  my ($i1, $i2, $options) = @_;

  __assert_eq($i1->nodeName, "EPI") or return 0;
  __assert_eq($i2->nodeName, "EPI") or return 0;

  # overlap test; if this fails, we bail
  my $result = _span_overlap(_get_span($i1), _get_span($i2));
  
  # contents test
  my %tests = ();
  if ($result) {
    # type test; the only one performed for shallow matches
    $tests{'type'} = $self->cmp_ont_slot($i1, $i2, 'type');
  
    unless (__option_is($options, 'deep', 0)) {
      $tests{'negation'} = $self->cmp_text_slot($i1, $i2, 'negation');
      $tests{'polarity'} = $self->cmp_text_slot($i1, $i2, 'polarity');
      $tests{'modality'} = $self->cmp_text_slot($i1, $i2, 'modality');
      $tests{'args'} = $self->cmp_args($i1, $i2, { deep => 0 });
    }
  }

  $result &&= min(values(%tests));
  $self->record_result($i1, $i2, $result, \%tests)
      unless (__option_is($options, 'deep', 0) and $result);
  DEBUG 2, "==> %s", $result;
  $result;
}

sub cmp_MODALITY {
  my $self = shift;
  my ($i1, $i2, $options) = @_;

  __assert_eq($i1->nodeName, "MODALITY") && __assert_eq($i2->nodeName, "MODALITY")
    or return 0;

  # overlap test; if this fails, we bail
  my $result = _span_overlap(_get_span($i1), _get_span($i2));
  
  # contents test
  my %tests = ();
  if ($result) {
    # type test; the only one performed for shallow matches
    $tests{'type'} = $self->cmp_ont_slot($i1, $i2, 'type');
  
    unless (__option_is($options, 'deep', 0)) {
      $tests{'negation'} = $self->cmp_text_slot($i1, $i2, 'negation');
      $tests{'polarity'} = $self->cmp_text_slot($i1, $i2, 'polarity');
      $tests{'args'} = $self->cmp_args($i1, $i2, { deep => 0 });
    }
  }

  $result &&= min(values(%tests));
  $self->record_result($i1, $i2, $result, \%tests)
      unless (__option_is($options, 'deep', 0) and $result);
  DEBUG 2, "==> %s", $result;
  $result;
}


### functions for matching extraction properties

sub cmp_attribute {
  my $self = shift;
  my ($i1, $i2, $attr) = @_;

  my $a1 = $i1->getAttribute($attr);
  my $a2 = $i2->getAttribute($attr);

  DEBUG 2, "matching attribute %s: %s ~~ %s", $attr, $a1, $a2; 
    
  return 1 if (! $a1) && (! $a2);
  return 0 unless $a1 && $a2;
  ($a1 eq $a2) + 0;
}

# NB: $set_cmp is restricted to 'strict'; any other value is equivalent to undef
sub cmp_attribute_aslist {
  my $self = shift;
  my ($i1, $i2, $attr, $delim, $set_cmp) = @_;

  $set_cmp //= '';
  
  my $a1 = $i1->getAttribute($attr);
  my $a2 = $i2->getAttribute($attr);
    
  DEBUG 2, "matching attribute %s: %s ~~ %s", $attr, $a1, $a2;
    
  return 1 if ((! $a1) && (! $a2));
  return 0 unless $a1;
  return 0 unless $a2;
  my @l1 = split(/\Q$delim/, $a1);
  my @l2 = split(/\Q$delim/, $a2);
  my $result =
    ($set_cmp eq 'strict')
    ? __set__is_equal(\@l1, \@l2)
    : (scalar(__set__intersection(\@l1, \@l2)) > 0);
  DEBUG 2, "cmp_attribute_aslist($attr, $a1 ~~ $a2) ==> $result";
  $result + 0;
}

# compare slot whose value is some text
sub cmp_text_slot {
  my $self = shift;
  my ($i1, $i2, $slot) = @_;
  my $t1 = get_slot_value($i1, $slot);
  my $t2 = get_slot_value($i2, $slot);
  return 1 if ((! $t1) && (! $t2));
  return 0 unless ($t1 && $t2);
  DEBUG 2, "%s: %s ~~ %s => %s", $slot, $t1, $t2, ($t1 eq $t2);
  ($t1 eq $t2) + 0;
}

# FIXME: <frequency> items appear to be polymorphous and this might fail!
#        perhaps the fix should be in the EKB format!
sub cmp_mods {
  my $self = shift;
  my ($i1, $i2) = @_;

  my ($mods1) = $i1->findnodes('./mods');
  my ($mods2) = $i2->findnodes('./mods');

  return 1 if ((! $mods1) && (! $mods2));
  return 0 unless $mods1 && $mods2;
  my %mods1 = __get_mods($mods1);
  my %mods2 = __get_mods($mods2);
  DEBUG 2, "\nmods1: %s\nmods2: %s", Dumper(\%mods1), Dumper(\%mods2);
  eq_deeply(\%mods1, \%mods2);
}

# TODO: should also compare not-features!
sub cmp_features {
  my $self = shift;
  my ($i1, $i2) = @_;

  my ($feats1) = $i1->findnodes('./features');
  my ($feats2) = $i2->findnodes('./features');

  return 1 if ((! $feats1) && (! $feats2));
  return 0 unless $feats1 && $feats2;

  my @tests =
    ( 
     $self->cmp_text_slot($feats1, $feats2, 'active'),
     $self->cmp_pseudo_args($feats1, $feats2, 'location'),
     $self->cmp_features_mutation($feats1, $feats2),
     $self->cmp_features_site($feats1, $feats2),
     $self->cmp_pseudo_args($feats1, $feats2, 'cell-line'),
     $self->cmp_features_inevent($feats1, $feats2),
     $self->cmp_pseudo_args($feats1, $feats2, 'bound-to', { deep => 0 }),
    );
  min(@tests);
}

# polymorphic:
# - true/false
# - fully specified
sub cmp_mutation {
  my $self = shift;
  my ($i1, $i2) = @_;

  my ($mut1) = $i1->findnodes('./mutation');
  my ($mut2) = $i2->findnodes('./mutation');

  return 1 if ((! $mut1) && (! $mut2));
  return 0 unless $mut1 && $mut2;

  # shortcut: we serialize them and compare as strings
  (my $ser_mut1 = $mut1->toString(0)) =~ s/>\s+</></mg;
  (my $ser_mut2 = $mut2->toString(0)) =~ s/>\s+</></mg;

  ($ser_mut1 eq $ser_mut2) + 0;
}

sub cmp_aggregate {
  my $self = shift;
  my ($i1, $i2) = @_;

  my ($agg1) = $i1->findnodes('./aggregate');
  my ($agg2) = $i2->findnodes('./aggregate');

  return 1 if ((! $agg1) && (! $agg2));
  return 0 unless $agg1 && $agg2;

  # FIXME: should we perhaps *not* short-circuit here?
  $self->cmp_attribute($agg1, $agg2, 'operator')
  &&
  $self->cmp_pseudo_args($agg1, $agg2, 'member');
}

sub cmp_components {
  my $self = shift;
  my ($i1, $i2) = @_;

  my ($comp1) = $i1->findnodes('./components');
  my ($comp2) = $i2->findnodes('./components');

  return 1 if ((! $comp1) && (! $comp2));
  return 0 unless $comp1 && $comp2;
  $self->cmp_pseudo_args($comp1, $comp2, 'component');
}

# this applies to protein family members, for which the dbid is the defining feature
# WARNING: this assumes the dbid is unique per member; if this changes in the future,
# this function will cease to work properly!
sub cmp_members {
  my $self = shift;
  my ($i1, $i2) = @_;

  my @elems1 = $i1->findnodes('./members');
  my @elems2 = $i2->findnodes('./members');

  return 1 if (! @elems1) && (! @elems2);
  return 0 unless @elems1 && @elems2;

  my @dbids1 =
    map { $_->getAttribute('dbid') }
    @elems1;
  my @dbids2 =
    map { $_->getAttribute('dbid') }
    @elems2;
   
  __set__is_equal(\@dbids1, \@dbids2);
}

sub cmp_predicate {
  my $self = shift;
  my ($i1, $i2) = @_;

  return 1 if ($self->option('ignore_text'));

  my ($pred1) = $i1->findnodes('./predicate');
  my ($pred2) = $i2->findnodes('./predicate');

  return 1 if ((! $pred1) && (! $pred2));
  return 0 unless $pred1 && $pred2;
  return 1 if $self->option('ignore_pred_type');
  $self->cmp_text_slot($pred1, $pred2, 'type'); #FIXME: cmp_ont_slot!?!
}

sub cmp_args {
  my $self = shift;
  my ($i1, $i2, $options) = @_;

  my @args1 = assertion_args($i1);
  my @args2 = assertion_args($i2);

  {
    DEBUG 2, "args: %d vs. %s", scalar(@args1), scalar(@args2);
  }

  # shortcut if both are empty
  return 1 if (! @args1) && (! @args2);
    
  # sort args by role (assuming unique arg per role)
  my %rolemap1 =
    map { $_->getAttribute('role') => { id => $_->getAttribute('id'), 
					arg => $_ } }
    @args1;
  my %rolemap2 =
    map { $_->getAttribute('role') => { id => $_->getAttribute('id'),
					arg => $_ } }
    @args2;

  # FIXME: do not shortcut
  #return 0 unless __set__is_equal([keys %rolemap1], [keys %rolemap2]);
  my %tests = ();
  foreach my $role (uniq keys(%rolemap1), keys(%rolemap2))   {
    if (! (exists $rolemap1{$role} && exists $rolemap2{$role})) {
      # role present only in one assertion
      $tests{$role} = 0;
      next;
    }
    my $a1 = (defined $rolemap1{$role}{id}) 
      ? $self->ekb1()->get_assertion($rolemap1{$role}{id})
      : undef;
    my $a2 = (defined $rolemap2{$role}{id})
      ? $self->ekb2()->get_assertion($rolemap2{$role}{id})
      : undef;
    if ($a1 && $a2) {
      DEBUG 3, "cmp_args: => 2";
      $tests{$role} = $self->cmp_ekb_item($a1, $a2, $options);
    }      
    elsif ((! $a1) && (! $a2)) {
      # no assertions for these args; we just look at the type
      DEBUG 3, "cmp_args: => 0";
      $tests{$role} =
	$self->cmp_text_slot($rolemap1{$role}{arg}, $rolemap2{$role}{arg}, 'type');
    }
    else { 
      DEBUG 3, "cmp_args: => 1";
      $tests{$role} = 0;
    }
  }
  my $result = min(values(%tests));
  DEBUG 3, "tests ==> %s", Dumper(\%tests);
  $self->record_result($i1, $i2, $result, {arg => \%tests})
    unless (__option_is($options, 'deep', 0) and $result);
  DEBUG 2, "cmp_args ==> %s", $result;
  $result;
}

# these are sub-elements that refer to other EKB items
# TODO: there may be corner cases where either the @id doesn't resolve to any
# EKB item, or there is no @id at all!
# FIXME: this should be renamed, as it used for other things than actual
# pseudo/satellite args!
sub cmp_pseudo_args {
  my $self = shift;
  my ($i1, $i2, $name, $options) = @_;
  my @elems1 = $i1->findnodes("./$name");
  my @elems2 = $i2->findnodes("./$name");

  return 1 if (! @elems1) && (! @elems2);
  return 0 unless @elems1 && @elems2;
  # this is never a deep match, so we take a shortcut
  return 0 unless scalar(@elems1) == scalar(@elems2);

  my @items1 =
    grep { defined }
    map { $self->ekb1()->get_assertion($_) }
    map { $_->getAttribute('id') }
    @elems1;
  my @items2 =
    grep { defined }
    map { $self->ekb2()->get_assertion($_) }
    map { $_->getAttribute('id') }
    @elems2;

  {
    local $Data::Dumper::Terse = 1;
    local $Data::Dumper::Indent = 0;
    DEBUG 3, "cmp_pseudo_args:\n\@items1=%s\@items2=%s",
      Dumper(\@items1), Dumper(\@items2);
  }
  
  # if there are missing assertions, we shortcut
  return 0 unless scalar(@items1) == scalar(@items2);

  # TODO: pseudo_args that have no referent should be compared, too!
  # TODO: some pseudo-args have other attributes beside id

  $self->cmp_ekb_items(\@items1, \@items2,
		       __options($options, 'deep' => 0));
}

# polymorphic:
# - true/false
# - pseudo-args
sub cmp_features_mutation {
  my $self = shift;
  my ($f1, $f2) = @_;
    
  # pseudo-args
  return 0 unless $self->cmp_pseudo_args($f1, $f2, 'mutation[@id]');

  # true/false
  my ($mut1) = $f1->findnodes('./mutation[not(@id)]');
  my ($mut2) = $f2->findnodes('./mutation[not(@id)]');

  return 1 if (! $mut1) && (! $mut2);
  return 0 unless $mut1 && $mut2;
  ($mut1->textContent eq $mut2->textContent) + 0;
}

# polymorphic:
# - domain sites are pseudo-args
# - residue sites are fully specified
sub cmp_features_site {
  my $self = shift;
  my ($f1, $f2) = @_;

  # pseudo-args
  return 0 unless $self->cmp_pseudo_args($f1, $f2, 'site[@id]');

  # residues: we serialize them and compare as strings
  my @elems1 =
    map { (my $s = $_) =~ s/>\s+</></mg; $s }
    map { $_->toString(0) }
    $f1->findnodes('./site[not(@id)]');
  my @elems2 = 
    map { (my $s = $_) =~ s/>\s+</></mg; $s }
    map { $_->toString(0) }
    $f2->findnodes('./site[not(@id)]');

  return 1 if (! @elems1) && (! @elems2);
  return 0 unless @elems1 && @elems2;

  {
    local $Data::Dumper::Terse = 1;
    local $Data::Dumper::Indent = 0;
    DEBUG 2, "sites: %s ~~ %s", Dumper(\@elems1), Dumper(\@elems2);
  }
    
  return __set__is_equal(\@elems1, \@elems2);
}

# this is the old format; see cmp_mods_inevent for the new style
sub cmp_features_inevent {
  my $self = shift;
  my ($f1, $f2) = @_;
  # FIXME: these XPath expressions match both the old style, and the new style.
  # eventually the old style should be removed
  my @elems1 = $f1->findnodes('./inevent/event[@id] | ./inevent[@id]');
  my @elems2 = $f2->findnodes('./inevent/event[@id] | ./inevent[@id]');

  return 1 if (! @elems1) && (! @elems2);
  return 0 unless @elems1 && @elems2;
  # this is never a deep match, so we take a shortcut
  return 0 unless scalar(@elems1) == scalar(@elems2);

  my @evids1 = 
    map { $_->getAttribute('id') } @elems1;
  my @events1 =
    grep { defined $_ }
    map { $self->ekb1()->get_assertion($_) } @evids1;
  my @evids2 = 
    map { $_->getAttribute('id') } @elems2;
  my @events2 =
    grep { defined $_ }
    map { $self->ekb2()->get_assertion($_) } @evids2;
  # another shortcut
  return 0 unless scalar(@events1) == scalar(@events2);

  {
    local $Data::Dumper::Terse = 1;
    local $Data::Dumper::Indent = 0;
    DEBUG 2, "inevent: %s ~~ %s", Dumper(\@evids1), Dumper(\@evids2);
    DEBUG 3, "inevent: %s ~~ %s", join(", ", @events1), join(", ", @events2);
  }
    
  return $self->cmp_ekb_items(\@events1, \@events2, { deep => 0} );
}

# compare slot whose value is an ont type
# we only check bio ontologies
sub cmp_ont_slot {
  my $self = shift;
  if ($self->option('strict_ont_match')) {
    return $self->cmp_text_slot(@_);
  }
  my ($i1, $i2, $slot) = @_;
  my $t1 = get_slot_value($i1, $slot);
  my $t2 = get_slot_value($i2, $slot);
  return 1 if ((! $t1) && (! $t2));
  return 0 unless $t1 && $t2;
  if ($ont_events->has($t1)) {
    return 0 + ($ont_events->is_a($t1, $t2) || $ont_events->is_a($t2, $t1));
  }
  if ($ont_bioents->has($t1)) {
    return 0 + ($ont_bioents->is_a($t1, $t2) || $ont_bioents->is_a($t2, $t1));
  }
  ($t1 eq $t2) + 0;
}


### operations on match graph (for an utterance)

# TODO: simplify this structure; maybe make it a class?
sub reset_x_map {
  my $self = shift;
  $self->{'x_map'} = {};
}

# record matching result for two assertions, using their ids
sub record_result {
  my $self = shift;
  my ($i1, $i2, $result, $tests) = @_;
  if ($result) {
    $self->record_match($i1, $i2);
  } else {
    $self->record_no_match($i1, $i2, $tests);
  }
  {
    DEBUG 3, "x_map ($i1, $i2): %s", Dumper($self->{'x_map'});
  }
}

# record a match
sub record_match {
  my $self = shift;
  my ($i1, $i2) = @_;
  return 1 if $self->matched($i1, $i2);
  my $id1 = $i1->getAttribute('id');
  my $id2 = $i2->getAttribute('id');
  if ($self->has_match($i1, 'ref')) {
    ERROR "Cannot re-match %s to %s (now: %s ~~ %s)",
      $id1, $id2, $id1, $self->{'x_map'}{'ref'}{$id1};
    return 0;
  }
  if ($self->has_match($i2, 'hyp')) {
    ERROR "Cannot re-match %s to %s (now: %s ~~ %s)",
      $id1, $id2, $self->{'x_map'}{'hyp'}{$id2}, $id2;
    return 0;
  }
  $self->{'x_map'}{'ref'}{$id1}{$id2} = 1;
  $self->{'x_map'}{'hyp'}{$id2}{$id1} = 1;
  # prune previous non-matches
  foreach my $hid (keys %{$self->{'x_map'}{'ref'}{$id1}}) {
    unless ($self->{'x_map'}{'ref'}{$id1}{$hid}) {
      delete $self->{'x_map'}{'ref'}{$id1}{$hid};
      delete $self->{'x_map'}{'hyp'}{$hid}{$id1};
      delete $self->{'x_map'}{'diff'}{$id1}{$hid};	
    }
  }
  foreach my $rid (keys %{$self->{'x_map'}{'hyp'}{$id2}}) {
    unless ($self->{'x_map'}{'hyp'}{$id2}{$rid}) {
      delete $self->{'x_map'}{'hyp'}{$id2}{$rid};
      delete $self->{'x_map'}{'ref'}{$rid}{$id2};
      delete $self->{'x_map'}{'diff'}{$rid}{$id2};	
    }
  }
  DEBUG 2, "matched: %s ~~ %s", $id1, $id2;
  1;
}

# record a no-match
sub record_no_match {
  my $self = shift;
  my ($i1, $i2, $tests) = @_;
  my $id1 = $i1->getAttribute('id');
  my $id2 = $i2->getAttribute('id');
  if ($self->matched($i1, $i2)) {
    ERROR "Cannot unmatch previous match: %s ~~ %s", $id1, $id2;
    return 0;
  }
  $self->{'x_map'}{'ref'}{$id1}{$id2} = 0;
  $self->{'x_map'}{'hyp'}{$id2}{$id1} = 0;
  if ($tests) {
    map {$self->{'x_map'}{'diff'}{$id1}{$id2}{$_} = $tests->{$_}} keys %$tests;
    DEBUG 3, "diffs($id1,$id2) = %s", Dumper($self->{'x_map'}{'diff'}{$id1}{$id2});
  }
  DEBUG 2, "No match: %s ~~ %s", $id1, $id2;
  1;
}

sub seen_before {
  my $self = shift;
  my ($i1, $i2) = @_;
  my $id1 = $i1->getAttribute('id');
  my $id2 = $i2->getAttribute('id');
  return
    (exists $self->{'x_map'}{'ref'}{$id1})
    &&
    (exists $self->{'x_map'}{'ref'}{$id1}{$id2});
}

sub matched {
  my $self = shift;
  my ($i1, $i2) = @_;
  return 0 unless $self->seen_before($i1, $i2);
  my $id1 = $i1->getAttribute('id');
  my $id2 = $i2->getAttribute('id');
  DEBUG 3, "matched(%s, %s) = %s", $id1, $id2, $self->{'x_map'}{'ref'}{$id1}{$id2};
  return $self->{'x_map'}{'ref'}{$id1}{$id2};
}

sub not_matched {
  my $self = shift;
  my ($i1, $i2) = @_;
  return 0 unless $self->seen_before($i1, $i2);
  my $id1 = $i1->getAttribute('id');
  my $id2 = $i2->getAttribute('id');
  DEBUG 3, "not_matched(%s, %s) = %s", $id1, $id2, $self->{'x_map'}{'ref'}{$id1}{$id2};
  return !$self->{'x_map'}{'ref'}{$id1}{$id2};
}

# nb: $source is 'ref' or 'hyp'
sub has_match {
  my $self = shift;
  my ($item, $source) = @_;
  ($source eq 'ref') || ($source eq 'hyp')
    or FATAL "2nd argument must be 'ref' or 'hyp'";
    
  my $id = $item->getAttribute('id');
  return 0 unless (exists $self->{'x_map'}{$source}{$id});
  DEBUG 3, "has_match(%s, %s) = %s", $id, $source,
    any { $self->{'x_map'}{$source}{$id}{$_} }
    keys %{ $self->{'x_map'}{$source}{$id} };
  return
    any { $self->{'x_map'}{$source}{$id}{$_} }
    keys %{ $self->{'x_map'}{$source}{$id} };
}


### diffs

sub add_diff {
  my $self = shift;
  my ($section, $diff) = @_;
  push @{ $self->diffs->{$section} }, $diff;
}

sub get_diffs {
  my $self = shift;
  my ($section) = @_;
  if (defined $self->diffs->{$section}) {
    return @{ $self->diffs->{$section} };
  }
  return ();
}

sub diffs_as_string {
  my $self = shift;

  {
    DEBUG 2, "diffs: %s", Dumper($self->diffs);
  }
    
  my $result;

  # options
  $result .= sprintf("Options: %s\n", $self->_options_to_string());

  # summary result
  $result .= sprintf("------\nResult: EKBs are %s\n",
		     $self->result ? "identical" : "different");

  # counts table
  $result .= "------\nAssertion counts:\n";
  my $summary = $self->summary();
  DEBUG 1, "summary: %s", Dumper($summary);

  $result .= sprintf(" REF\t%d\n",
		     $summary->{del} + $summary->{eql});
  $result .= sprintf(" HYP\t%d\n",
		     $summary->{ins} + $summary->{eql});
  $result .= sprintf(" DEL\t%d\n",
		     $summary->{del});
  $result .= sprintf(" INS\t%d\n",
		     $summary->{ins});
  $result .= sprintf(" EQL\t%d\n",
		     $summary->{eql});
  
  # if identical, that's it
  return $result if $self->result;
    
  # meta info
  $result .= "------\nDiffs:\n\n";

  # attributes
  foreach my $diff ($self->get_diffs('attributes')) {
    $result .= sprintf("EKB attribute: %s\n< %s\n> %s\n",
		       $diff->{attr},
		       $diff->{ref},
		       $diff->{hyp});
  }
  
  # sentences
  $result .= sprintf("Input:\n")
    if $self->get_diffs('sentences');
  foreach my $diff ($self->get_diffs('sentences')) {
    $result .= sprintf("< %s\n", $self->ekb1->get_sentence($diff->{'ref'}))
      if (exists $diff->{'ref'});
    $result .= sprintf("> %s\n", $self->ekb2->get_sentence($diff->{'hyp'}))
      if (exists $diff->{'hyp'});
  }
  DEBUG 1, "result: %s", $result;

  # assertions, by utt and spans
  my @a_diffs = sort _diff_cmp $self->get_diffs('assertions');
  DEBUG 3, "sorted diffs: %s", Dumper(\@a_diffs);
  my @a_diff_sids = uniq map { $_->{sid} } grep {exists $_->{sid}} @a_diffs;
  $result .= "Assertions (by sentence):\n"
    if (@a_diff_sids);
  foreach my $sid (@a_diff_sids) {
    my $utt = $self->ekb1()->get_sentence($sid);
    $result .= sprintf("uttnum=%d %s\n", $sid, $utt->textContent);
    foreach my $diff (grep { $_->{sid} eq $sid } @a_diffs) {
      if (exists $diff->{'ref'}) {
	$result .= sprintf("<<< %s\n%s\n",
			   $self->diff_highlights($diff->{'ref'}),
			   $self->ekb1->get_assertion($diff->{'ref'})->toString(1));
      }
      $result .= sprintf(">>> \n%s\n",
			 $self->ekb2()->get_assertion($diff->{'hyp'})->toString(1))
	if exists $diff->{'hyp'};
    }
  }
  # assertions, ungrounded
  my @a_diff_nos = grep {!exists $_->{sid}} @a_diffs;
  $result .= "Assertions (ungrounded):\n"
    if (@a_diff_nos);
  foreach my $diff (@a_diff_nos) {
    if (exists $diff->{'ref'}) {
      $result .= sprintf("<<< %s\n%s\n",
			 $self->diff_highlights($diff->{'ref'}),
			 $self->ekb1->get_assertion($diff->{'ref'})->toString(1));
    }
    $result .= sprintf(">>> \n%s\n",
		       $self->ekb2()->get_assertion($diff->{'hyp'})->toString(1))
      if exists $diff->{'hyp'};
  }

  $result;
}

# creates a string highlighting the "problem" areas
# if REF assertion was matched unsuccessfully against multiple HYP assertions,
# this picks only what is obviously changed
sub diff_highlights {
  my $self = shift;
  my $aid = shift;
  return "" unless exists $self->{'x_map'}{'diff'}{$aid};
  my $diff = $self->{'x_map'}{'diff'}{$aid};
  my %summary; # flattened hash of "problems", in aggregate
  my $n = 0;
  foreach my $hid (keys %$diff) {
    foreach my $k (keys %{$diff->{$hid}}) {
      next if $k eq "args"; #FIXME: this should disappear 
      if ($k eq "arg") {
	foreach my $r (keys %{$diff->{$hid}{$k}}) {
	  my $p = $k . ":" . $r;
	  $summary{$p} += $diff->{$hid}{$k}{$r};
	}
      }
      else {
	$summary{$k} += $diff->{$hid}{$k};
      }
    }
    $n++;
  }
  DEBUG 2, "summary diffs (%s): %s", $aid, Dumper(\%summary);
  my (@n, @m);
  foreach my $p (keys %summary) {
    if ($summary{$p} == 0) {
      push(@n, $p);
    }
    elsif ($summary{$p} < $n) {
      push(@m, $p);
    }
  }
  sprintf("!(%s) ~(%s)", join(" ", @n), join(" ", @m));
}

sub _options_to_string {
  my $self = shift;
  my $result = "";
  my %options = $self->options();
  return
    join ", ",
    map {
      my $opt = $_;
      my $val;
      if (ref($options{$opt}) eq "ARRAY") {
	$val = sprintf("(%s)", join(",", @{$options{$opt}}));
      } else {			# scalar
	$val = $options{$opt};
      }
      sprintf("%s => %s", $opt, $val);
    }
    sort keys %options;
  return $result;
}


### operations on spans

# get span from an EKB entry
# TODO: move to EKB.pm
sub _get_span {
  my $x = shift;
  my $start = $x->getAttribute('start');
  my $end = $x->getAttribute('end');
  unless (defined $start and defined $end) {
    $start = 0;
    $end = 0;
  }
  return [$start, $end];
}

# TODO: this likely belongs in EKB.pm, though the way it's done here is a bit
# peculiar. also, we're only doing this for mods, which is also peculiar!
# will have to rethink it in a more general way.
sub __get_mods {
  my $mods_elem = shift;
  my @children = $mods_elem->findnodes('./mod|./degree|./frequency');
  DEBUG 3, "mods [%d]: %s", scalar(@children), "@children";
  my %mods;
  map { $mods{$_->nodeName}{get_slot_value($_, 'type')} = 1 }
    $mods_elem->nonBlankChildNodes();
  return %mods;
}

# span comparator
sub _span_cmp {
  my ($span1, $span2) = @_;
  if ($span1->[0] == $span2->[0]) {
    return ($span1->[1] <=> $span2->[1]);
  }
  return -1 if ($span1->[0] <= $span2->[0]);
  return  1;
}

# span overlap
sub _span_overlap {
  my ($span1, $span2) = @_;
  # by convention, a zero-length span is unknown, so assume overlap
  return 1 if ($span1->[0] == $span1->[1] or $span2->[0] == $span2->[1]);
  # span1 left-of span2
  return 0 if ($span1->[1] < $span2->[0]);
  # span1 right-of span2
  return 0 if ($span1->[0] > $span2->[1]);
  # overlap
  return 1;
}

# span distance: the Pompeiu???Hausdorff distance
sub _span_distance {
  my ($span1, $span2) = @_;
  return max ( abs($span2->[0] - $span1->[0]),
	       abs($span2->[1] - $span1->[1]) );
}

# diff comparison (use only for sorting assertion diffs!!)
sub _diff_cmp($$) {
  my ($a, $b) = @_;
  return (!defined($a->{sid}) or !defined($b->{sid}) or $a->{sid} <=> $b->{sid})
    || (!defined($a->{start}) or !defined($b->{start}) or $a->{start} <=> $b->{start})
    || (!defined($a->{end}) or !defined($b->{end}) or $a->{end} <=> $b->{end})
    || ((exists($a->{ref}) and !exists($b->{ref})) ? -1 : 0)
    || ((exists($a->{hyp}) and !exists($b->{hyp})) ? 1 : 0)
    || (!defined($a->{ref}) or !defined($b->{ref}) or $a->{ref} cmp $b->{ref})
    || (!defined($a->{hyp}) or !defined($b->{hyp}) or $a->{hyp} cmp $b->{hyp});
}


### handling options

# opts: hashref
# 
sub __options {
  my $opts = shift;
  my %new_opts = @_;
  while (my ($opt, $val) = each(%new_opts)) {
    __option($opts, $opt, $val);
  }
  return $opts;
}

sub __option {
  my $opts = shift;
  return undef unless (ref($opts) eq "HASH");
  my ($option, $value) = @_;
  if (defined $value) {
    $opts->{$option} = $value;
  }
  return $opts->{$option};
}

sub __option_is {
  my ($opts, $option, $value) = @_;
  return 0 unless (ref($opts) eq "HASH");
  return 0 unless (defined $value);
  return 0 unless (defined $opts->{$option});
  return $opts->{$option} eq $value;
}

### other utilities

# returns 1 if operands are either undefined or they are equal
sub __assert_eq {
  my ($a, $b) = @_;
  return 1 if (! defined $a) && (! defined $b);
  return 1 if ($a eq $b);
  DEBUG 3, "assertion failed: (\"%s\" eq \"%s\")", $a, $b;
  return 0;
}

# set operations (use only for lists of scalars!)
sub __set__is_member {
  my ($elem, $list) = @_;
  any { $_ eq $elem } @$list;
}
sub __set__is_included {
  my ($list1, $list2) = @_;
  all { __set__is_member($_, $list2) } @$list1;
}
sub __set__is_equal {
  my ($list1, $list2) = @_;
  __set__is_included($list1, $list2) and __set__is_included($list2, $list1);
}
sub __set__union {
  my ($list1, $list2) = @_;
  uniq(@$list1, @$list2);
}
sub __set__intersection {
  my ($list1, $list2) = @_;
  grep { __set__is_member($_, $list2) } @$list1;
}

1;
