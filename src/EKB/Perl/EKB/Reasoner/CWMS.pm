# CWMS.pm
#
# Time-stamp: <Sat Apr 10 23:04:54 CDT 2021 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>,  1 Jun 2016
# $Id: CWMS.pm,v 1.13 2021/04/11 04:20:10 lgalescu Exp $
#

#----------------------------------------------------------------
# Description:
# Reasoner for making inferences from text-extracted EKBs.

#----------------------------------------------------------------
# History:
# 2018/03/05 v0.0	lgalescu
# - Started. Copied a few general-purpose rules from DRUM.
# 2019/06/11 v1.0	lgalescu
# - Significant additions.
# 2019/07/01 v1.1	lgalescu
# - Fixed bugs (getting attribute value from descendants)
# 2019/12/15 v1.2	lgalescu
# - Fixed bug.
# 2020/07/28 v1.3	lgalescu
# - Added TRIPS ontology
# - Added inference for "provide a source of"
# 2020/10/27 v1.4	lgalescu
# - Added rules to fix and interpred ONT::PRODUCE events

#----------------------------------------------------------------
# Usage:
# 

package EKB::Reasoner::CWMS;

$VERSION = '1.4';

use strict 'vars';
use warnings;

use Data::Dumper;
use XML::LibXML;
use List::Util 1.45 qw(all any first min max uniq);

use EKB;
use EKB::Match;
use Ont::BioEntities;
use Ont::BioEvents;
use Ont::Geo;
use Ont::TRIPS;
use EKB::Reasoner::Rule;
use EKB::Reasoner;
use util::Log;

our @ISA = qw(EKB::Reasoner);

my @rules;

# nb: we don't want to initialize these here
my $ont_geo;
my $ont_trips;

sub new {
  my $class = shift;
  my $ekb = shift;
  my %options = @_;
  my $self = {
	      ekb => $ekb,
	      rules => [],
	      # options
	      options => {},
	      # log for which rules applies to what elements
	      log => {},
	     };
  bless $self, $class;
  $self->_init(%options);
  return $self;
}

sub _init {
  my $self = shift;
  $self->default_options();
  $self->SUPER::_init(@_); # pass options up

  # hack: initialize ontologies here
  $ont_geo = Ont::Geo->new();
  $ont_trips = Ont::TRIPS->new();

  push @{ $self->{rules} },
    map { EKB::Reasoner::Rule->new($self, $_) } @rules;
}

sub default_options {
  my $self = shift;
  $self->options(
		);
}

## Rules

# note: this is an ordered list!
# rules that modify assertions should precede clean-up rules
@rules =
  (

   ### ont fixes
   {
    ## ONT::PRODUCE
    # e=EVENT[type=ONT::PRODUCE]
    # p=e->predicate->type
    # =>
    # e=EVENT[type=p]
    name => "EKR:FixPRODUCE",
    constraints => ['EVENT[type[.="ONT::PRODUCE"]]'],
    handler => sub {
      my ($rule, $ekb, $e) = @_;
      
      my $e_id = $e->getAttribute('id');
      my ($pred) = $e->findnodes('predicate');
      my $ptype = get_slot_value($pred, "type") if $pred;
      $ptype = "ONT::SITUATION-ROOT" unless $ptype;
      
      INFO "Rule %s matches event %s (pred: %s)",
	$rule->name(), $e_id, $ptype;

      $ekb->modify_assertion( $e,
			      make_slot_node(type => $ptype)
			      );
      1;
    }
   },

   # abandoned -- maybe we should get IM/EX to do better first
   # {
   #  ## ONT::CONFLICT
   #  # e=EVENT[type=ONT::PRODUCE,predicate/type=X]
   #  # X==INVADE,INVASION
   #  # =>
   #  # e=EVENT[type=p]
   #  name => "EKR:FixCONFLICT",
   #  constraints => ['EVENT[type[.="ONT::CONFLICT"]]'],
   #  handler => sub {
   #    my ($rule, $ekb, $e) = @_;
      
   #    my $e_id = $e->getAttribute('id');
   #    my ($pred) = $e->findnodes('predicate');
   #    my $ptype = get_slot_value($pred, "type") if $pred;
   #    my ($prednorm) = getvalue_xpath($e, 'predicate/text/@normalization');
   #    $prednorm 
   # 	or return 0;
   #    if ($prdnorm

      
   #    INFO "Rule %s matches event %s (ONT::CONFLICT~%s~%s)",
   # 	$rule->name(), $e_id, $ptype, $prednorm;

   #    $ekb->modify_assertion( $e,
   # 			      make_slot_node(type => $ptype)
   # 			      );
   #    1;
   #  }
   # },

   {
    ## ONT::CREATE
    # e=EVENT[type=ONT::CREATE, predicate/text/@normalization=BUILD]
    # =>
    # e=EVENT[type=ONT::BUILD]
    name => "EKR:FixCREATE-BUILD-1",
    constraints => ['EVENT[type[.="ONT::CREATE"]]'],
    handler => sub {
      my ($rule, $ekb, $e) = @_;
      
      my $e_id = $e->getAttribute('id');
      my ($prednorm) = getvalue_xpath($e, 'predicate/text/@normalization');
      $prednorm and ($prednorm eq "BUILD")
	or return 0;
      
      INFO "Rule %s matches event %s (prednorm: %s)",
	$rule->name(), $e_id, $prednorm;

      $ekb->modify_assertion( $e,
			      make_slot_node(type => "ONT::BUILD")
			      );
      1;
    }
   },
   
   {
    ## ONT::CREATE
    # e=EVENT[type=ONT::CREATE, affected-result=x]
    # x=TERM[type=ONT::MAN-MADE-STRUCTURE]
    # =>
    # e=EVENT[type=ONT::BUILD]
    name => "EKR:FixCREATE-BUILD-2",
    constraints => ['EVENT[type[.="ONT::CREATE"] and arg2[@role=":AFFECTED-RESULT"]]'],
    handler => sub {
      my ($rule, $ekb, $e) = @_;
      
      my $e_id = $e->getAttribute('id');

      # arg2
      my $x_id = $e->findvalue('arg2/@id');
      my $x = $ekb->get_assertion($x_id, "TERM")
	or return 0;
      my $x_type = get_slot_value($x, 'type');
      $ont_trips->is_a($x_type, "ONT::MAN-MADE-STRUCTURE")
	or return 0;
      
      INFO "Rule %s matches event %s (x: %s)",
	$rule->name(), $e_id, $x_id;

      $ekb->modify_assertion( $e,
			      make_slot_node(type => "ONT::BUILD")
			      );
      1;
    }
   },
   
   ###
   {
    ## X provides/offers/creates a source of Y
    # N.B.: need to distinguish b/w "increase" and "increase availability of"
    #   (or the closely related "increase access to")
    #   i'm assuming the former if Y is under ONT::ORDERED-DOMAIN (~~ scalar property)
    # e=EVENT[type=ONT::SUPPLY, agent=x, affected=t]
    # t=TERM[type=ONT::SOURCE, assoc-with=y[type->>ONT::ORDERED-DOMAIN]]
    # =>
    # EVENT[type=ONT::INCREASE, agent=x, affected=y]
    name => "EKR:SupplySource",
    constraints => ['EVENT[type[.="ONT::SUPPLY"] and arg1 and arg2[@role=":AFFECTED"]]'],
    handler => sub {
      my ($rule, $ekb, $a) = @_;
      my $a_id = $a->getAttribute('id');

      # arg1
      my $x_id = $a->findvalue('arg1/@id');
 
      # arg2 = TERM[type=ONT::SOURCE and assoc-with[@id=Y]]
      # NB: should be ONT::SOURCE but it may be (incorrectly) ONT::DEVICE-COMPONENT
      # NB: role may also be :AFFECTED-RESULT -- not handled yet! **FIXME**
      my $t_id = $a->findvalue('arg2/@id');
      my $t = $ekb->get_assertion($t_id, "TERM")
	or return 0;
      my $t_type = get_slot_value($t, 'type');
      $ont_trips->is_a($t_type, "ONT::SOURCE") || ($t_type eq "ONT::DEVICE-COMPONENT")
        or return 0;

      my $y_id = $t->findvalue('assoc-with/@id')
	or return 0;
      my $y = $ekb->get_assertion($y_id, "TERM")
	or return 0;

      # y = TERM[type->ONT::ORDERED-DOMAIN]
      my $y_type = get_slot_value($y, 'type');
      $ont_trips->is_a($y_type, "ONT::ORDERED-DOMAIN") 
        or return 0;

      INFO "Rule %s matches event %s (y: %s)",
	$rule->name(), $a_id, $y_id;

      # add assertion
      my $e1 = $ekb->clone_assertion($a, {rule => $rule->name});
      set_slot_value($e1, 'type' => 'ONT::INCREASE');
      $ekb->replace_arg($e1, ':AFFECTED' => $y_id);
      my $e1_id = $ekb->add_assertion($e1);

      1;
      }
   },

   ### temporal arguments
   {
    ## numbers with modifiers, e.g., "over 10,000"
    name => "EKR:NumberMod",
    constraints => ['TERM[type[.="ONT::NUMBER"] and value[@id and @mod]]'],
    handler => sub {
      my ($rule, $ekb, $t) = @_;
      
      my $t_id = $t->getAttribute('id');

      my ($val) = $t->findnodes('value');
      my $tn_id = $val->getAttribute('id');
      my $tn_term = $ekb->get_assertion($tn_id, "TERM")
	or return 0;
      
      match_node( $tn_term,
		  { SX => { 'type' => "ONT::NUMBER",
			    'value' => {} }
		  } )
	or return 0;

      my ($tn_value) = $tn_term->findnodes('value');
      # must have some actual value
      $tn_value->hasChildNodes()
	or return 0;
      
      INFO "Rule %s matches term %s (t: %s)",
	$rule->name(), $t_id, $tn_id;

      # getmod
      my $mod = $val->getAttribute('mod');
      # set value
      my $new_val = clone_node($tn_value);
      $new_val->setAttribute('mod', $mod);
      $t->removeChild($val);
      $ekb->modify_assertion( $t,
      			      { rule => $rule->name, refid => $tn_id },
			      $new_val
			    );
      DEBUG 2, "Asserted: %s", $t;

      1;
    }
   },
   
   {
    ## e.g., "the year 2016"
    name => "EKR:TimeIdentifiedAs",
    constraints => ['TERM[type[.="ONT::YEAR"] and equals[@id] and scale[.="ONT::TIME-LOC-SCALE"]]'],
    handler => sub {
      my ($rule, $ekb, $t) = @_;
      
      my $t_id = $t->getAttribute('id');
      
      my $tl_id = getvalue_xpath($t, 'equals/@id');
      my $tl_term = $ekb->get_assertion($tl_id, "TERM");
      match_node( $tl_term,
		  { SX => { 'type' => "ONT::TIME-LOC",
			    'timex' => {} } }
		)
	or return 0;

      INFO "Rule %s matches term %s (t: %s)",
	$rule->name(), $t_id, $tl_id;

      # remove scale
      my ($s) = $t->findnodes('scale[.="ONT::TIME-LOC-SCALE"]');
      $t->removeChild($s);
      
      # copy slots
      $ekb->modify_assertion( $t,
			      { rule => $rule->name, refid => $tl_id },
			      make_slot_node(type => get_slot_value( $tl_term, "type")),
			      clone_node($tl_term->findnodes('timex'))
			    );
      DEBUG 2, "Asserted: %s", $t->toString;
      
      1;
    }
   },
   
   {
    ## e.g., "the month of March"
    name => "EKR:TimeMonthOf",
    constraints => ['TERM[type[.="ONT::MONTH"] and assoc-with]'],
    handler => sub {
      my ($rule, $ekb, $t) = @_;

      my $t_id = $t->getAttribute('id');

      # get assoc-with node(s) and look for one that is a TIME-LOC
      my $tl_id;
      my $assoc;
      my @assocs = $t->findnodes('assoc-with');
      DEBUG 0, "Found %d assocs: %s", scalar(@assocs), join(", ", map {$_->toString} @assocs);
      foreach my $a (@assocs) {
	my $a_id = $a->getAttribute('id');
	my $a_term = $ekb->get_assertion($a_id, "TERM")
	  or next;
	my $m = match_node( $a_term,
			    { SX => { 'type' => "ONT::TIME-LOC",
				      'timex' => {} } }
			  );
	if ($m) {
	  if ($tl_id) { # shouldn't happen!
	    WARN "Duplicate TIME-LOCs: %s and %s", $tl_id, $a_id;
	  } else {
	    $tl_id = $a_id;
	    $assoc = $a;
	  }
	}
      }
      
      return 0 unless $tl_id;
      
      INFO "Rule %s matches term %s (t: %s)",
	$rule->name(), $t_id, $tl_id;

      my $tl_term = $ekb->get_assertion($tl_id, "TERM");

      # clone timex and add mod
      my ($timex) = $tl_term->findnodes('timex');
      my $new_timex = clone_node($timex);
      $new_timex->setAttribute("type", "DURATION");

      # remove assoc
      $t->removeChild($assoc);
      
      $ekb->modify_assertion( $t,
			      { rule => $rule->name, refid => $tl_id },
			      make_slot_node(type => get_slot_value( $tl_term, "type")),
			      $new_timex
			    );
      DEBUG 2, "Asserted: %s", $t;

      1;
    }
   },

   {
    ## e.g., "the first half of March"
    name => "EKR:TimePart",
    constraints => ['TERM[type[.="ONT::PART"] and assoc-with]'],
    handler => sub {
      my ($rule, $ekb, $t) = @_;

      my $t_id = $t->getAttribute('id');

      # get assoc-with node(s) and look for one that is a TIME-LOC
      my $tl_id;
      my $assoc;
      my @assocs = $t->findnodes('assoc-with');
      DEBUG 0, "Found %d assocs: %s", scalar(@assocs), join(", ", map {$_->toString} @assocs);
      foreach my $a (@assocs) {
	my $a_id = $a->getAttribute('id');
	my $a_term = $ekb->get_assertion($a_id, "TERM")
	  or next;
	my $m = match_node( $a_term,
			    { SX => { 'type' => "ONT::TIME-LOC",
				      'timex' => {} } }
			  );
	if ($m) {
	  if ($tl_id) { # shouldn't happen!
	    WARN "Duplicate TIME-LOCs: %s and %s", $tl_id, $a_id;
	  } else {
	    $tl_id = $a_id;
	    $assoc = $a;
	  }
	}
      }
      
      return 0 unless $tl_id;
      
      INFO "Rule %s matches term %s (t: %s)",
	$rule->name(), $t_id, $tl_id;

      my $tl_term = $ekb->get_assertion($tl_id, "TERM");

      # clone timex and add mod
      my ($timex) = $tl_term->findnodes('timex');
      my $new_timex = clone_node($timex);
      my $m = getvalue_xpath($t, 'text/@normalization');
      $new_timex->setAttribute("mod", $m);
      $new_timex->setAttribute("type", "DURATION");

      # remove assoc
      $t->removeChild($assoc);
      
      $ekb->modify_assertion( $t,
			      { rule => $rule->name, refid => $tl_id },
			      make_slot_node(type => get_slot_value( $tl_term, "type")),
			      $new_timex
			    );
      DEBUG 2, "Asserted: %s", $t;

      1;
    }
   },

   ## the first 2 weeks of March
   # ...

   {
    ## "the beginning of 2015", "the start of the week"
    # < EVENT[@id=$t_id and assoc-with[@id=$tl_id] and type=$type]
    #    $type=one-of(ONT::START, ???)
    # < TERM[@id=$tl_id and type=ONT::TIME-LOC]
    # > + TERM[@id=$t_id]/ as near-clone of EVENT[@id=$t_id]
    #     - assoc-with
    #     ~ type=ONT::TIME-LOC
    #     + timex=TERM[@id=$tl_id]/timex
    #     + timex/@mod=$type
    # > - TERM[@id=$tl_id]
    # > - EVENT[@id=$t_id]
    name => "EKR:TimeModStart",
    constraints => ['EVENT[type[.="ONT::START"] and assoc-with]'],
    handler => sub {
      my ($rule, $ekb, $e) = @_;

      my $e_id = $e->getAttribute('id');
      my $tl_id;

      # get assoc-with node(s) and look for one that is a TIME-LOC
      my @assocs = $e->findnodes('assoc-with');
      DEBUG 0, "Found %d assocs: %s", scalar(@assocs), join(", ", map {$_->toString} @assocs);
      foreach my $a (@assocs) {
	my $a_id = $a->getAttribute('id');
	my $a_term = $ekb->get_assertion($a_id, "TERM")
	  or next;
	my $m = match_node( $a_term,
			    { SX => { 'type' => "ONT::TIME-LOC",
				      'timex' => {} } }
			  );
	if ($m) {
	  if ($tl_id) { # shouldn't happen!
	    WARN "Duplicate TIME-LOCs: %s and %s", $tl_id, $a_id;
	  } else {
	    $tl_id = $a_id;
	  }
	}
      }
      
      return 0 unless $tl_id;
      
      INFO "Rule %s matches event %s (t: %s)",
	$rule->name(), $e_id, $tl_id;

      my $tl_term = $ekb->get_assertion($tl_id, "TERM");
      
      # clone timex and add mod
      my ($timex) = $tl_term->findnodes('timex');
      my $new_timex = clone_node($timex);
      $new_timex->setAttribute("mod", "START");

      @assocs = grep { $_->getAttribute('id') ne $tl_id } @assocs;
      
      my $t = $ekb->derive_assertion( $e_id, 'TERM', 1 );
      $ekb->add_assertion($t);
      $ekb->modify_assertion( $t,
			      { rule => $rule->name, refid => $tl_id },
			      make_slot_nodes(type => get_slot_value( $tl_term, "type"),
					      spec => get_slot_value( $tl_term, "spec")),
			      map { clone_node($_) } @assocs,
			      $new_timex,
			    );
      DEBUG 2, "Asserted: %s", $t->toString;
     
      1;
    }
   },

   {
    ## e.g., "the end of 2015", "the end of April"
    # < TERM[@id=$t_id and assoc-with[@id=$tl_id] and type=$type]
    #    $type=one-of(ONT::END-LOCATION, ???)
    # < TERM[@id=$tl_id and type=ONT::TIME-LOC]
    # > ~ TERM[@id=$t_id]/
    #     - assoc-with
    #     ~ type=ONT::TIME-LOC
    #     + timex=TERM[@id=$tl_id]/timex
    #     + timex/@mod=$type
    # > - TERM[@id=$tl_id]
    name => "EKR:TimeModEnd",
    constraints => ['TERM[type[.="ONT::END-LOCATION"] and assoc-with]'],
    handler => sub {
      my ($rule, $ekb, $t) = @_;

      my $t_id = $t->getAttribute('id');

      # get assoc-with node(s) and look for one that is a TIME-LOC
      my $tl_id;
      my $assoc;
      my @assocs = $t->findnodes('assoc-with');
      DEBUG 0, "Found %d assocs: %s", scalar(@assocs), join(", ", map {$_->toString} @assocs);
      foreach my $a (@assocs) {
	my $a_id = $a->getAttribute('id');
	my $a_term = $ekb->get_assertion($a_id, "TERM")
	  or next;
	my $m = match_node( $a_term,
			    { SX => { 'type' => "ONT::TIME-LOC",
				      'timex' => {} } }
			  );
	if ($m) {
	  if ($tl_id) { # shouldn't happen!
	    WARN "Duplicate TIME-LOCs: %s and %s", $tl_id, $a_id;
	  } else {
	    $tl_id = $a_id;
	    $assoc = $a;
	  }
	}
      }
      
      return 0 unless $tl_id;
      
      INFO "Rule %s matches term %s (t: %s)",
	$rule->name(), $t_id, $tl_id;

      my $tl_term = $ekb->get_assertion($tl_id, "TERM");

      # clone timex and add mod
      my ($timex) = $tl_term->findnodes('timex');
      my $new_timex = clone_node($timex);
      $new_timex->setAttribute("mod", "END");

      # remove assoc
      $t->removeChild($assoc);
      
      $ekb->modify_assertion( $t,
			      { rule => $rule->name, refid => $tl_id },
			      make_slot_nodes(type => get_slot_value( $tl_term, "type"),
					      spec => get_slot_value( $tl_term, "spec")),
			      $new_timex
			    );
      DEBUG 2, "Asserted: %s", $t;

      1;
    }
   },

   {
    ## fix: location => time 
    ## order: after all time-related rules
    name => "EKR:LocationToTime",
    constraints => ['*/location'],
    handler => sub {
      my ($rule, $ekb, $a) = @_;

      my $a_id = $a->getAttribute('id');

      my @locs = $a->findnodes("location");

      my $count = 0;
      foreach my $l (@locs) {
	my $tl_id = $l->getAttribute('id')
	  or next;
	my $tl_term = $ekb->get_assertion($tl_id, "TERM")
	  or next;
	match_node( $tl_term, { SX => { 'type' => "ONT::TIME-LOC" } } )
	  or next;

	INFO "Rule %s matches assertion %s (t: %s)",
	  $rule->name(), $a_id, $tl_id;

	$a->removeChild($l);
	$ekb->modify_assertion( $a,
				{ rule => $rule->name, refid => $tl_id },
				make_node("time", { id => $tl_id }) );

	$count++;
      }
      
      $count;
    }
   },

   ### locations
   {
    ## fix: result => location
    name => "EKR:EResultIsLoc",
    constraints => ['EVENT[result/@id and location/@id]'],
    handler => sub  {
      my ($rule, $ekb, $e) = @_;
      
      my $e_id = $e->getAttribute('id');

      my @r = $e->findnodes('result[@id]');
      my $count = 0;
      foreach my $s (@r) {
	my $t_id = $s->getAttribute('id');
	my $t = $ekb->get_assertion($t_id, "TERM")
	  or next;
	my ($tl) = $e->findnodes('location[@id="'.$t_id.'"]')
	  or next;

	INFO "Rule %s matches term %s (t: %s)",
	  $rule->name(), $e_id, $t_id;

	my $mod = $s->getAttribute('mod');
	# put mod=>$mod in the location
	if ($mod && !$tl->getAttribute('mod')) {
	  set_attribute($tl, "mod", $mod);
	}
	$e->removeChild($s);
	$ekb->modify_assertion( $e, { rule => $rule->name } );

	$count++
      }
      
      $count;
    }
   },

   {
    ## fix: ARRIVE location => to-location
    name => "EKR:ArriveIn",
    constraints => ['EVENT[type[.="ONT::ARRIVE"] and location[@id and @mod]]'],
    handler => sub  {
      my ($rule, $ekb, $e) = @_;
      
      my $e_id = $e->getAttribute('id');

      my ($s) = $e->findnodes('location[@id]'); 
      my $mod = $s->getAttribute('mod');
      any { $mod eq $_ } qw/ONT::IN-LOC/
	or return 0;
      my $t_id = $s->getAttribute('id');
      my $t = $ekb->get_assertion($t_id, "TERM")
	or return 0;
      $ont_geo->has(get_slot_value($t, "type"))
	or return 0;

      # make sure we don't have it already
      match_node( $e, { SX => { 'to-location' => $t_id } } )
	and return 0;

      INFO "Rule %s matches term %s (t: %s)",
	$rule->name(), $e_id, $t_id;

      $e->removeChild($s);
      $ekb->modify_assertion( $e,
			      { rule => $rule->name, refid => $t_id },
			      make_node("to-location", { id => $t_id,
							 mod => $mod }) );

      1;
    }
   },
   
   {
    ## fix: DEPART location => from-location
    name => "EKR:DepartN",
    constraints => ['EVENT[type[.="ONT::DEPART"] and arg2[@role=":NEUTRAL"]]'],
    handler => sub  {
      my ($rule, $ekb, $e) = @_;
      
      my $e_id = $e->getAttribute('id');

      my ($arg) = $e->findnodes('arg2');
      my $t_id = $e->findvalue('arg2/@id');
      my $t = $ekb->get_assertion($t_id, "TERM")
        or return 0;
      my $t_type = get_slot_value($t, 'type');
      $ont_geo->has(get_slot_value($t, "type"))
	or return 0;

      # make sure we don't have it already
      match_node( $e, { SX => { 'from-location' => $t_id } } )
	and return 0;

      INFO "Rule %s matches term %s (t: %s)",
	$rule->name(), $e_id, $t_id;

      $e->removeChild($arg);
      $ekb->modify_assertion( $e,
			      { rule => $rule->name, refid => $t_id },
			      make_node("from-location", { id => $t_id }) );

      1;
    }
   },
   
   {
    ## fix: location => from-location
    name => "EKR:ELoc2FromLoc",
    constraints => ['EVENT[location[@id and @mod="ONT::FROM-LOC"]]'],
    handler => sub  {
      my ($rule, $ekb, $e) = @_;
      
      my $e_id = $e->getAttribute('id');

      my ($s) = $e->findnodes('location[@id]'); 
      my $mod = $s->getAttribute('mod');
      my $t_id = $s->getAttribute('id');
      my $t = $ekb->get_assertion($t_id, "TERM")
	or return 0;
      $ont_geo->has(get_slot_value($t, "type"))
	or return 0;

      # make sure we don't have it already
      match_node( $e, { SX => { 'from-location' => $t_id } } )
	and return 0;

      INFO "Rule %s matches term %s (t: %s)",
	$rule->name(), $e_id, $t_id;

      $e->removeChild($s);
      $ekb->modify_assertion( $e,
			      { rule => $rule->name, refid => $t_id },
			      make_node("from-location", { id => $t_id,
							   mod => $mod }) );

      1;
    }
   },
   
   {
    ## fix: location => to-location
    name => "EKR:ELoc2ToLoc",
    constraints => ['EVENT[location[@id and @mod]]'],
    handler => sub  {
      my ($rule, $ekb, $e) = @_;
      
      my $e_id = $e->getAttribute('id');

      my ($s) = $e->findnodes('location[@id]'); 
      my $mod = $s->getAttribute('mod');
      any { $mod eq $_ } qw/ONT::TO-LOC ONT::GOAL-AS-CONTAINMENT/
	or return 0;
      my $t_id = $s->getAttribute('id');
      my $t = $ekb->get_assertion($t_id, "TERM")
	or return 0;
      $ont_geo->has(get_slot_value($t, "type"))
	or return 0;

      # make sure we don't have it already
      match_node( $e, { SX => { 'to-location' => $t_id } } )
	and return 0;

      INFO "Rule %s matches term %s (t: %s)",
	$rule->name(), $e_id, $t_id;

      $e->removeChild($s);
      $ekb->modify_assertion( $e,
			      { rule => $rule->name, refid => $t_id },
			      make_node("to-location", { id => $t_id,
							 mod => $mod }) );

      1;
    }
   },
   
   {
    ## fix: source => from-location
    name => "EKR:ESource2FromLoc",
    constraints => ['EVENT[source/@id and not(from-location)]'],
    handler => sub  {
      my ($rule, $ekb, $e) = @_;
      
      my $e_id = $e->getAttribute('id');

      my ($s) = $e->findnodes('source[@id]'); 
      my $t_id = $s->getAttribute('id');
      my $t = $ekb->get_assertion($t_id, "TERM")
	or return 0;
      $ont_geo->has(get_slot_value($t, "type"))
	or return 0;

      match_node( $e, { SX => { 'from-location' => $t_id } } )
	and return 0;

      my $mod = $s->getAttribute('mod');
      if ($mod) {
	any { $mod eq $_ } qw/FROM/
	  or return 0;
      }
      
      INFO "Rule %s matches term %s (t: %s; mod: %s)",
	$rule->name(), $e_id, $t_id, $mod;

      $e->removeChild($s);
      $ekb->modify_assertion( $e,
			      { rule => $rule->name, refid => $t_id },
			      make_node("from-location", { id => $t_id, mod => $mod }) );

      1;
    }
   },
   
   {
    ## fix: result(TO) => to-location
    name => "EKR:EResult2ToLoc",
    constraints => ['EVENT[result[@id and @mod]]'],
    handler => sub  {
      my ($rule, $ekb, $e) = @_;
      
      my $e_id = $e->getAttribute('id');

      my ($s) = $e->findnodes('result[@id and @mod]'); 
      my $t_id = $s->getAttribute('id');
      my $t = $ekb->get_assertion($t_id, "TERM")
	or return 0;
      $ont_geo->has(get_slot_value($t, "type"))
	or return 0;

      match_node( $e, { SX => { 'to-location' => $t_id } } )
	and return 0;

      my $mod = $s->getAttribute('mod');
      any { $mod eq $_ } qw/TO INTO/
	or return 0;


      INFO "Rule %s matches term %s (t: %s)",
	$rule->name(), $e_id, $t_id;

      ## NOT USED -- only issues a warning
      #WARN "Rule %s not applied.", $rule->name();
      #return 0;

      $e->removeChild($s);
      $ekb->modify_assertion( $e,
			      { rule => $rule->name, refid => $t_id },
			      make_node("to-location", { id => $t_id,
							 mod => $mod }) );

      1;
    }
   },

   {
    ## fix: result => from-location
    name => "EKR:EResult2FromLoc",
    constraints => ['EVENT[result[@id and @mod="FROM"] and not(from-location)]'],
    handler => sub  {
      my ($rule, $ekb, $e) = @_;
      
      my $e_id = $e->getAttribute('id');

      my ($s) = $e->findnodes('result[@id]'); 
      my $t_id = $s->getAttribute('id');
      my $t = $ekb->get_assertion($t_id, "TERM")
	or return 0;
      $ont_geo->has(get_slot_value($t, "type"))
	or return 0;

      match_node( $e, { SX => { 'from-location' => $t_id } } )
	and return 0;

      INFO "Rule %s matches term %s (t: %s)",
	$rule->name(), $e_id, $t_id;

      my $mod = $s->getAttribute('mod');
      $e->removeChild($s);
      $ekb->modify_assertion( $e,
			      { rule => $rule->name, refid => $t_id },
			      make_node("from-location", { id => $t_id, mod => $mod }) );

      1;
    }
   },

   {
    ## risk of, problem of as CC factors
    # < (CC A :factor X)
    # < (EVENT X ONT::TROUBLE :assoc-with Y)
    # > (CC A1 :factor Y)
    name => "EKR:TroubleFactor",
    constraints => ['CC and arg[@role=":FACTOR"]'],
    handler => sub {
      my ($rule, $ekb, $a) = @_;
      my $a_id = $a->getAttribute('id');
      my $x_id = $a->findvalue('arg[@role=":FACTOR"]/@id');
      my $x = $ekb->get_assertion($x_id, "EVENT")
	or return 0;
      my $x_type = get_slot_value($x, "type");
      $ont_trips->is_a($x_type, "ONT::TROUBLE")
	or return 0;
      my $y_id = $x->findvalue('assoc-with/@id')
	or return 0;
      
      INFO "Rule %s matches assertion %s (x: %s/%s, y: %s)",
	$rule->name(), $a_id, $x_id, $x_type, $y_id;

      # add assertion
      my $a1 = $ekb->clone_assertion($a, {rule => $rule->name});
      $ekb->replace_arg($a1, ':FACTOR' => $y_id);
      $ekb->add_assertion($a1);

      1;
      }
    },

   ### generic clean-up rules
   
   {
    ## remove dangling references on relation arguments
    # < X:EVENT()|CC()|EPI()|MODALITY()
    # < X/arg[@id=id]
    # < ! *[@id=id]
    # > - X/arg/@id
    name => "EKR:RemoveDanglingReferences",
    constraints => [],
    handler => sub  {
      my ($rule, $ekb, $r) = @_;

      return 0 unless is_relation($r);

      my $r_id = $r->getAttribute('id');

      my @args = assertion_args($r);
      my $count = 0;
      foreach my $arg (@args) {
	my $a_id = $arg->getAttribute('id');
	next if $ekb->get_assertion($a_id);
	INFO "Rule %s matches %s (arg: %s)",
	  $rule->name, $r_id, $a_id;
	$arg->removeAttribute('id');
	$count++;
      }

      $count;
    }
   },
   
   {
    ## delete CC relations that don't have two arguments and lack referrers
    # < C:CC(!arg[2])
    # < ! *(*:C)
    # > -C
    # WARNING: removes assertions
    name => "EKR:RemoveIncompleteCC",
    constraints => ['CC[not(arg[2])]', '_DO_NOT_USE_'],
    handler => sub {
      my ($rule, $ekb, $c) = @_;

      my $c_id = $c->getAttribute('id');

      # < C:CC(!arg[2])
      my @args = assertion_args($c);
      return 0 if (scalar(@args) >= 2);

      # < ! *(*:C)
      my @referrers =
	grep { $_->getAttribute('id') ne $c_id }
	$ekb->find_referrers($c_id);
      return 0 unless (scalar(@referrers) == 0);

      INFO("Rule %s matches %s",
	   $rule->name(), $c_id);
	
      # > -C
      $c->parentNode->removeChild($c);

      1;
    }
   },

   {
    ## delete empty relations (no arguments of any kind) with no referrers
    # < X:reln()
    # < ! *(*:X)
    # > - X
    # WARNING: removes assertions
    name => "EKR:RemoveBareReln",
    constraints => [],
    repeat => 1,
    handler => sub  {
      my ($rule, $ekb, $r) = @_;

      return 0 unless is_relation($r);

      my $r_id = $r->getAttribute('id');

      # < X:reln()
      my @args = ( assertion_args($r), assertion_xargs($r) );
      return 0 unless (scalar(@args) == 0);
	
      # < ! *(*:X)
      my @referrers =
	grep { $_->getAttribute('id') ne $r_id }
	$ekb->find_referrers($r_id);
      return 0 unless (scalar(@referrers) == 0);

      # > - X
      INFO("Rule %s matches %s",
	   $rule->name, $r_id);
	
      $ekb->remove_assertion($r);

      1;
    }
   },
   
   {
    ## Remove dangling 'inevent' features (event doesn't exist or it doesn't
    ## have an arg pointing to the term)
    # < TERM[@id=$t_id and //inevent[@id=$e]]
    #   ! *[@id=$e and arg*[@id=$t_id]]
    # > - TERM//inevent[@id=$e]
    name => "EKR:FixDanglingInevent",
    constraints => ['TERM[features/inevent]'],
    handler => sub {
      my ($rule, $ekb, $t) = @_;
	
      my $t_id = $t->getAttribute('id');
      
      my @e_ids = map { $_->value } $t->findnodes('features/inevent/@id');
      my $count = 0;
      foreach my $e_id (@e_ids) {
	my $e = $ekb->get_assertion($e_id);
	my @args = ( assertion_args($e), assertion_xargs($e) );
	next if any { $_->getAttribute('id') eq $t_id } @args;

	INFO "Rule %s matches term %s (inevent: %s)",
	  $rule->name(), $t_id, $e_id;

	remove_elements($t, 'features/inevent[@id="'.$e_id.'"]');
	    
	$count++;
      }
      $count;
    }
   },

   {
    ## when <mod><type>ONT::NEG</type><value>NOT</value></mod>
    ## flip <negation>
    # < E[mods/mod[type=ONT::NEG and value=NOT]]
    # > - E/mods/mod[type=ONT::NEG and value=NOT]
    # > ~ E[negation=flip()]
    name => 'EKR:ModNegNot',
    constraints => ['*/mods/mod[type[.="ONT::NEG"] and value[.="NOT"]]'],
    handler => sub {
      my ($rule, $ekb, $r) = @_;

      return 0 unless is_relation($r);

      my $r_id = $r->getAttribute('id');

      INFO("Rule %s matches %s",
	   $rule->name, $r_id);

      # > ~ E[negation=flip()]
      my $neg = get_slot_value($r, 'negation');
      if (defined $neg) {
	if ($neg eq '+') {
	  $r->set_slot_value('negation', '-');
	} else {
	  $r->set_slot_value('negation', '+');
	}
      } else {
	$r->addChild(make_slot_node( negation => '+' ));
      }

      # > - E:/mods/mod[type=ONT::NEG and value=NOT]
      remove_elements($r, 'self::*/mods/mod[type[.="ONT::NEG"] and value[.="NOT"]]');

      1;
    }
   },
   
   {
    ## obtain localization event from term w/ location info
    # < TERM[@id=$t_id and features/location/@id=$l_id]
    # > EVENT[type=ONT::LOCALIZATION
    #         pred=ONT::BE-AT-LOC
    #         arg[@id=$t_id]
    #         location[@id=$l_id]]
    # WARNING: adds assertions
    name => "EKR:LocateTerm",
    constraints => ['TERM[features/location]'],
    handler => sub {
      my ($rule, $ekb, $t) = @_;
	
      my $t_id = $t->getAttribute('id');

      # get location(s)
      my @loc_ids = map {$_->value} $t->findnodes('features/location/@id');

      my @loc_terms =
	grep { defined $_ } map { $ekb->get_assertion($_, "TERM") } @loc_ids;
		     
      my $start = min ( $t->getAttribute('start'),
			map { $_->getAttribute('start') }
			@loc_terms );
      my $end = max ( $t->getAttribute('end'),
		      map { $_->getAttribute('end') }
		      @loc_terms );

      # TODO: perhaps, instead of making multiple events, i should make
      # an aggregate location?? but then why shouldn't the original term have
      # a single location feature pointing to the aggregate??
      foreach my $loc_term (@loc_terms) {
	my $l_id = $loc_term->getAttribute('id');
	$ekb->infer_assertion( 'EVENT',
			       { refid => $t_id,
				 start => $start,
				 end => $end,
				 rule => $rule->name },
			       make_slot_node(type => 'ONT::LOCALIZATION'),
			       make_predicate('ONT::BE-AT-LOC'),
			       make_arg(':NEUTRAL' => $t_id),
			       make_node("location", { id => $l_id }) );
      }
		     
      1;
    }
   },

   {
    ## distribute :EPI info in aggregate event assertions to member events
    # EVENT[type=S-CONJOINED and 
    #       aggregate/member[@id=$eid] and 
    #       epistemic-modality[@id=$mid]]
    # EVENT[@id=$id]
    # => mod: EVENT[@id=$id and epistemic-modality[@id=$mid]]
    # WARNING: adds EKB cross-refs
    name => "EKR:EpiConjoinedEvent",
    constraints => ['EVENT[aggregate and epistemic-modality]'],
    handler => sub {
      my ($rule, $ekb, $e) = @_;

      my $e_id = $e->getAttribute('id');
	
      # match aggregate
      match_node($e, { SX => { 'type' => [OP_OR,
					  "ONT::S-CONJOINED",
					  "ONT::VP-CONJOINED"],
			       'aggregate' => {},
			       'epistemic-modality' => {} } })
	or return 0;

      # get :epi node(s)
      my @epis = $e->findnodes('epistemic-modality');
    
      # get aggregate member ids
      my @m_ids = map { $_->value } $e->findnodes('aggregate/member/@id');
      return 0 unless @m_ids;

      INFO("Rule %s matches event %s w/ members (%s)", 
	   $rule->name(), $e_id, "@m_ids");

      foreach my $m_id (@m_ids) {
	my $m_e = $ekb->get_assertion($m_id, "EVENT");
	DEBUG(1, "%s", $m_e);
	next unless $m_e;
	my ($pred) = $m_e->findnodes('predicate');
	foreach my $epi (@epis) {
	  my $c_epi = $epi->cloneNode(1);
	  $m_e->insertBefore($c_epi, $pred);
	  DEBUG(1, "%s", $m_e);
	}
      }

      1;
    }
   },

   {
    ## delete terms not referenced elsewhere
    # < T:TERM(id=$t_id)
    # < ! *[...id=$t_id...]
    # > - T
    # WARNING: removes assertions
    name => 'EKR:RemoveOrphanTerm',
    constraints => ['TERM'],
    handler => sub {
      my ($rule, $ekb, $t) = @_;

      my $t_id = $t->getAttribute('id');

      # < ! *(...id=X...)
      my @referrers = $ekb->find_referrers($t_id);
      return 0 unless (scalar(@referrers) == 0);
  
      INFO("Rule %s matches term %s",
	   $rule->name(), $t_id);
      
      $ekb->remove_assertion($t);

      1;
    }
   },

   ### TEMPORARY fixes
   
   {
    ## fix timex expressions for TIME-RANGE terms
    name => 'EKR:FixTimeRangeTimex',
    constraints => ['TERM[type="ONT::TIME-RANGE" and timex/from and timex/to]'],
    handler => sub {
      my ($rule, $ekb, $t) = @_;

      my $t_id = $t->getAttribute('id');

      INFO("Rule %s matches term %s",
	   $rule->name(), $t_id);

      my ($tx) = $t->findnodes('timex');
      my @tx_children = $tx->childNodes(); ## from and to
      foreach my $n (@tx_children) {
	$tx->removeChild($n);
	my $name = $n->nodeName;
	$n->setNodeName( $name . "-time" );
	$t->addChild($n);
      }
      $t->removeChild($tx);

      1;
    }
   },

   ### ontology mappings
   {
    name => 'EKR:AddWMOntTypes',
    constraints => ['*[not(wm-type)]'],
    handler => sub {
      my ($rule, $ekb, $a) = @_;
      
      my $a_id = $a->getAttribute('id');
      my $a_type = get_slot_value($a, "type");
      # extra info: pred type, lex, gid, gname
      my $info = {};
      # for relns, use text normalization to get lexeme
      my ($pred, $lex) = (undef, undef);
      ($pred) = $a->findnodes('predicate');
      my $p_type = get_slot_value($pred, "type") if $pred;
      $info->{pred} = $p_type if $p_type;
      ($lex) = getvalue_xpath($pred // $a, 'text/@normalization') ;
      $info->{lex} = $lex if $lex;
      my ($gid) = getvalue_xpath($a, 'grounding/term/@id'); # first term
      $info->{gid} = $gid if $gid;      
      # my ($gname) = $a->findnodes('grounding/term/@name'); # first term

      my $wm_mapping = $ont_trips->map($a_type, $info)
	or return 0;

      INFO "Rule %s matches assertion %s (type=%s, {%s} => %s)",
	$rule->name(), $a_id, $a_type, join(",", map { "$_:$info->{$_}" } keys %$info),
	Dumper($wm_mapping);

      my $tn = get_child_node($a, "type");
      my $wm_node = make_wm_node($wm_mapping);
      $ekb->modify_assertion( $a, $wm_node );
      
      1;
    }
   },

   {
    name => 'EKR:AddWMOntTypesQuals',
    constraints => ['*[qualifiers]'],
    handler => sub {
      my ($rule, $ekb, $a) = @_;

      my $a_id = $a->getAttribute('id');
      my @qq = $a->findnodes('qualifiers/qual');

      my $count = 0;
      foreach my $q (@qq) {
	my $q_type = get_slot_value($q, "type");

	my $wm_mapping = $ont_trips->map($q_type)
	  or next;

	INFO "Rule %s matches assertion %s (qual type=%s)",
	  $rule->name(), $a_id, $q_type;

	my $tn = get_child_node($q, "type");
	my $wm_node = make_wm_node($wm_mapping);
	$q->addChild( $wm_node );
        $count++;
      }
      
      $count;
    }
   }

  );

1;
