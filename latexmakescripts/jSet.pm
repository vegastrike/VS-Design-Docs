# Simple set-of-scalars object
#
# Copyright 2008 Jeff Brown.  This software is distributed under the terms of
# the GNU General Public License.
#
# JAB
# $Id$

package jSet;

use strict;
use warnings;
use Carp;

#use Exporter qw(import);
#our @EXPORT = qw();
#our @EXPORT_OK = qw();


# the object is currently a blessed hash-reference, with the actual scalars
# stored as the keys of that hash (and values "undef").  there are no other
# object fields.

sub new {
  my $class = shift;
  if (@_ != 0) { confess "usage: new $class()"; }
  #my ($args) = @_;
  my $self = { };
  bless($self, $class);
  return $self;
}

# (compare ->size() with scalar(@list) to test input uniqueness.)
sub new_from_list {
  my $class = shift;
  my $self = new jSet();
  foreach my $elt (@_) { $$self{$elt} = undef; }
  return $self;
}

# (compare ->size() with scalar(@$listref) to test input uniqueness.)
sub new_from_listref {
  my $class = shift;
  if (@_ != 1) { confess "usage: new $class(listref)"; }
  my ($listref) = @_;
  my $self = new jSet();
  foreach my $elt (@$listref) { $$self{$elt} = undef; }
  return $self;
}

sub DESTROY {
  my $self = shift;
  # (placeholder)
}

sub insert {
  my $self = shift;
  foreach my $elt (@_) { $$self{$elt} = undef; }
  return $self;
}

sub insert_listref {
  my $self = shift;
  if (@_ != 1) { confess "usage: obj->insert_listref(listref)"; }
  my $listref = shift;
  foreach my $elt (@$listref) { $$self{$elt} = undef; }
  return $self;
}

sub test {
  my $self = shift;
  if (@_ != 1) { confess "usage: obj->test(elt)"; }
  my $elt = shift;
  return exists($$self{$elt}) ? 1 : 0;
}

sub erase {
  my $self = shift;
  foreach my $elt (@_) { delete $$self{$elt}; }
  return $self;
}

sub size {
  my $self = shift;
  if (@_ != 0) { confess "usage: obj->size()"; }
  return scalar(keys %$self);
}

sub empty {
  my $self = shift;
  if (@_ != 0) { confess "usage: obj->empty()"; }
  return (scalar(keys %$self)) ? 0 : 1;
}

sub clear {
  my $self = shift;
  if (@_ != 0) { confess "usage: obj->clear()"; }
  %$self = ();
  return $self;
}

sub copy {
  my $self = shift;
  if (@_ != 0) { confess "usage: obj->copy()"; }
  my %copy = %$self;
  my $new = \%copy;
  bless($new, ref($self));
  return $new;
}

# returns an unsorted list (value, not reference) of all elements
sub list_all {
  my $self = shift;
  if (@_ != 0) { confess "usage: obj->list_all()"; }
  return keys %$self;
}

sub union {
  my $self = shift;
  if (@_ != 1) { confess "usage: obj->union(add_set)"; }
  my $add_set = shift;
  foreach my $elt (keys %$add_set) {
    $$self{$elt} = undef;
  }
  return $self;
}

sub intersect {
  my $self = shift;
  if (@_ != 1) { confess "usage: obj->intersect(and_set)"; }
  my $and_set = shift;
  if ($and_set->size() >= $self->size()) {
    my @to_del;
    foreach my $elt (keys %$self) {
      if (!exists($$and_set{$elt})) { push @to_del, $elt; }
    }
    foreach my $elt (@to_del) {
      delete $$self{$elt};
    }
  } else {
    my %new_elts;
    foreach my $elt (keys %$and_set) {
      if (exists($$self{$elt})) { $new_elts{$elt} = undef; }
    }
    %$self = %new_elts;
  }
  return $self;
}

sub subtract {
  my $self = shift;
  if (@_ != 1) { confess "usage: obj->subtract(minus_set)"; }
  my $minus_set = shift;
  my @to_del;
  foreach my $elt (keys %$self) {
    if (exists($$minus_set{$elt})) { push @to_del, $elt; }
  }
  foreach my $elt (@to_del) {
    delete $$self{$elt};
  }
  return $self;
}

return 1;
