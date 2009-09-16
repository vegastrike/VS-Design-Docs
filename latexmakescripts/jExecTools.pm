# Utility routines to call external commands, etc.
#
# Copyright 2008 Jeff Brown.  This software is distributed under the terms of
# the GNU General Public License.
#
# JAB
# $Id$

package jExecTools;

use strict;
use warnings;
use Carp;
use IO::File;


sub redirect_stdio($) {
  my ($redirects) = @_;
  if (defined($$redirects{stdin})) {
    close STDIN;
    open(STDIN, "<", $$redirects{stdin}) or
      confess "couldn't redirect stdin from $$redirects{stdin}: $!";
  }
  if (defined(my $out_to = $$redirects{stdout})) {
    my $result = ($out_to eq "stderr") ?
      open(STDOUT, ">&STDERR") : open(STDOUT, ">", $out_to);
    if (!$result) {
      confess "couldn't redirect stdout to $out_to: $!";
    }
    #open(STDOUT, ">", $$redirects{stdout}) or
    #  confess "couldn't redirect stdout to $$redirects{stdout}: $!";
  }
  if (defined(my $err_to = $$redirects{stderr})) {
    my $result = ($err_to eq "stdout") ?
      open(STDERR, ">&STDOUT") : open(STDERR, ">", $err_to);
    if (!$result) {
      confess "couldn't redirect stderr to $err_to: $!";
    }
  }
}


# run a command like system(), but without splitting or shell-parsing the
# command; optionally, call code just before exec to set up redirects
# and such.
#
# returns
#   can't fork/etc.: undef + warn
#   child killed: undef + warn
#   else: child exit() status
sub system_cmd($$) {
  my ($r_cmd, $child_preexec) = @_;
  my $pid = fork();
  my $result;
  if (!defined($pid)) {
    carp "couldn't fork: $!";
  } elsif ($pid) {		# parent
    my $kid = waitpid($pid, 0);
    if (!defined($kid) || ($kid != $pid)) {
      confess "unexpected waitpid($pid) return: " .
	(defined($kid) ? $kid : "undef") . " ($!)";
    }
    if (my $sig = ($? & 127)) {
      carp "child died, signal $sig";
    } else {
      $result = $? >> 8;
    }
  } else {			# child
    if (defined($child_preexec)) { &$child_preexec(); }
    { exec {$$r_cmd[0]} @$r_cmd; }
    confess "exec failed: $!";
  }
  return $result;
}


# run a command (without splitting or shell-parsing), collecting the
# output into a listref.  optionally, call code just before exec to
# set up redirects and such.
#
# returns
#   can't fork/etc.: undef + warn
#   child killed: undef + warn
#   else: listref [ exit status, line1, line2 ... ]
sub backtick_cmd($$) {
  my ($r_cmd, $child_preexec) = @_;
  my @lines;

  my $pid = open(CMD_KID, "-|");
  if (!defined($pid)) {
    carp "couldn't fork: $!";
    return undef;
  }

  if ($pid) {   # Parent: collect output from child
    @lines = <CMD_KID>;
  } else {      # Child: run command
    if (defined($child_preexec)) { &$child_preexec(); }
    { exec {$$r_cmd[0]} @$r_cmd; }
    confess "exec failed: $!";	# (in child)
  }

  my $exitstat;
  if (!close(CMD_KID)) {
    if ($!) {
      carp "error closing child pipe: $!";
    } elsif (my $sig = ($? & 127)) {
      carp "child died, signal $sig";
    } else {
      $exitstat = $? >> 8;
    }
  } else {
    $exitstat = 0;
  }
  if (!defined($exitstat)) { return undef; }
  unshift @lines, $exitstat;
  return \@lines;
}


return 1;
