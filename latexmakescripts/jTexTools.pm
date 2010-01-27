# Routines for interpreting and manipulating the output of tex/latex/bibtex
# and friends.  Mostly ad-hoc.
#
# Copyright 2008 Jeff Brown.  This software is distributed under the terms of
# the GNU General Public License.
#
# JAB
# $Id: jTexTools.pm,v 1.32 2010/01/27 23:07:32 jbrown Exp $

package jTexTools;

use strict;
use warnings;
use File::Basename;
use Carp;
use Data::Dumper;
use Exporter qw(import);

use jSet;

our @EXPORT = qw();
our @EXPORT_OK = qw(unsplit_latex_out parse_latex_out filter_latex_out
                    parse_bibtex_out filter_bibtex_out
                    REPORT_INFO REPORT_WARN REPORT_ERR);

use constant {
  DEFAULT_MAX_PRINT_LINE => 79,
  ENABLE_DEBUG_PRINT => 1,

  # constants indicating the type of each "report" record
  REPORT_INFO => 0,             # generic spam
  REPORT_WARN => 1,             # warnings (missing cites, non-fatal files...)
  REPORT_ERR => 2               # errors
};


# Note: the latex error message format seems to resemble:
# ! Error message
#   [maybe multiple lines of stuff, which don't obey latex's usual log
#   line-breaking rules, sigh sigh]
# l.<linenumber> [maybe some more stuff here]
#   [and maybe more stuff here, just in case you weren't confused enough]
# ?<space>


# debug print
sub _dp($) {
  my ($str) = @_;
  if (ENABLE_DEBUG_PRINT) { print "# " . $str . "\n"; }
}


# trim off leading "./" if present
sub trim_fname($) {
  my ($fname) = @_;
  my $result = $fname;
  while (substr($result, 0, 2) eq "./") {
    $result = substr($result, 2);
  }
  return $result;
}


# fix further pdftex brokenness, for gems like this:
# [15 <./Figures/accelerator.pdf, page is rotated 90 degrees>] (./paper.aux)
sub pdftex_fname_expletive($) {
  my ($fname) = @_;
  if ($fname =~ /^(.*), page is rotated \d+ degrees/) {
    # thanks for adding commentary _within_ the filename delimiting braces,
    # gang.
    $fname = $1;
  }
  return $fname;
}


# (we do this on the entire output instead of one line at a time, to allow
# for multi-line analysis in case we end up needing it later)
#
# in: listref of pre-chomped lines
# opt: hashref of options, or undef
#
# returns new listref of unsplit lines
sub unsplit_latex_out($$) {
  my ($in, $opts) = @_;
  if (!defined($opts)) { $opts = {}; }
  my $max_print_line = $$opts{max_print_line};  # tex config, from texmf.cnf
  if (!defined($max_print_line)) {
    $max_print_line = DEFAULT_MAX_PRINT_LINE;
  }

  my @out;
  LINE: for (my $in_idx = 0; $in_idx < @$in; $in_idx++) {
    # A guess at TeX's goofy line-continuation semantics: if we see a line
    # that's "max_print_line" chars long well then golly let's just guess that
    # it continues onto the next. :(
    #
    # (see texk/web2c/tex.web, procedures print_char and print)
    my $unsplit = "";
    CONT: for (;;) {
      my $line = $$in[$in_idx];
      my $next = $$in[$in_idx + 1];     # undef if past end
      $unsplit .= $line;
      my $is_contd = (length($line) == $max_print_line) &&
        defined($next);

      if ($is_contd && ($next =~ /^\[\d+/)) {
        # Additional pdflatex braindamage: when a page number print occurs
        # immediately after a long-line split point, the usual separating
        # whitespace/newline is omitted, e.g.
        #    ....................(./paper.bbl\n
        #    [6 <./Figures/...
        # ...unsplits to "...(./paper.bbl[6 <./Figures/", which confuses the
        # filename parser.  We'll attempt to hack around this by injecting a
        # space before the continuation line.
        $unsplit .= " ";
      }

      if (!$is_contd && ($line =~ /^[{<(]/)) {
        # Further tex trainwreckery: pdftex screws up the line splitting
        # sometimes, when printing the first fragment of a long line
        # containing a bunch of filenames at the end of the document; it only
        # does this in the .log file, and does so just before emitting an
        # "Output written to..." line.  So, off we go to crazytown, testing it.
        my $scan_idx = $in_idx + 1;
        my $scan_txt;
        for (; defined($scan_txt = $$in[$scan_idx]) &&
               (length($scan_txt) == $max_print_line); ++$scan_idx) {
        }       # scan_txt/scan_idx are first non-max_print_line line
        $scan_txt = $$in[++$scan_idx];  # check the line after that
        if (defined($scan_txt) &&
            ($scan_txt =~ /^Output written on .* \(\d+ pages?,/)) {
          $is_contd = 1;
        }
      }

      if (!$is_contd) { last CONT; }
      $in_idx++;
    }
    push @out, $unsplit;

    # pdftex quirk: sometimes warnings are output with a "! " that looks like
    # the start of a multiline error message, but which isn't.
    # also, we won't use this to pass multiline \PackageError{} stuff
    # through without unsplitting, since they actually seem to obey
    # the usual unsplitting rules.
    if (($unsplit =~ /^! /) && !($unsplit =~ /^! pdf(?:la)?tex warn/i)
        && !($unsplit =~ /^! (?:Package|Class) .* Error:/)) {
      # This line looks like the start of an error message.  latex sometimes
      # outputs multiple lines of detailed info starting at the line
      # following, sometimes with a "l.<number>" toward the end, and ending
      # with a "?" line.
      # Unfortunately, the intervening lines seem to use a different
      # max_print_line-based truncation system.  SIGH.  We'll just pass
      # those lines through for now, at least until we have more info about
      # how they should be unsplit.
      ELINE: while (defined(my $trainwreck = $$in[$in_idx + 1])) {
        push @out, $trainwreck;
        ++$in_idx;
        if ($trainwreck =~ /^\? /) { last ELINE; }
      }
    }
  }

  # in texlive at least, various packages (e.g. caption3) report routine
  # info via \PackageInfo which results in output of the form:
  #   Package caption Info: Unknown document class (or package),
  #   (caption)             standard defaults will be used...
  #
  # There are also \PackageWarning and \PackageError, as well as
  # variants for classes instead of packages.  (See "lterror.dtx" section
  # in "source2e.pdf", or "lterror.dtx" itself.)
  #
  # Since the parens confuse the filename parser, and since these are
  # conceptually just long single messages, we'll treat them as continued
  # lines and un-split them.  We're doing this with an inefficient seperate
  # pass, since the normal tex long-line-splitting is also done.  The
  # (packagename) prefixing isn't done at that splitting, but apparently
  # only at \MessageBreak.
  LINE: for (my $scan_idx = 0; $scan_idx < @out; $scan_idx++) {
    my $line = $out[$scan_idx];
    my $targ_line_idx = $scan_idx;
    if ($line =~ m{^(?:(?:Package|Class)\ (.*)\ (?:Info|Warning)|
                       !\ (?:Package|Class)\ (.*)\ Error):}xi) {
      my $is_err = (defined($2)) ? 1 : 0;
      my $pkg_name = ($is_err) ? $2 : $1;
    PKG_LINE:
      while (defined(my $pkg_maybe = $out[$scan_idx + 1])) {
        if ($pkg_maybe =~ m{^\($pkg_name\)\s+(\S.*)?$}) {
          my $pkg_cont = $1;
          # append continuation text onto the first line of this warning/etc,
          # and then replace the source line with a blank (instead of
          # shifting the rest of @out or re-copying it)
          $out[$targ_line_idx] .= " " . $pkg_cont;
          $out[$scan_idx + 1] = "";
          ++$scan_idx;
        } else {
          last PKG_LINE;
        }
      }
    }
  }

  return \@out;
}


sub _update_filename_stack($$$) {
  my ($stack, $line, $in_files_set) = @_;
  my $ldebug = $ENV{TEXTOOLS_LDEBUG_FILESTACK};
  my $stack_changed = 0;
  if ($ldebug) { _dp "_update_filename_stack: $line"; }
  MATCH: while ($line =~ m{([()<>{}])           # open or close paren
                    ([^()<>{}]*)}gx) {  # everything until paren or EOL
    my ($paren, $fname) = ($1, $2);
    $fname =~ s/\s+$//;
    if ($fname =~ m{^(?:use\ |
                      .*,\ id=\d+,\ |
                      .*\ \d+(\.\d+)?pt,\ |
                      see\ the\ transcript\ file|
                      to\ be\ read\ again|
                      1in=72
                     )}xi) {
      # oi, this isn't a filename, just more spam (likely from pdftex)
      if (pos($line) < length($line)) {
        pos($line) = pos($line) + 1;    # consume closing paren
      }
      next MATCH;
    }
    $fname = trim_fname($fname);        # lop off "./" if present
    $fname = pdftex_fname_expletive($fname);  # fixup further pdftex brokenness
    if ($paren =~ /^[(<{]$/) {  # file open
      if ($fname eq "") {
        die "opening empty filename? parser is confused, input \"$line\"";
      }
      if ($ldebug) { _dp "filename push: '$fname'"; }
      push @$stack, $fname;
      $stack_changed = 1;
      $in_files_set->insert($fname);
    } else {                    # file close
      my $popped = pop @$stack;
      $stack_changed = 1;
      if (!defined($popped)) {
        die "popped empty filename stack, input \"$line\"";
      }
      if ($ldebug) { _dp "filename pop: '$popped'"; }
    }
  }
  if ($ldebug) { _dp "filename stack: " . join(" ", @$stack); }
  return 1;
}


sub parse_latex_out($$) {
  my ($in, $opts) = @_;
  if (!defined($opts)) { $opts = {}; }
  my @file_stack;               # current "include" stack, most recent at end
  my $already_warned = new jSet(); # things we've already warned about
  my $ldebug = $ENV{TEXTOOLS_LDEBUG_LATEX};
  my %missing_type_map = ( "Reference" => "undef_refs",
                           "Citation" => "undef_cites",
                           "Label" => "undef_labels" );
  my $non_pdf_special = "Non-PDF special ignored!";

  my $result = {
                in_files => new jSet(),         # names
                missing_files => new jSet(),    # names
                undef_refs => new jSet(),               # names
                undef_cites => new jSet(),      # names
                undef_labels => new jSet(),     # names
                n_errors => 0,          # count
                n_warnings => 0,        # count
                any_undef_refs => 0,    # flag
                need_rerun => 0,        # flag: "may have changed, re-run"
                report => [],           # list of [REPORT_type,text] pairs
                n_pages => undef        # count
               };

  {
    my $unsplit = unsplit_latex_out($in, undef);
    $in = $unsplit;
  }

  LINE: for (my $in_idx = 0; $in_idx < @$in; $in_idx++) {
    # note: loop body may read additional lines
    my $line = $$in[$in_idx];
    my ($out_type, $out_txt);

    if ($ldebug) { print "in-> $line\n"; }

    # The "non-pdf special" errors get mixed in with page numbers, confusing
    # the page-number-remover and the filename parser.  We'll take advantage
    # of their (seemingly?) always occuring at the start of lines to infer
    # that they're triggered by the source file on the top of the stack.
    if ($line =~ s/^$non_pdf_special//gio) {
      my $file = (@file_stack) ? $file_stack[$#file_stack] : "(top-level)";
      if (!$already_warned->test("non_pdf_special $file")) {
        my $msg = "$file: warning: $non_pdf_special";
        push @{$$result{report}}, [REPORT_WARN, $msg];
        $$result{n_warnings}++;
        $already_warned->insert("non_pdf_special $file");
      }
    }

    # filter out freely-interspersed page numbers, grr (probably too loose)
    $line =~ s/^\]//;
    $line =~ s/(^| )\[\d+\]?//g;

    if ($in_idx == 0) {
      if (!($line =~ /^This is (pdf)?e?TeXk?, Version/i)) {
        die "error parsing latex output, unrecognized first line '$line'";
      }
      next LINE;
    }

    # Filter out some messages early: things we don't care about, or things
    # which will confuse later rules
    my $skip_namestack = 0;;
    if ($line =~ m{^(?:\((?:Font|hyperref)\)|
                     (?:Transcript|Output)\ written\ on|
                     See\ the\ LaTeX|
                     Package:)}xi) {
      if ($line =~ /^Output written on (.*) \((\d+) pages?, \d+ bytes?\)/) {
        $$result{n_pages} = $2;
      }
      # ignore otherwise
      next LINE;
    } elsif ($line =~ m{^\s*\((?:
                        \\end\ occurred\ inside\ a\ group
                       )}xi) {
      # something interesting which looks like filename info, but isn't
      $skip_namestack = 1;
    } elsif (($line =~ m{^(?:!\ )?(?:Package|Class)
                         \ .*\ (?:Info|Warning|Error):}xi)) {
      # these are unsplit in unsplit_latex_out(), and are full of spammy
      # parens
      $skip_namestack = 1;
    }

    if (!$skip_namestack && ($line =~ /^\s*[()<>{}]/)) {
      # starts with open/close paren or angle-bracket, and isn't recognized
      # parenthetical spam: assume it's file open/closing.
      _update_filename_stack(\@file_stack, $line, $$result{in_files});
    }

    my $file = $file_stack[$#file_stack];
    if (!defined($file)) {
      $file = "(top-level)";
    }

    # pdflatex apparently uses the "! " it's-an-error syntax, to emit some
    # single-line warnings (thanks for that).  it's still unclear if it ever
    # uses this to emit single-line errors.
    if ($line =~ /^! (pdfTeX) warning (.*)/) {
      my $warn = "$1: $2";
      $out_type = REPORT_WARN;
      $out_txt = "$file: warning: $warn";

    } elsif ($line =~ /^(?:!|Error:) (.*)/) {
      # latex error, with possible details on following lines
      # (see note at top of this file re: error format)
      my $err = $1;
      my ($lnum, $detail, @ml_body);
      EBODY: while (defined(my $next = $$in[$in_idx + 1])) {
        $in_idx++;
        if ($next =~ /^\? /) {
          last EBODY;
        } elsif ($next =~ /^l\.(\d+)(?: (.+))?/) {
          ($lnum, $detail) = ($1, $2);
          if (defined($detail) && ($detail =~ /^h for help\?/)) {
            $detail = undef;            # useless
          }
        } else {
          push @ml_body, $next;         # multi-line error message body
        }
      }
      $err =~ s/^LaTeX Error: //;       # thank you captain obvious
      if ($err =~ /File `(.+?)' not found./i) {
        my $fname = $1;
        $$result{missing_files}->insert($fname);
      }
      $out_type = REPORT_ERR;
      $out_txt = "$file:" . (defined($lnum) ? "$lnum:" : "") .
        " error: $err" . (defined($detail) ? " $detail" : "");

      if ($err eq "Emergency stop.") {
        # redundant error message
        $out_type = undef;
      }

    } elsif ($line =~ /^LaTeX Warning: (.*)/i) {
      my $warn = $1;
      my $lnum;
      if ($warn =~ / on input line (\d+)/i) {
        $lnum = $1;
        $warn =~ s/ on input line \d+//;
      }

if ($warn =~ /input lines/) { die "got a warning with a range of lines, at last; check code" }

      if ($warn =~ /:$/) {
        # collect extra lines, guess at continuation format here too
        WBODY: while (defined(my $next = $$in[$in_idx + 1])) {
          chomp $next;
          if ($next =~ /^\s+(\S.*)$/) {
            $warn .= " " . $1;
            $in_idx++;
          } else {
            last WBODY;
          }
        }
      }

      if ($warn =~ /^(Reference|Citation|Label)\s`(\S*)'\son\ page\ \S+\s+
                  (undefined|multiply\ defined).*/xi) {
        # (note: sometimes page numbers get mangled to "\thepage ")
        my ($type, $label, $problem) = ($1, $2, $3);
        if ($problem eq "undefined") {
          my $seen_tag = "$type $label";
          if (!$already_warned->test($seen_tag)) {
            $already_warned->insert($seen_tag);
          } else {
            $warn = undef;      # suppress
          }
          if (defined(my $result_key = $missing_type_map{$type})) {
            $$result{$result_key}->insert($label);
          }
          $$result{any_undef_refs} = 1;
        }
      } elsif ($warn =~ /There were undefined references/i) {
        if (!$$result{any_undef_refs}) {
          die "uh-oh, missed an undefined reference";
        }
        $warn = undef;  # suppress; it is obvious from other output
      } elsif ($warn =~ /\bRerun to get (.*) right\b/i) {
        $$result{need_rerun} = 1;
      }

      if (defined($warn)) {
        $out_type = REPORT_WARN;
        $out_txt = "$file:" . (defined($lnum) ? "$lnum:" : "") .
          " warning: $warn";
      }

    } elsif ($line =~ /^(?:No file (.*)\.)|(?:Can't find (.*) in (.*):)$/i) {
      my $fname = (defined($1)) ? $1 : "$3$2";
      $fname = trim_fname($fname);
      $out_type = REPORT_WARN;
      $out_txt = "$file: warning: missing file \"$fname\"";
      $$result{missing_files}->insert($fname);

    } elsif ($line =~ /^psfig: including (.*?)\s?$/) {
      my $fname = $1;
      $fname = trim_fname($fname);
      $$result{in_files}->insert($fname);

    } elsif ($line =~ /^\((\\end occurred inside a group.*)\)\s*$/i) {
      my $msg = $1;
      $out_type = REPORT_ERR;
      $out_txt = "$file: error: $msg";

    } elsif ($line =~ /^Runaway argument/i) {
      my $next = $$in[$in_idx + 1];
      if (defined($next) && ($next =~ /^\s*[({<]/)) {
        # gobble up next line, to keep from confusing filename parser.  sigh.
        $in_idx++;
      }

    } elsif ($line =~ m{^(?:Package|Class)\ .*\ Warning:
                        \ (.*?)(?:\ on\ input\ line\ (\d+)\.)?$}xi) {
      my ($warn, $lnum) = ($1, $2);
      $out_type = REPORT_WARN;
      $out_txt = "$file:" . (defined($lnum) ? "$lnum:" : "") .
        " warning: $warn";

    } elsif ($$opts{warn_boxes} &&
             ($line =~ m{^(Under|Over)full\ \\(h|v)box\ (.*)$}xi)) {
      my ($sense, $h_or_v, $detail) = ($1, $2, $3);
      my $lnum;
      if ($detail =~ /at lines (\d+)--(\d+)/) {
        $lnum = $1;
      } elsif ($detail =~ /at line (\d+)/) {
        $lnum = $1;
      }
      $out_type = REPORT_WARN;
      $out_txt = "$file:" . (defined($lnum) ? "$lnum:" : "") .
        " warning: $line";
    }

    if (defined($out_type)) {
      if ($out_type == REPORT_WARN) {
        $$result{n_warnings}++;
      } elsif ($out_type == REPORT_ERR) {
        $$result{n_errors}++;
      }

      defined($out_txt) or die "missing out_txt";
      push @{$$result{report}}, [$out_type, $out_txt];
      if ($ldebug) { print "out> $out_txt\n"; }
    }
  }

  if (@file_stack && ($$result{n_errors} == 0)) {
    my $msg = "warning: unmatched entries on file stack: " .
      join(" ", @file_stack);
    push @{$$result{report}}, [ REPORT_WARN, $msg ];
    die $msg;   # tough love for now, to help debug parsing
  }

  if ($$opts{dump_parsed}) {
    my $dd = Data::Dumper->new([$result], []);
    $dd->Indent(1);
    $dd->Terse(1);
    $dd->Useqq(1);
    $dd->Sortkeys(1);
    print $dd->Dump();
  }
  return $result;
}


sub filter_latex_out($$) {
  my ($in, $opts) = @_;
  my $parsed = parse_latex_out($in, $opts) or
    die "parsing failed";
  my @out_lines = map { $$_[1] } @{$$parsed{report}};
  return \@out_lines;
}


sub parse_bibtex_out($$) {
  my ($in, $opts) = @_;
  if (!defined($opts)) { $opts = {}; }
  my $already_warned = new jSet(); # things we've already warned about
  my $ldebug = $ENV{TEXTOOLS_LDEBUG_BIBTEX};
  my $err_tot = 0;
  my $warn_tot = 0;

  my $result = {
                in_files => new jSet(),         # names
                undef_cites => new jSet(),      # names
                n_errors => 0,          # count
                n_warnings => 0,        # count
                any_undef_refs => 0,    # flag
                report => []            # list of [REPORT_type,text] pairs
               };

  LINE: for (my $in_idx = 0; $in_idx < @$in; $in_idx++) {
    # note: loop body may read additional lines
    my $line = $$in[$in_idx];
    my ($out_type, $out_txt);

    if ($ldebug) { print "in-> $line\n"; }

    if ($in_idx == 0) {
      if (!($line =~ /^This is BibTeX, Version/i)) {
        die "error parsing bibtex output, unrecognized first line '$line'";
      }
      next LINE;
    }

    if ($line =~ m{^(I'm\ skipping whatever|
                     \(Error\ may\ have\ been)}xi) {
      # ignore
      next LINE;
    }

    if ($line =~ m{^(?:The\ (?:.+)\ file
                    |Database\ file\ \#\d+):\ (.*)$}xi) {
      my $fname = $1;
      $fname = trim_fname($fname);
      $$result{in_files}->insert($fname);

    } elsif ($line =~ /^\(There (?:was|were) (\d+) (error|warning)/i) {
      my ($count, $type) = ($1, $2);
      if ($type eq "error") {
        $err_tot = $count;
      } else {
        $warn_tot = $count;
      }

    } elsif ($line =~ /^Warning--(.*)/i) {
      my $warn = $1;
      my ($file, $line);
      if (defined(my $next = $$in[$in_idx + 1])) {
        if ($next =~ /^--line (\d+) of file (.*)/i) {
          ($file, $line) = ($2, $1);
          $in_idx++;
        }
      }
      if ($warn =~ /^I didn't find a database entry for "([^"]+)"/i) {
        my $label = $1;
        my $seen_tag = "undef $label";
        if (!$already_warned->test($seen_tag)) {
          $already_warned->insert($seen_tag);
        } else {
          $warn = undef;        # suppress
        }
        $$result{undef_cites}->insert($label);
        $$result{any_undef_refs} = 1;
      }
      if (defined($warn)) {
        $out_type = REPORT_WARN;
        $out_txt = (defined($file)) ? "$file:$line: " : "";
        $out_txt .= "warning: $warn";
      }

    } elsif ($line =~ /^(.*)---(?:line (\d+) of|while reading) file (.*)/i) {
      # error
      my ($err, $file, $line) = ($1, $3, $2);
      $out_type = REPORT_ERR;
      if ((($err eq "while executing") || ($err eq "")) &&
          defined(my $prev = $$in[$in_idx - 1])) {
        $err = $prev;           # meaningful error message is on previous line
        
      }
      $out_txt = "$file:" . (defined($line) ? "$line:" : "") . " ";
      $out_txt .= "error: $err";

    } elsif ($line =~ /^(.*)---(.*)/i) {                                # err
      my ($msg1, $msg2) = ($1, $2);
      $out_type = REPORT_ERR;
      $out_txt = "error: $msg1 $msg2";
    }

    my @cont_lines;
    BODY: while (defined(my $next = $$in[$in_idx + 1])) {
      # walk over body of multi-line message
      if ($next =~ /^ : (.*)/) {
        my $cont = $1;
        if ($ldebug) { print "in2> $cont\n"; }
        push @cont_lines, $cont;
        ++$in_idx;
      } else {
        last BODY;
      }
    }

    if (defined($out_type)) {
      if ($out_type == REPORT_WARN) {
        $$result{n_warnings}++;
      } elsif ($out_type == REPORT_ERR) {
        $$result{n_errors}++;
      }
      defined($out_txt) or die "missing out_txt";
      push @{$$result{report}}, [$out_type, $out_txt];
      if ($ldebug) { print "out> $out_txt\n"; }
    }
  }

  if ($err_tot && !$$result{n_errors}) {
    warn "unexpected: err_tot $err_tot, n_errors $$result{n_errors}";
    $$result{n_errors} = 1;     # be sure to indicate some error
  }
  if ($warn_tot && !$$result{n_warnings}) {
    warn "unexpected: warn_tot $warn_tot, n_warnings $$result{n_warnings}";
    $$result{n_warnings} = 1;   # be sure to indicate some warnings
  }

  if ($$opts{dump_parsed}) {
    my $dd = Data::Dumper->new([$result], []);
    $dd->Indent(1);
    $dd->Terse(1);
    $dd->Useqq(1);
    $dd->Sortkeys(1);
    print $dd->Dump();
  }
  return $result;
}


sub filter_bibtex_out($$) {
  my ($in, $opts) = @_;
  my $parsed = parse_bibtex_out($in, $opts) or
    die "parsing failed";
  my @out_lines = map { $$_[1] } @{$$parsed{report}};
  return \@out_lines;
}


sub parse_makeindex_out($$) {
  my ($in, $opts) = @_;
  if (!defined($opts)) { $opts = {}; }
  my $already_warned = new jSet(); # things we've already warned about
  my $ldebug = $ENV{TEXTOOLS_LDEBUG_MAKEINDEX};
  my $err_tot = 0;
  my $warn_tot = 0;

  my $result = {
                in_files => new jSet(),         # names
                n_errors => 0,          # count
                n_warnings => 0,        # count
                report => []            # list of [REPORT_type,text] pairs
               };

  LINE: for (my $in_idx = 0; $in_idx < @$in; $in_idx++) {
    # note: loop body may read additional lines
    my $line = $$in[$in_idx];
    my ($out_type, $out_txt);

    if ($ldebug) { print "in-> $line\n"; }

    if ($in_idx == 0) {
      if (!($line =~ /^This is makeinde?x, /i)) {
        die "error parsing makeindex output, unrecognized first line '$line'";
      }
      next LINE;
    }

#    if ($line =~ m{^()}xi) {
#      # ignore
#      next LINE;
#    }

    if ($line =~ m{^Scanning\ (?:.+)\ file\ (.*?)\.{3}}xi) {
      my $fname = $1;
      $fname = trim_fname($fname);
      $$result{in_files}->insert($fname);

    } elsif ($line =~ /^Generating\ output\ file.*done\ 
             \(\d+\ lines?\ written,\ (\d+)\ warnings?\)\.$/x) {
      $warn_tot = $1;

    # error formats
    } elsif ($line =~ /^(?:\*\*|!!)\ (.+)\ 
             \(file\ =\ (.*),\ line\ =\ (\d+)\):\s*$/xi) {
      my ($err, $file, $line) = ($1, $2, $3);
      if (defined(my $next = $$in[$in_idx + 1])) {
        if ($next =~ /^\s*-- (.+)\.$/i) {
          $err = $1;
          $in_idx++;
        }
      }
      if (defined($err)) {
        $out_type = REPORT_ERR;
        $out_txt = (defined($file)) ? "$file:$line: " : "";
        $out_txt .= "error: $err";
      }
    }

    if (defined($out_type)) {
      if ($out_type == REPORT_WARN) {
        $$result{n_warnings}++;
      } elsif ($out_type == REPORT_ERR) {
        $$result{n_errors}++;
      }
      defined($out_txt) or die "missing out_txt";
      push @{$$result{report}}, [$out_type, $out_txt];
      if ($ldebug) { print "out> $out_txt\n"; }
    }
  }

#  if ($err_tot && !$$result{n_errors}) {
#    warn "unexpected: err_tot $err_tot, n_errors $$result{n_errors}";
#    $$result{n_errors} = 1;     # be sure to indicate some error
#  }
  if ($warn_tot && !$$result{n_warnings}) {
    # note: I haven't managed to generate any warnings yet, so I don't know
    # what they look like.
    warn "unexpected: warn_tot $warn_tot, n_warnings $$result{n_warnings}";
    $$result{n_warnings} = 1;   # be sure to indicate some warnings
  }

  if ($$opts{dump_parsed}) {
    my $dd = Data::Dumper->new([$result], []);
    $dd->Indent(1);
    $dd->Terse(1);
    $dd->Useqq(1);
    $dd->Sortkeys(1);
    print $dd->Dump();
  }
  return $result;
}


sub filter_makeindex_out($$) {
  my ($in, $opts) = @_;
  my $parsed = parse_makeindex_out($in, $opts) or
    die "parsing failed";
  my @out_lines = map { $$_[1] } @{$$parsed{report}};
  return \@out_lines;
}

return 1;
