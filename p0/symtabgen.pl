#!/usr/bin/env perl

# This script is responsible for taking a statically-linked binary and
# populating the 'functions' table inside it.
#
# This is done with a fairly crude parsing of the STABS (symbol tables) info
# inside the binary. The STABS format documentation is located at:
#   http://sourceware.org/gdb/download/onlinedocs/stabs.html
# Note that the STABS parser here is *very* incomplete, and there are almost
# certainly types of STAB that we should handle but do not.
# Note also that the STABS format used by gcc and gdb is not really documented.
# Those consulting the documentation linked above will notice that 85% of the
# sentences are of the form: "In VENDOR_COMPILER version FOO.BAR, the BAZ field
# sometimes has property QUXX for no well-explained reason."; we usually don't
# care about these unless VENDOR_COMPILER is gcc.
#
# Also, since I believe STABS is currently considered deprecated in favor of the
# more powerful (and *vastly* more complex) DWARF format, it may become
# necessary at some point in the future to try to parse DWARF info. Beware of
# doing this; libdwarf is very complicated (as of mid-2009) to interface with,
# and currently libbfd (the gnu debug info library) is far worse, being
# essentially designed to work only with gnu tools which have essentially
# unlimited amounts of person-hours devoted to making them work. As such, if you
# find yourself required to parse DWARF, you may wish to look at the source of
# dwarfdump and see if it is easy to extract only the sections you care about.
# 
# Furthermore, some of the information this tool needs (quite a lot of it,
# actually) can be extracted by much simpler tools like nm(1) or readelf(1); if
# STABS support does get dropped, you might be able to hack something together
# by looking only for DWARF function and type entries and mashing that together
# with the output of nm(1). No promises, though; I have not tried this myself.
#
# Note that gcc will freely reuse type numbers in different compilation units.
# Fortunately, we don't trip over this, because we resolve (x,y) type number
# pairs into the actual traceback_internal.h constants at PSYM time (i.e., when
# the "right" definitions are still in scope).
#
# NOTE NOTE NOTE
# See the warning in hlsym() about compound types. If something goes wrong with
# this script and it complains about a never-before-seen type in student code,
# it's a decent bet that it was declared somewhere inside a compound type and we
# missed it.

use warnings qw(all);
use strict;

use Fcntl qw(SEEK_CUR SEEK_SET O_WRONLY);

# Maximum length of a function name. Duplicated in traceback_internal.h
my $FUNCTS_MAX_NAME = 60;

# Maximum length of an argument name. Duplicated in traceback_internal.h
my $ARGS_MAX_NAME = 24;

# Maximum number of arguments. Duplicated in traceback_internal.h
my $ARGS_MAX_NUM = 6;

# Name of the `objdump` tool.
my $OBJDUMP = 'objdump';

# Name of the table into which we should put all this data.
my $FUNCS = 'functions';

# Types which we care to emit, and their corresponding numbers.
# Note that UNKNOWN is special; all types we can't handle are emitted as
# UNKNOWN.
my $outtypes = {
    'char' => 0,
    'int' => 1,
    'float' => 2,
    'double' => 3,
    'char*' => 4,
    'char**' => 5,
    'void*' => 6,
    'UNKNOWN' => -1,
};

# Mapping of types to the types they are declared to be. For example, if we have
# previously seen (0,3)=*(0,2)=*(0,1), then we have $types->{'(0,3')} =
# '*(0,2)'.
my $types = {};

# Type names - these are learned when we inspect LSYM entries which declare
# types.
my $tnames = {};

# Which function are we currently looking at? Required when parsing for argument
# type lines.
my $func = undef;

# Map of all the functions we've seen, from name to { addr, args }
my $funcs = {};

# Load address of the .rodata section. We need to know this because we are given
# the offset of the .rodata section inside the file, but we only know the load
# address of the functions table; thus, we have to take $ftab_vma - $rodata_vma
# + $rodata_off to determine where in the file to start writing.
my $rodata_vma = 0;
my $rodata_off = 0;
my $ftab_vma = 0;

# typetostr - Transform a type into a string. This takes a string looking like
# '(0,1)' and returns a string looking like 'char**'. It can handle all the
# types in tnames, which is populated by the hntsym() below. It can also handle
# a pointer to any type it can handle.
sub typetostr {
    my ($t) = @_;

    # If we get a type of the form (x,y) where we've never seen (x,y) before but
    # we HAVE seen (z,y) before for some z < x, it could be a declaration of a
    # type earlier that was still in scope when we got to file x.
    # Let's have a look through the table and see if we get lucky...
    # Note that the highest value of z must be chosen, because it supersedes
    # earlier declarations of y.
    #
    # Oops! We can also be a valid type even with z > x, if x includes z, z
    # defines the type, and then x uses it later.
    if (!exists($types->{$t}) && $t =~ /^\((\d+),(\d+)\)/) {
        my ($fn,$tn) = ($1,$2);
        my @ks = grep { $_ =~ /^\((\d+),(\d+)\)/
                     && $2 == $tn } keys %$types;
        @ks = sort { $b cmp $a } @ks;
        if (!@ks) {
            die "Unknown type: '$t'";
        }
        $t = $ks[0];
    }

    # Primitive types are defined as (x,y) = r(x,y)
    # Except void, which is defined as (x,y) = (x,y), sigh
    # And float types, which are declared as t(x,y) = r(a,b)...
    if ($types->{$t} =~ /^r/ or $types->{$t} eq $t) {
        return $tnames->{$t};
    }

    # Typedefs look like this, with the contents of the () not matching the
    # name. As long as there's no other character before the (), the types are
    # equivalent.
    elsif ($types->{$t} =~ /^\(.+\)/) {
        return $tnames->{$t};
    }

    # Pointer types look like this (with a '*' preceding the type numbers)
    elsif ($types->{$t} =~ /\*(.+)/) {
        my $n = typetostr($1);
        return $n . '*';
    }

    # Array types look like this.
    elsif ($types->{$t} =~ /^ar(.+)/) {
        my $n = typetostr($1);
        return $n . '*';
    }

    # Function types look like this. We punt on these because they aren't in the
    # type hash before. f(x,y) is a function returning type (x,y).
    elsif ($types->{$t} =~ /^f/) {
        return 'UNKNOWN';
    }

	# Structure types get ignored.
	elsif ($types->{$t} =~ /^s/) {
		return 'UNKNOWN';
	}

    # Oops?
    else {
        printf("Interesting. You stumped me with type '%s'.\n", $t);
        printf("Please hand in the current version of traceback.c as traceback.c.broken.\n");
    }
}

# typetoconst - Transform a type into its constant (as given in outtypes). If
# the type is not actually an outtype, default to UNKNOWN.
sub typetoconst {
    my ($t) = @_;
    my $s = typetostr($t);
    if (exists $outtypes->{$s}) {
        return $outtypes->{$s};
    }
    return $outtypes->{'UNKNOWN'};
}

# hatsym - Handles an anonymous type symbol. This function is recursive because
# sometimes we'll get lines like:
#   (3,1)=*(2,1)=*(1,1)=*(0,1)
# Which really define a bunch of new types.
# We are passed the left and right sides of a type declaration (i.e., after
# splitting on '=').
sub hatsym {
    my ($l, $r) = @_;

    # Oops, the right side contains more type definitions. Time to recurse.
    if ($r =~ /=/) {
        # Grab the left and right sides of the right side.
        my ($l0, $r0) = split(/=/, $r, 2);

        # The left side of the right side might be e.g. a range type or a
        # pointer type, in which case we need to throw away the sigil so that
        # the types hash won't have '*(x,y)' or something in it.
        $l0 = ($l0 =~ /^\(/ ? $l0 : substr($l0, 1));
        hatsym($l0, $r0);
    }

    # Extract the real thing the right side refers to (i.e., cut off the rest of
    # the type defs), and add it.
    my ($rs,undef) = split(/=/,$r,2);
    $types->{$l} = $rs;
}

# hntsym - Handles a named type symbol. We invoke hatsym() to do the "hard work"
# of figuring out which type is which, then add a mapping from the type we have
# to its name in tnames.
sub hntsym {
    my ($name, $l, $r) = @_;
    hatsym($l, $r);
    my ($rs, undef) = split(/=/,$r,2);
    $tnames->{$l} = $name;
}

# hlsym - Handles a stabs LSYM. Types are expressed this way.
sub hlsym {
    my ($value, $str) = @_;
    my ($name, $desc) = split(/:/, $str);
    my $type = substr($desc, 0, 1);

    # Type symbol? Call hntsym() to deal with the named type we're learning
    # about. Note that gcc *can* emit things like:
    #   :(0,3)=r(0,3)
    # Without giving a name, which is a top-level anonymous type definition; if
    # this happens, we need to call hatsym() instead.
    if ($type eq 't') {
        my ($def, undef) = split(/;/, substr($desc, 1));
        my ($l, $r) = split(/=/, $def, 2);
        if ($name) {
            hntsym($name, $l, $r);
        } else {
            hatsym($l, $r);
        }
    }

    # Sometimes these are the actual variable types of promoted arguments. For
    # example, if we declare:
    #    void foo(char bar)
    # gcc will emit the STABS for 'void foo(int bar) { char bar; }'.
    # We need to catch that downcast here to give the real type.
    elsif ($type eq '(' && $name && $func) {
        my @a = grep { $_->{name} eq $name } @{$funcs->{$func}->{args}};
        if (@a) { $a[0]->{type} = typetoconst($desc); }
    }
    else {
        # This occurs when we have, e.g., function local variables.
        # Ignore it for now. This also makes us ignore compound types, which are
        # declared with 'T'. This can cause problems, I think, if a primitive
        # type is first defined inside a compound type, but this does not seem
        # to happen -- yet.
        #
        # This is left unimplemented because I do not fully understand the
        # struct and union declaration formats.
    }
}

# hfun - Handles a stabs FUN.
# Sometimes things are declared inside return types (clever, eh?) so we need to
# handle those. I've never seen a *named* type created in a return type, and I
# do not believe it is possible to do so in C, but maybe...
#
# This function actually does very little work, because the second pass is
# responsible for spotting function entries. However, we do do quite a bit of
# sanity-checking here, and if we ever care about function return types, this
# would be the place to add it.
sub hfun {
    my ($value, $str) = @_;
    my ($name, $desc) = split(/:/, $str);
    my $type = substr($desc, 0, 1);
    my $rt = substr($desc, 1);
    $func = $name;

    # Oops, there's a new type defined in the return type! Deal with it.
    if ($rt =~ /=/) {
        my ($l, $r) = split(/=/, $rt, 2);
        hatsym($l, $r);
    }

    # Sanity-check - pass 2 should've seen this function.
    if (!exists($funcs->{$name})) {
        die "Previously undeclared function '$name'";
    }

    # Sanity-check - it had better have the same address as before.
    if ($funcs->{$name}->{addr} != hex($value)) {
        die sprintf("Address mismatch for '$name': %08x != %08x",
                $funcs->{$name}->{addr}, hex($value));
    }
}

# hpsym - Handles a stabs PSYM. PSYMs define parameters for functions, and give
# their types (in the string field) and their offsets (in the value field).
sub hpsym {
    my ($value, $str) = @_;
    my ($name, $desc) = split(/:/, $str);
    my $type = substr($desc, 0, 1);
    my $rt = substr($desc, 1);

    # We'd better be inside a function, since this is a parameter symbol.
    die "PSYM outside function" unless $func;

    # New type defined in the parameter type. Call hatsym() to make sure we
    # learn all the types defined therein. Again, I do not believe named types
    # can be instantiated inside function argument types.
    if ($rt =~ /=/) {
        my ($l, $r) = split(/=/, $rt, 2);
        $rt = $l;
        hatsym($l, $r);
    }

    # Stash this one in the functions table.
    push @{$funcs->{$func}->{args}}, { name => $name,
                                       offset => hex($value),
                                       type => typetoconst($rt) };
}

# This function ensures that we emit exactly one symbol for each address that
# has any symbols pointing at it in the program. We prefer symbols that do not
# start with '_'; under that restriction, we choose the first symbol,
# alphabetically.
sub uniq {
    my $fa = {}; # Address -> [Symbols at that address]
    my @r = (); # Result list of function entries

    # Look through the passed-in list of functions
    foreach my $k (@_) {
        my $i = $funcs->{$k};

        # Nothing else seen at this address so far
        if (!$fa->{$i->{addr}}) {
            $fa->{$i->{addr}} = [];
        }

        # Add ourselves to the list of things at this address
        push @{$fa->{$i->{addr}}}, $k;
    }

    # Sort the function table by address
    foreach my $k (sort keys %$fa) {
        # Sort the entries at each address by name
        my @ss = sort @{$fa->{$k}};

        # Grab the ones that don't start with _
        my @fs = grep { $_ !~ /^_/ } @ss;

        # If there are any non-underscored ones, emit the first one; otherwise,
        # emit the first underscored one (we alpha-sorted above).
        if (@fs) {
            push @r, $fs[0];
        } else {
            push @r, $ss[0];
        }
    }

    return @r;
}

# wfunc - Writes the entry for an individual function. Handed a file descriptor
# and a function object; the file descriptor is assumed to be placed where we
# need to write.
sub wfunc {
    my ($fd, $func) = @_;
    my $f = $funcs->{$func};
    my $addr = $f->{addr};

    die "Function $func with no address?" unless $addr;

    # Pack the entry into a C structure, and spit out the header.
    my $bs = pack(sprintf("I Z%d", $FUNCTS_MAX_NAME), $addr, $func);
    syswrite($fd, $bs);

    # Sort the argument list by offset; at least with gcc-4, these are sorted
    # already, but this is not guaranteed to be the case.
    my @args = sort { $a->{offset} <=> $b->{offset} } @{$f->{args}};

    # Spit out each argument.
    for (my $i = 0; $i < $ARGS_MAX_NUM; $i++) {
        my $a = { type => 0, offset => 0, name => '' };
        if ($i <= $#args) {
            $a = $args[$i];
        }

        # Pack the argument entry into a C struct and spit it out.
        my $as = pack(sprintf("I I Z%d", $ARGS_MAX_NAME), $a->{type}, $a->{offset}, $a->{name});
        syswrite($fd, $as);
    }
}

# File we're operating on
my $binary = $ARGV[0];

die "Target '$binary' is not a file." unless -f $binary;

# List section offsets and virtual addresses
my @sects = `$OBJDUMP -h $binary`;
if ($?) { die "`$OBJDUMP -h $binary` failed: $!"; }

# Spit out all the STABS entries
my @stabs = `$OBJDUMP -G $binary`;
if ($?) { die "`$OBJDUMP -G $binary` failed: $!"; }

# Spit out all the symbols
my @syms = `$OBJDUMP -t $binary`;
if ($?) { die "`$OBJDUMP -t $binary` failed: $!"; }

# No ignored lines here, since we only operate if the regex matches
# The regex matches lines like:
#   14 .rodata 000014b9 08048640 08048640 00000640 2**5
# Which have the format:
#   Idx Name Size VMA LMA Offset Alignment
for my $line (@sects) {
    if ($line =~ /^\s+\d+\s(\S+)\s+\S+\s+(\S+)\s+\S+\s+(\S+)/) {
        my ($sect, $vma, $off) = ($1, $2, $3);
        if ($sect eq '.rodata') {
            $rodata_vma = hex($vma);
            $rodata_off = hex($off);
        }
    }
}

# blank line, file format, blank line, "SYMBOL TABLE"
# Output ends with two blank lines
@syms = @syms[4 .. $#syms - 2];
for my $line (@syms) {
    $line =~ s/\n//;
    # addr, flags, type, section, size, name
    # The name may contain spaces.
    my ($addr, undef, $type, undef, undef, $name) = split(/\s+/, $line, 6);
    if (defined($name) && $name eq $FUNCS) {
        $ftab_vma = hex($addr);
    }
    elsif ($type eq 'F') {
        if ($name =~ / /) {
            # Sometimes the name is like: .hidden _foo
            # In this case, we really want _foo, so:
            $name =~ s/^\S+\s+//;
        }
        $funcs->{$name} = {};
        $funcs->{$name}->{addr} = hex($addr);
        $funcs->{$name}->{args} = [];
    }
}

if (!$ftab_vma) {
    printf("The provided file does not contain symbol '%s'\n", $FUNCS);
    printf("Please ensure there is at least one reference to $FUNCS in traceback.c\n");
    exit 1;
}

# blank line, file format, blank, "Contents of .stab section", blank, column
#   names, blank
# The output ends with an empty line.
@stabs = @stabs[7 .. $#stabs - 1];
for my $line (@stabs) {
    $line =~ s/\n//;
    my (undef, $type, undef, undef, $value, undef, $str) = split(/\s+/, $line, 7);
    if ($type eq 'LSYM') { hlsym($value, $str); }
    elsif ($type eq 'FUN') { hfun($value, $str); }
    elsif ($type eq 'PSYM') { hpsym($value, $str); }
}

# Open the binary for writing
my $outfd = undef;
sysopen($outfd, $binary, O_WRONLY) or die "Can't open '$binary': $!";

# Seek to the start of the functions table
sysseek($outfd, ($ftab_vma - $rodata_vma) + $rodata_off, SEEK_SET);

# Strip multiple symbols pointing at the same address. Prefer non-underscored
# versions. This also sorts the function table by address.
my @sfns = uniq(keys %$funcs);

# Write functions back to the binary.
foreach my $f (@sfns) { wfunc($outfd, $f); }
close($outfd) or die "Can't close '$binary': $!";
