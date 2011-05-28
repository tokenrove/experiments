#!/usr/local/bin/wish8.5

# dream functionality:
#  store documents by hash
#  at any time, manipulate "normal" filename along with metadata, tags
#  look up documents online, grab bibtex and stick it in a database
#  allow local tagging
#
# Note: uniq/[first byte of sha1 hash]/[rest] like git
#

# TODO
#   What's left to do?
#   - - read tags from tag widget
#   - - make tag directories if they don't already exist
#   - - check for duplicates in uniq database already, if already exists, complain (allow editing later)
#   - - do it -> mv, mkdir, ln, and move forward or quit
#   - - use paned window instead of pack for top-level
#   - - preview pane pdf: pdfdraw -> image, click to show next page
#   - - preview pane djvu: djvutxt -> pane, first 500 lines
#   - - preview pane chm: chm-dump -s file | grep -i .hhc$, first 500 lines
#   - - search button
#

#### UTILITIES
### general helpers: providing a slightly lispier feel to tcl.
proc map {lambda list} {	# Per http://wiki.tcl.tk/4884
    set result {}
    foreach item $list { lappend result [apply $lambda $item] }
    return $result
}
proc mapcan {lambda list} {
    set result {}
    foreach item $list { set result [concat $result [apply $lambda $item]] }
    return $result
}
proc I {x} {set x};     	# Prefer expressions to statements.
proc member {item list} {expr -1 != [lsearch $list $item]}
proc when {predicate body} {uplevel [list if "$predicate" $body]}
proc unless {predicate body} {uplevel [list if "$predicate" {} $body]}

### sbset: from http://wiki.tcl.tk/950
proc sbset {sb first last} {
    if {$first <= 0 && $last >= 1} {
	grid remove $sb
    } else {
	grid $sb
    }
    $sb set $first $last
}
### make-scrollable: add horizontal/vertical scrollbars to a frame
proc make-scrollable { kind frame interior } {
    grid $interior -row 0 -column 0 -sticky nsew
    if {[member "horizontal" $kind]} {
	ttk::scrollbar $frame.hsb -orient horizontal -command [list $interior xview]
	grid $frame.hsb -row 1 -column 0 -sticky nsew
	$interior conf -xscrollcommand [list sbset $frame.hsb]
	grid columnconfigure $frame 0 -weight 1
    }
    if {[member "vertical" $kind]} {
	ttk::scrollbar $frame.vsb -orient vertical -command [list $interior yview]
	grid $frame.vsb -row 0 -column 1 -sticky nsew
	$interior conf -yscrollcommand [list sbset $frame.vsb]
	grid rowconfigure $frame 0 -weight 1
    }
}


#### DOCUMENT
### the document data structure
# hash table keyed on file name, containing:
#   (title,author) -> current best guesses for title and author
#   tags -> list of tags
#   results -> list of (text,tag) pairs.
#   hash -> sha1 hash of file
set documents [dict create]

proc seen-file-before? {f} {global documents; dict exists $documents $f}
proc read-magic {f} {exec file -ib /home/julian/ref/book/$f}

proc try-pdfinfo {f} {
    global documents
    set fd [open "|pdfinfo -x \"/home/julian/ref/book/$f\" 2>/dev/null"]
    while {![eof $fd]} {
	switch -regexp -matchvar m -- [gets $fd] {
	    {/Title \((.+)\)} { dict set documents $f title [lindex $m 1] }
	    {/Author \((.+)\)} { dict set documents $f author [lindex $m 1] }
	}
    }
    close $fd
    return true
}

proc populate-w/defaults {f} {
    global documents
    # worst case, fall back on:
    dict set documents $f title [file rootname $f]
    dict set documents $f author ""
    dict set documents $f tags {compiler book}
    dict set documents $f results [list {"Fresh file, unable to analyze." normal} {"  " normal} {"(Click here to see raw file contents in this pane.)" raw-preview}]
}

proc initially-analyze-file {f} {
    global documents
    # if this is already in the uniq/ folder, ask if we can remove it from the list
    # dispatch on file type to make some initial guesses for values
    populate-w/defaults $f
    switch [read-magic $f] {
	application/pdf {
	    when [try-pdfinfo $f] {return}
	}
	image/vnd.djvu {
	    # XXX
	    puts "got a djvu $f"
	}
	application/x-chm {
	    # XXX
	    puts "got a chm $f"
	}
    }
}

proc repopulate-tags {tags} {
    .controls.tags delete 1.0 end
    foreach tag $tags { .controls.tags insert insert $tag tag " " ws }
}

proc repopulate-results {results} {
    .results.t configure -state normal
    .results.t delete 1.0 end
    foreach {text tag} [join $results] { .results.t insert insert $text $tag }
    .results.t configure -state disabled
}

proc map-in-file {f} {
    global documents
    upvar #0 [.controls.title cget -textvariable] title_var
    upvar #0 [.controls.author cget -textvariable] author_var
    dict with documents $f {
	set title_var $title
	set author_var $author
	repopulate-tags $tags
	repopulate-results $results
    }
}


#### GUI
### file pane
make-scrollable {horizontal vertical} [ttk::labelframe .files -text "Files"] [listbox .files.lb -listvariable files]

### results pane
make-scrollable vertical [ttk::labelframe .results -text "Results"] [text .results.t]
.results.t tag configure raw-preview -underline true
# XXX preview of contents
.results.t tag bind raw-preview <Button-1> {puts "time to do something!"}

### controls pane
ttk::labelframe .controls -relief groove -padding {5 5 5 5} -text "Controls"

## title/author entries
grid configure [ttk::entry .controls.title -textvariable title] -column 0 -row 0
set title "(unset)"
grid configure [ttk::label .controls.titleLabel -text "Title"] -column 1 -row 0
grid configure [ttk::entry .controls.author -textvariable author] -column 0 -row 1
grid configure [ttk::label .controls.authorLabel -text "Author"] -column 1 -row 1

## search and preview buttons
grid configure [ttk::button .controls.search -text "Search"] -column 0 -row 2
grid configure [ttk::button .controls.preview -text "Preview"] -column 1 -row 2

## tag box
grid configure [text .controls.tags -width 20 -height 5] -column 0 -columnspan 2 -row 3 -sticky news
.controls.tags tag configure tag -underline true -foreground blue

## action buttons
grid configure [ttk::button .controls.quit -text "Quit"] -column 0 -row 4
grid configure [ttk::button .controls.doIt -text "Do it! →"] -column 1 -row 4

### layout
foreach i {0 1} { grid columnconfigure .controls $i -weight 1 }
grid rowconfigure .controls 3 -weight 1
pack .files .results .controls -side left -fill y

### event handlers
.controls.quit configure -command { exit 0 }
.controls.doIt configure -command convert-file
bind .files.lb <<ListboxSelect>> choose-file

proc titleAuthorValidation {name value} {
    if {[regexp {/} $value]} {return false}
    global documents
    set key [switch $name .controls.title {I title} .controls.author {I author}]
    dict set documents [current-file] $key $value
    return true
}

foreach widget {.controls.title .controls.author} {
    $widget configure -validate key
    $widget configure -validatecommand {titleAuthorValidation %W %P}
}

bind .controls.tags <<Modified>> { puts "Modified tags!" }

#### ACTIONS

proc current-file {} {
    global current_file files
    lindex $files $current_file
}

# XXX ugly thanks to fixes; clean up
proc choose-file {} {
    global current_file files
    set f [.files.lb curselection]
    if {[llength $f] > 1} {set f [lindex $f 0]; .files.lb selection set $f} \
	elseif {[llength $f] < 1} {.files.lb selection set $current_file}
    set current_file $f
    set f [current-file]
    unless [seen-file-before? $f] {initially-analyze-file $f}
    map-in-file $f
}

proc uniq-name {f} {
    global documents fileBase
    if {![dict exists $documents $f hash]} {
	set s [exec sha1 -q $fileBase/$f]
	dict set documents $f hash "[string range $s 0 1]/[string range $s 2 end]"
    }
    return "uniq/[dict get $documents $f hash]"
}

proc presentation-name {f} {
    global documents
    dict with documents $f {
	regsub -all " " "$author: $title[file extension $f]" "_"
    }
}

proc by-author-name {f} {return "by-author/[presentation-name $f]"}

proc associated-tags {f} {
    global documents
    dict get $documents $f tags
}
proc tagged-name {f t} {return "$t/[presentation-name $f]"}

proc convert-file {} {
    toplevel .pop -width 20c -height 4c
    grab .pop
    set f [current-file]
    ttk::label .pop.l -text "Move $f to [uniq-name $f]\n and create links to: [by-author-name $f] [map {t {tagged-name [current-file] $t}} [associated-tags $f]]\n"
    bind .pop.l <Configure> [list %W configure -wraplength %w]
    ttk::button .pop.b -text "Go away" -command {destroy .pop}
    pack .pop.l .pop.b -in .pop -fill x
    focus .pop
}


#### MAIN
set fileBase [glob [if {$argc > 0} {lindex $argv 0} {I "~/ref/book"}]]
set files [mapcan {i {if {[file type $i] eq "file"} {list [file tail $i]}}} \
	       [glob $fileBase/*]]
if {[llength $files] < 1} {
    puts "I'm afraid that's not enough files for me to work with.  Drop all your
 unsorted crap into a random directory and point me at that."
    exit 1
}
.files.lb selection set 0
choose-file
