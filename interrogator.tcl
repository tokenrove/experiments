#!/usr/bin/env wish
#
# Potential enhancements:
#  - lock anything we're not writing in append mode

#### CONFIGURABLE VARIABLES

set mood_log "~/.mood_log.[clock format [clock seconds] -format "%Y-%m"]"
set histogram_db "~/.mood_db"
set suggestion_text "If you're unsure about intent, why not try a habit \
like solfege or sketching, or even a short, restricted-scope code exercise?  \
Check your daily list."

#### UTILITIES
### general helpers: providing a slightly lispier feel to tcl.
proc map {lambda list} {	# Per http://wiki.tcl.tk/4884
    set result {}
    foreach item $list { lappend result [apply $lambda $item] }
    return $result
}

proc any {lambda list} {
    foreach x $list { if [apply $lambda $x] {return 1} }
    return 0
}

#### AUTOCOMPLETE
### code from http://wiki.tcl.tk/13267, modified for combobox.
proc autocomplete {win action validation value} {
    if {$action == 1 & $value != {} & \
        [set pop [lsearch -inline [$win cget -values] $value*]] != {}} {
	$win delete 0 end;  $win insert end $pop
	$win selection range [string length $value] end
	$win icursor [string length $value]
    } else {
	$win selection clear
    }
    after idle [list $win configure -validate $validation]
    return 1
}

#### DATABASE or such as these simple flat files are...

array set choices [list mood [dict create] intent [dict create]]
variable mood {} intent {} warned 0

proc vet_line ls {
    if [any {v {string is space $v}} $ls] {
	if {!$::warned} {
	    puts "Warning: mood DB contains bad entries (starting with \"$ls\")"
	    set ::warned 1
	}
	return 1
    }
    return 0
}

## XXX should lock file
proc read_histogram {path} {
    if { [catch {open $path r} fd] } { return }
    while {![eof $fd]} {
	set line [gets $fd]
	if [string is space $line] { continue }
	lassign [split $line "\t"] type value count
	if [vet_line [list $type $value $count]] { continue }
	dict set ::choices($type) $value $count
    }
    close $fd
}

proc write_histogram {path} {
    set fd [open $path "w"]
    foreach d [array names ::choices] {
	variable $d
	dict incr ::choices($d) [set $d]
	dict for {k v} $::choices($d) { puts $fd [join [list $d $k $v] "\t"] }
    }
    close $fd
}

proc sort_choices {} {
    foreach d [array names ::choices] {
	set l {}
	dict for {k v} $::choices($d) {lappend l [list $k $v]}
	set ::sorted_choices($d) [map {x {lindex $x 0}} \
				      [lsort -integer -index 1 -decreasing $l]]
    }
}

#### MAIN

# For testing: don't write new output.
set dummy_run_p [regexp -- "-dummy" $::argv]

read_histogram $histogram_db
sort_choices

# Build UI
grid [ttk::label .error -text "" -foreground red]
grid [ttk::labelframe .mood -text "You feel:"] -sticky news
grid [ttk::labelframe .intent -text "You intend to:"] -sticky news
foreach v [array names choices] {
    pack [ttk::combobox .$v.box -values $sorted_choices($v) -textvar $v \
	      -validate all -validatecommand {autocomplete %W %d %v %P}] \
	-side top -fill x
    bind .$v.box <Return> { .goforth invoke }
}
grid [ttk::button .goforth -text "Onwards" -command {
    if [any {x {string is space $x}} [list $mood $intent]] {
	.error configure -text "Please fill in all values." -background black
	return
    }

    if {! $::dummy_run_p} {
	set fd [open $mood_log "a"]
	puts $fd [join [list [clock seconds] $mood $intent] "\t"]
	close $fd

	write_histogram $histogram_db
    }
    exit 0
}]
grid [ttk::label .suggestion -text $::suggestion_text]
bind .goforth <Return> { .goforth invoke }
focus .mood.box
wm attributes . -topmost 1 -fullscreen 1
wm title . "Mood Interrogator"
wm deiconify .
bind . <Map> { grab -global . }
