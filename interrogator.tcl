#!/usr/bin/wish8.5
#
# Potential enhancements:
#  - store the histogram of responses separately from the logs;
#    keeping the logs in plain text (maybe compressing them regularly)
#    allows easier analysis later on, but it's going to get slow as
#    the logs get large.

#### CONFIGURABLE VARIABLES

set mood_db "~/.mood_log"


#### UTILITIES
### general helpers: providing a slightly lispier feel to tcl.
proc map {lambda list} {	# Per http://wiki.tcl.tk/4884
    set result {}
    foreach item $list { lappend result [apply $lambda $item] }
    return $result
}

####

variable moods [dict create] intents [dict create] warned 0

proc vet_line ls {
    global warned
    foreach v $ls {
	if [string is space $v] {
	    if {!$warned} {
		puts "Warning: mood DB contains bad entries (starting with \"$ls\")"
		set warned 1
	    }
	    return 1
	}
    }
    return 0
}

## XXX should lock file
set fd [open $mood_db "r+"]
while {![eof $fd]} {
    set line [gets $fd]
    if [string is space $line] { continue }
    lassign [split $line "\t"] stamp mood intent
    if [vet_line [list $stamp $mood $intent]] { continue }
    dict incr moods $mood
    dict incr intents $intent
}

foreach i {moods intents} {
    set l {}
    dict for {k v} [expr $$i] {lappend l [list $k $v]}
    set "sorted_$i" [map {x {lindex $x 0}} [lsort -integer -index 1 -decreasing $l]]
}
variable mood "" intent ""

grid [ttk::label .error -text "" -foreground red]
grid [ttk::labelframe .mood -text "You feel:"] -sticky news
grid [ttk::labelframe .intent -text "You intend to:"] -sticky news
pack [ttk::combobox .mood.box -values $sorted_moods -textvar mood] -side top -fill x
pack [ttk::combobox .intent.box -values $sorted_intents -textvar intent] -side top -fill x
grid [ttk::button .goforth -text "Onwards" -command {
    if {[string is space $mood] || [string is space $intent]} {
	.error configure -text "Please fill in both values." -background black
	return
    }
    puts $fd [join [list [clock seconds] $mood $intent] "\t"]
    close $fd
    exit 0
}]
bind .mood.box <Return> { .goforth invoke }
bind .intent.box <Return> { .goforth invoke }
focus .mood.box