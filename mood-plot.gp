
set datafile separator "\t"
set style data fsteps
set timefmt "%s"
set xdata time
set format x "%m/%d\n%H"
set ytics
plot '< perl mood-subst.pl ~/.mood_log.2011-07' using 1:2:yticlabels(3) with points, '' using 1:2 t '' with steps
