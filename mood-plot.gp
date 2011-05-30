
set datafile separator "\t"
set style data fsteps
set timefmt "%s"
set xdata time
set format x "%m/%d %H"
set ytics
plot '< perl mood-subst.pl ~/.mood_log.2011-05' using 1:2:yticlabels(3) title 'Moods 2011/05' with points, '' using 1:2 t '' with steps
