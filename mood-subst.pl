use strict;

my %moods = (
    'pretty good' => 7,
    'good' => 8,
    'not so great' => 4,
    'okay' => 5,
    'decent' => 6,
    'anxious' => 3,
    'bad' => 1,
    'not great' => 3
);

for(<>) {
    chomp;
    @_ = split /\t/;
    print $_[0] . "\t" . $moods{$_[1]} . "\t" . $_[1] . "\n";
}
