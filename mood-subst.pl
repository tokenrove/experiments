use strict;

my %moods = (
    'great' => 9,
    'good' => 8,
    'pretty good' => 7,
    'decent' => 6,
    'okay' => 5,
    'not so great' => 4,
    'mediocre' => 4,
    'anxious' => 3,
    'not great' => 3,
    'bad' => 1
);

for(<>) {
    chomp;
    @_ = split /\t/;
    print $_[0] . "\t" . $moods{$_[1]} . "\t" . $_[1] . "\n";
}
