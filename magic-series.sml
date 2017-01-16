
fun gcd(0,n) = n
  | gcd(m,n) = gcd(n mod m, m);

fun is_magic xs =
    xs = List.tabulate (5, (fn x => List.foldl (fn (a, b) => if x = a then 1+b else b) 0 xs));

List.find is_magic (map (fn x => [x div 625 mod 5, x div 125 mod 5, x div 25 mod 5, x div 5 mod 5, x mod 5]) (List.tabulate (3125, (fn x => x))));
