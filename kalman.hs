-- experiments with stuff from http://vixra.org/pdf/1606.0328v1.pdf

mean :: (Foldable t, Fractional r) => t r -> (r, r)
mean zs = foldr f (0,0) zs
  where f z (x, n) = (x + k*(z-x), 1+n)
          where k = 1/(1+n)

variance zs = foldr f (0,0,0) zs
  where f z (var,x,n) = (ssr/(max 1 n), x + u, 1+n)
          where
            u = (1/(1+n)) * (z-x)
            ssr = (n-1)*var + u*n*(z-x)
