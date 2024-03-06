rm_dups :: [*] -> [*]
rm_dups [] = []
rm_dups (front : rest)
= front : (rm_dups (filter (~= front) rest))

union :: [*] -> [*] -> [*]
union aset bset
    = rm_dups (aset ++ bset)