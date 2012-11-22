module Twinkle where
import Haskore

-- note updaters for mappings
fd d n = n d v
vol  n = n   v
v      = [Volume 80]
lmap f l = line (map f l)

-- repeat something n times
times  1    m = m
times n m = m :+: (times (n - 1) m)

l1 = lmap (fd qn) [c 5, c 5, g 5, g 5, a 5, a 5] :+: g 5 hn v :+: lmap (fd qn) [f 5, f 5, e 5, e 5, d 5, d 5] :+: c 5 hn v
l2 = lmap (fd qn) [g 5, g 5, f 5, f 5, e 5, e 5] :+: d 5 hn v

mainLine = l1 :+: times 2 l2 :+: l1

-- Putting it all together:
twinkle = Instr "piano" (Tempo 3 (Phrase [Dyn SF] mainLine )) 