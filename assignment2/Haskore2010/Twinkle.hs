module Twinkle where
import AutoComp
import Haskore 
import Data.Ratio

-- note updaters for mappings
--fd d n = n d v
--vol  n = n   v
--v      = [Volume 80]
--lmap f l = line (map f l)

---- repeat something n times
--times  1    m = m
--times n m = m :+: (times (n - 1) m)

twinkleChords :: ChordProgression
twinkleChords = [	((C, 3), wn), 
					((F, 3), hn), ((C, 3), hn), 
					((G, 3), hn), ((C, 3), hn), 
					((G, 3), hn), ((C, 3), hn), 
					
					((C, 3), hn), ((G, 3), hn),
					((C, 3), hn), ((G, 3), hn),
					((C, 3), hn), ((G, 3), hn),
					((C, 3), hn), ((G, 3), hn),

					((C, 3), wn), 
					((F, 3), hn), ((C, 3), hn), 
					((G, 3), hn), ((C, 3), hn), 
					((G, 3), hn), ((C, 3), hn)  ]

l1 = lmap (fd qn) [c 5, c 5, g 5, g 5, a 5, a 5] :+: g 5 hn v :+: lmap (fd qn) [f 5, f 5, e 5, e 5, d 5, d 5] :+: c 5 hn v
l2 = lmap (fd qn) [g 5, g 5, f 5, f 5, e 5, e 5] :+: d 5 hn v

twinkleMelody = l1 :+: times 2 l2 :+: l1

twinkleBasic   = twinkleMelody :=: autoComp basic (C, Major) twinkleChords
twinkleCalypso = twinkleMelody :=: autoComp calypso (C, Major) twinkleChords
twinkleBoogie  = twinkleMelody :=: autoComp boogie (C, Major) twinkleChords

-- Test
twinkle = Tempo (5%2) (Instr "piano" twinkleCalypso)