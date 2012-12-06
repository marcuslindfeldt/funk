
--Functional music by Erik Westenius F08 (atf08ewe) and Marcus Lindfeldt (ada08mli).
--Since plagiarism is frowned upon, we refer to the course page 
--(http://cs.lth.se/english/course/edan40_functional_programming/programming_assignments/functional_music/)
--for the underlaying theory of this musical adventure in functional programming.

--We thought you might be a little disappointed by the lack of theory, so we drew some music to cheer you up :)
--(... No we didn't, we copied it, you can find it on the interwebs)

--		    ( )                     |____|
--		 ___|/________|\____________|____|_______
--		|__/|/_)_|____|_______|\__(_)__(_)_______|
--		|_(_|_/__|__(_)_______|\_________________|
--		|___|____|__________(_)__________________|
--		|________|_________________________(_)___|
--		                                     |
--		                                     |  

module AutoComp where
import Haskore hiding(Key)
import Data.Ratio
import Data.List

-- Helpers
fd d n = n d v
vol  n = n   v
v      = [Volume 80]
cmap f l = chord (map f l)
lmap f l = line (map f l)
times  1    m = m
times n m = m :+: (times (n - 1) m)

shift ::Eq a => a -> [a] -> [a]
shift k (s : ss) 
	| k == s = [s] ++ ss
	| otherwise = shift k $!(ss ++ [s])

-- Types!
type BassStyle = [(Int, Dur)]
type ChordProgression = [(Pitch, Dur)]
type Scale = [Int]
type Chord = [Pitch]
type Key = (PitchClass, Mode)

-- Constants
triad 			= [0, 2, 4] :: [Int];

basic, calypso, boogie :: BassStyle
basic = [(0, hn), (4, hn)]

calypso = [	(-1, qn), (0, en), (2, en), 
			(-1, qn), (0, en), (2, en)]

boogie = [ 	(0, en), (4, en), 
			(5, en), (4, en), 
			(0, en), (4, en), 
			(5, en), (4, en)]

-- Scales

ionian         	=       [0, 2, 4, 5, 7, 9, 11]
lydian			=       [0, 2, 4, 6, 7, 9, 11]
mixolydian		=       [0, 2, 4, 5, 7, 9, 10]
aeolian        	=       [0, 2, 3, 5, 7, 8, 10]
dorean			=       [0, 2, 3, 5, 7, 9, 10]
phrygian       	=       [0, 1, 3, 5, 7, 8, 10]

majorScales = [ionian, mixolydian, [], lydian, mixolydian, [], []];

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

twinkleRow = [	((C, 3), wn), 
					((F, 3), hn), ((C, 3), hn), 
					((G, 3), hn), ((C, 3), hn), 
					((G, 3), hn), ((C, 3), hn) ]					

-- functions

-- Auto bass
autoBass :: BassStyle -> Key -> ChordProgression -> Music
autoBass b k c = line $ map (halpBass b) c

halpBass :: BassStyle -> (Pitch, Dur) -> Music
halpBass b p 
	| snd p == hn = line $ pitchDur (scaleForBass (half b) (scaleForKey (fst p) ionian)) (half b)
 	| otherwise = line $ pitchDur (scaleForBass b (scaleForKey (fst p) ionian)) b

scaleForBass :: BassStyle -> [Pitch] -> [Pitch]
scaleForBass b s = map ((!!) s) (map fst b)

pitchDur :: [Pitch] -> BassStyle -> [Music]
pitchDur (p:ps) (b:bs)
	| null ps || null bs = [Note p (snd b) v]
	| (null ps || null bs) && (fst b) == -1 = [Rest (snd b)]
	| (fst b) == -1 = (Rest (snd b)) : pitchDur ps bs
	| otherwise =  (Note p (snd b) v) : pitchDur ps bs

half :: BassStyle -> BassStyle
half b = fst $ splitAt (length b `div` 2) b

-- Auto chord
autoChord :: Key -> ChordProgression -> Music
autoChord k c = line $ map chord (map notify (zip (optimizeChords (reverse chords)) durs))
	where
	durs = snd (unzip c)
	chords = map (basicTriad k) c

optimizeChords :: [Chord] -> [Chord]
optimizeChords (c:cs)
	| null cs = [c]
	| otherwise = bestTriad c (head cs) : optimizeChords cs

basicTriad :: Key -> (Pitch, Dur) -> Chord
basicTriad k p = boostOctave (map ((!!) (scaleForKey (fst p) ionian)) triad)

bestTriad :: Chord -> Chord -> Chord
bestTriad cn co = p !! (maybe (-1) id (elemIndex (minimum distances) distances))
	where 
	p = inversions cn
	distances = map (distance co) p 

inversions :: Chord -> [Chord]
inversions c = (invertUp c) ++ (invertDown c)

invertUp :: Chord -> [Chord]
invertUp (c:cs)
	| maximum (snd (unzip ch)) == 7 = []
	| otherwise = [ch] ++ invertUp (ch)
	where 
		ch = cs ++ [((fst c), (snd c) +1)]

invertDown :: Chord -> [Chord]
invertDown c
	| minimum (snd (unzip ch)) == 4 = []
	| otherwise = [ch] ++ invertDown (ch)
	where 
		ch = [((fst (last c)), (snd (last c)) -1)] ++ (init c)

boostOctave :: Chord -> Chord
boostOctave (p : ps) 
	| null ps = [(fst p, snd p +1)]
	| otherwise = (fst p, snd p +1) : (boostOctave ps)

distance :: [Pitch] -> [Pitch] -> Int
distance (p:ps) (q:qs)
	| null ps || null qs = abs ((absPitch p) - (absPitch q))
	| otherwise = abs ((absPitch p) - (absPitch q)) + distance ps qs 

notify :: ([Pitch], Dur) -> [Music]
notify pd
	| length (fst pd) == 0 = []
	| otherwise = (Note (head (fst pd)) (snd pd) v) : (notify ((tail (fst pd)), (snd pd)))

-- Common
scaleForKey :: Pitch -> Scale -> [Pitch]
scaleForKey k s = map pitch $ map (+ (absPitch k)) (majorScales !! maybe (-1) id (elemIndex (mod (absPitch k) 12) s))

autoComp :: BassStyle -> Key -> ChordProgression -> Music
autoComp b k c = (autoBass b k c) :=: (autoChord k c)