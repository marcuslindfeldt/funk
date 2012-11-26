module Hawesome where
import Haskore hiding(Key)
import Data.Ratio
import Data.List

-- HALP
fd d n = n d v
vol  n = n   v
v      = [Volume 80]
cmap f l = chord (map f l)
lmap f l = line (map f l)

-- repeat something n times
times  1    m = m
times n m = m :+: (times (n - 1) m)

shift ::Eq a => a -> [a] -> [a]
shift k (s : ss) 
	| k == s = [s] ++ ss
	| otherwise = shift k $!(ss ++ [s])

-- TAPAS!
type BassStyle = [(Int, Dur)]
type ChordProgression = [(Pitch, Dur)]
type Scale = [Int]
type Chord = Music
type Key = (PitchClass, Mode)

-- CANTSTANDS
--cM = chord [ Note (x, 4) hn v | x <- [C, E, G]];
--fM = chord [ Note (x, 4) hn v | x <- [F, A, C]];
--gM = chord [ Note (x, 4) hn v | x <- [G, B, D]];

cMajor = (C, Major)

basic, calypso, boogie :: BassStyle
basic = [(0, hn), (4, hn)]

calypso = [	(-1, qn), (0, en), (2, en), 
			(-1, qn), (0, en), (2, en)]

boogie = [ 	(0, en), (4, en), 
			(5, en), (4, en), 
			(0, en), (4, en), 
			(5, en), (4, en)]

-- SCALES MAFFAKKA
-- baseScale = [KeyC, ,KeyD, KeyE, KeyF, KeyG, KeyA, KeyB]

--ionian 		= 	[C, D, E, F, G, A, B]
--lydian 		= 	[C, D, E, Fs, G, A, B]
--mixolydian 	= 	[C, D, E, F, G, A, As]
--aeolian 	= 	[C, D, Ds, F, G, Gs, As]
--dorean 		= 	[C, D, Ds, F, G, A, As]
--phrygian 	= 	[C, Cs, Ds, F, G, Gs, As]

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
					((G, 3), hn), ((C, 3), hn)]

l1 = lmap (fd qn) [c 5, c 5, g 5, g 5, a 5, a 5] :+: g 5 hn v :+: lmap (fd qn) [f 5, f 5, e 5, e 5, d 5, d 5] :+: c 5 hn v
l2 = lmap (fd qn) [g 5, g 5, f 5, f 5, e 5, e 5] :+: d 5 hn v

mainLine = l1 :+: times 2 l2 :+: l1

twinkle = Instr "piano" (Tempo 3 (Phrase [Dyn SF] (mainLine :=: (hautoBass basic (C, Major) twinkleChords)))) 

-- FANKTIONS!

hautoBass :: BassStyle -> Key -> ChordProgression -> Music
hautoBass b k c = line $ map (halpBass b) c

--musicForChord :: BassStyle -> Key -> (Pitch, Dur) -> Music
--musicForChord b k c = line

halpBass :: BassStyle -> (Pitch, Dur) -> Music
halpBass b p = line $ pitchDur (scaleForBass b (scaleForKey (fst p) ionian)) b {-Always major-}

scaleForBass :: BassStyle -> [Pitch] -> [Pitch]
scaleForBass b s = map ((!!) s) (map fst b)

scaleForKey :: Pitch -> Scale -> [Pitch]
scaleForKey k s = map pitch $ map (+ (absPitch k)) (majorScales !! maybe (-1) id (elemIndex (mod (absPitch k) 12) s))

pitchDur :: [Pitch] -> BassStyle -> [Music]
pitchDur (p:ps) (b:bs)
	| null ps || null bs = [Note p (snd b) v]
	| otherwise =  (Note p (snd b) v) : pitchDur ps bs 


 {- Always major -}

-- 1: Get first scale according to the songs MainKey
-- 2: obtain the scale for the Chord Key according to the scale in step 1
-- 3: apply the new scale on the Chord Key


-- indexForKey :: KeyName -> Scale -> Int
-- indexForKey k s = maybe (-1) id (elemIndex k baseScale)



-- applyScale :: KeyName -> [Int] -> KeyName
