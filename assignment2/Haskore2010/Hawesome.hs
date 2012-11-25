module Hawesome where
import Haskore hiding(Key)
import Data.Ratio
import Data.List

-- HALP
fd d n = n d v
vol  n = n   v
v      = [Volume 80]
cmap f l = chord (map f l)

shift ::Eq a => a -> [a] -> [a]
shift k (s : ss) 
	| k == s = [s] ++ ss
	| otherwise = shift k $!(ss ++ [s])

-- TAPAS!
type BassStyle = [(Int, Dur)]
type ChordProgression = [(Music, Dur)]
type Scale = [KeyName]
type Chord = Music
type Key = (KeyName, KeyType)

data KeyType = Major | Minor
				deriving(Eq)

-- CANTSTANDS
cM = chord [ Note (x, 4) hn v | x <- [C, E, G]];
fM = chord [ Note (x, 4) hn v | x <- [F, A, C]];
gM = chord [ Note (x, 4) hn v | x <- [G, B, D]];

basic, calypso, boogie :: BassStyle
basic = [(0, hn), (4, hn)]

calypso = [	(-1, qn), (0, en), (2, en), 
			(-1, qn), (0, en), (2, en)]

boogie = [ 	(0, en), (4, en), 
			(5, en), (4, en), 
			(0, en), (4, en), 
			(5, en), (4, en)]

-- SCALES MAFFAKKA
baseScale :: Scale
baseScale = [KeyC, KeyD, KeyE, KeyF, KeyG, KeyA, KeyB] 

ionain 		= 	[0, 2, 4, 5, 7, 9, 11]
lydian 		= 	[0, 2, 4, 6, 7, 9, 11]
mixolydian 	= 	[0, 2, 4, 5, 7, 9, 10]
aeolian 	= 	[0, 2, 3, 5, 7, 8, 10]
dorean 		= 	[0, 2, 3, 5, 7, 9, 10]
phrygian 	= 	[0, 1, 3, 5, 7, 8, 10]

majorScales = [ionain, mixolydian, [], lydian, mixolydian, [], []];


twinkleChords :: ChordProgression
twinkleChords = [	(cM, wn), 
					(fM, hn), (cM, hn), 
					(gM, hn), (cM, hn), 
					(gM, hn), (cM, hn)]

twinkleFirst :: ChordProgression
twinkleFirst = [(cM, wn)]

-- FANKTIONS!

hautoBass :: BassStyle -> Key -> ChordProgression -> Music
hautoBass b k c = undefined

findMajorScale :: KeyName -> Scale -> [Int]
findMajorScale k s = maybe [] ((!!) majorScales) (elemIndex k s)

-- applyScale :: KeyName -> [Int] -> KeyName
