
Functional music by Erik Westenius F08 (atf08ewe) and Marcus Lindfeldt (ada08mli).

  _    _                                  _                            _      
 | |  | |                                (_)                          (_)     
 | |__| | __ _ _ __ _ __ ___   ___  _ __  _  ___   _ __ ___  _   _ ___ _  ___ 
 |  __  |/ _` | '__| '_ ` _ \ / _ \| '_ \| |/ __| | '_ ` _ \| | | / __| |/ __|
 | |  | | (_| | |  | | | | | | (_) | | | | | (__  | | | | | | |_| \__ \ | (__ 
 |_|  |_|\__,_|_|  |_| |_| |_|\___/|_| |_|_|\___| |_| |_| |_|\__,_|___/_|\___|
                                                                              

    ( )                     |____|
 ___|/________|\____________|____|_______
|__/|/_)_|____|_______|\__(_)__(_)_______|
|_(_|_/__|__(_)_______|\_________________|
|___|____|__________(_)__________________|
|________|_________________________(_)___|
                                     |
                                     |  

== Introduction
------------------------------
The goal of this assignment is to write a haskell program which generates a simple bassline and harmonic accompaniment useing the Haskell music framework Haskore. To do this a small exploration of music theory is required and presented below.

== Musical Scores
------------------------------
In our simple case, a song is represented by a Melody, a chord progression and a bassline playing all together. The plueprint of a song is called the songs score, which is what you interpret when you read so called `printed music´. The notes in a score shows the songs melody and the chord sequence (The letters over the melody line) shows the songs harmonic structure.

== Haskore Music
------------------------------
This assignment uses a Haskell music library called Haskore which allows for the music to be expressed in a declarative manner. For more information on a Hascore and an introductory tutorial, please visit http://haskell.cs.yale.edu/wp-content/uploads/2011/02/HaskoreMusicTutorial-Springer.pdf

In hascore Music is represented as a set of notes. A note is represented by a pitch, duration and various other note attributes such as volume or instrument. The notes corresponding to the white piano keys are C, D, E, F, G, A, and B. The duration of an individual note us given by its nominal valie which is measured in fractions of a bar, this meaning that, for example, a half note is equal to 1/2. The actual duration is then determined by the songs tempo. In our case we chose a tempo of 120 bpms which is equal to 120 quater notes per minute.

Two notes can either be played either in succession to one another or in parallell.

To be able to use hascore in this assignment we first have to import Hascolre, and some other useful modules.

> module AutoComp where
> import Haskore hiding(Key)
> import Data.Ratio
> import Data.List

To reduce the ammount of wtfs/minute we decided to introduce some helper functions to make our lives easier.

These helperfunctions allows for easy creation of notes with appropriate volumes and the construction of note sequences.

> fd d n = n d v
> vol  n = n   v
> v      = [Volume 80]
> cmap f l = chord (map f l)
> lmap f l = line (map f l)
> times  1    m = m
> times n m = m :+: (times (n - 1) m)

== Harmonic theory
------------------------------
An imporant part of the score of a song is the songs Key. A key has two components, a Root and a harmonic quality (major or minor). The key defines the scale which gives the main note supply for the song. A scale is a subset of the twelve notes in an octave. The ionian scale is used to generate the scales of all major keys.

> ionian         	=       [0, 2, 4, 5, 7, 9, 11]
> lydian			=       [0, 2, 4, 6, 7, 9, 11]
> mixolydian		=       [0, 2, 4, 5, 7, 9, 10]
> aeolian        	=       [0, 2, 3, 5, 7, 8, 10]
> dorean			=       [0, 2, 3, 5, 7, 9, 10]
> phrygian       	=       [0, 1, 3, 5, 7, 8, 10]

> majorScales = [ionian, mixolydian, [], lydian, mixolydian, [], []];

The harmonic structure (the letters above the notes) of the accompaniment is called a chord progression. A chord is a couple of notes played simultaniously. Each chord symbol in a chord progression designates a chord class, which is a set of chords thatr are harmonically equivalent.

The types corresponding to the above described musical concepts are defined below.

> type ChordProgression = [(Pitch, Dur)]
> type Scale = [Int]
> type Chord = [Pitch]
> type Key = (PitchClass, Mode)

== Generate a bass line
------------------------------
In this assignment we should be able to generate three types of bass lines: Basic, Calypso and Boogie.

A bass style is described by a set of chords and durations.

> type BassStyle = [(Int, Dur)]

The notes in a bass style indicate a rythmic pattern of the bass lines.
The three bass patterns used in this assignment are described below.
The numbers below the notes indicate the number of the note in the scale to be played starting with 1 as the root, but since we like to start with a 0 in arrays, all numbers are subtracted with 1.

> basic, calypso, boogie :: BassStyle
> basic = [(0, hn), (4, hn)]
> 
> calypso = [	(-1, qn), (0, en), (2, en), 
> 			(-1, qn), (0, en), (2, en)]
> 
> boogie = [ 	(0, en), (4, en), 
> 			(5, en), (4, en), 
> 			(0, en), (4, en), 
> 			(5, en), (4, en)]

In order to create a bass accompaniment we need to know the key and the chord progression of the song, along with the desired bass style.

> autoBass :: BassStyle -> Key -> ChordProgression -> Music
> autoBass b k c = line $ map (helpBass b) c

> helpBass :: BassStyle -> (Pitch, Dur) -> Music
> helpBass b p 
> 	| snd p == hn = line $ pitchDur (scaleForBass (half b) (scaleForKey (fst p) ionian)) (half b)
>  	| otherwise = line $ pitchDur (scaleForBass b (scaleForKey (fst p) ionian)) b

Scale for bass is used to determine which scale to use when createing the bass line.

> scaleForBass :: BassStyle -> [Pitch] -> [Pitch]
> scaleForBass b s = map ((!!) s) (map fst b)

> pitchDur :: [Pitch] -> BassStyle -> [Music]
> pitchDur (p:ps) (b:bs)
> 	| null ps || null bs = [Note p (snd b) v]
> 	| (null ps || null bs) && (fst b) == -1 = [Rest (snd b)]
> 	| (fst b) == -1 = (Rest (snd b)) : pitchDur ps bs
> 	| otherwise =  (Note p (snd b) v) : pitchDur ps bs

Half is a help function used to split a bass style pattern in half in order to glue bass lines of two different keys in the same bar together.

> half :: BassStyle -> BassStyle
> half b = fst $ splitAt (length b `div` 2) b

== Chord Voicing
------------------------------
The chord voicing used in this assignment is built on triads. The basic pattern for a triad is discribed below. A triad is a chord with three notes sounding at once.

> triad 			= [0, 2, 4] :: [Int]

The cord voicing changes according to the songs chord progression. The chord voicing is created to back the song up harmonically.

> autoChord :: Key -> ChordProgression -> Music
> autoChord k c = line $ map chord (map notify (zip (optimizeChords (reverse chords)) durs))
> 	where
> 	durs = snd (unzip c)
> 	chords = map (basicTriad k) c

The tones in a triad do not have to be chosen from a specific occtave since pitch classes of different octaves are harmonically equal. However in this assignment we were advised to keep the chords in the range E,4 - G,5. A rule of thumb is to keep the changes in the chord melodies as limited as possible. We achieve this by using The chord optimization function below.

> optimizeChords :: [Chord] -> [Chord]
> optimizeChords (c:cs)
> 	| null cs = [c]
> 	| otherwise = bestTriad c (head cs) : optimizeChords cs

> basicTriad :: Key -> (Pitch, Dur) -> Chord
> basicTriad k p = boostOctave (map ((!!) (scaleForKey (fst p) ionian)) triad)

> bestTriad :: Chord -> Chord -> Chord
> bestTriad cn co = p !! (maybe (-1) id (elemIndex (minimum distances) distances))
> 	where 
> 	p = inversions cn
> 	distances = map (distance co) p 

Since the order of the notes in the chords need not to be fixed, there is something called inversions, which are permutatuions of the note combinations in the chord. This can be useful to minimize the distance between chords. A function to determine the inversions of a chord that are allowed (according to previously mentioned conditions) are described below.

> inversions :: Chord -> [Chord]
> inversions c = (invertUp c) ++ (invertDown c)

> invertUp :: Chord -> [Chord]
> invertUp (c:cs)
> 	| maximum (snd (unzip ch)) == 7 = []
> 	| otherwise = [ch] ++ invertUp (ch)
> 	where 
> 		ch = cs ++ [((fst c), (snd c) +1)]

> invertDown :: Chord -> [Chord]
> invertDown c
> 	| minimum (snd (unzip ch)) == 4 = []
> 	| otherwise = [ch] ++ invertDown (ch)
> 	where 
> 		ch = [((fst (last c)), (snd (last c)) -1)] ++ (init c)

> boostOctave :: Chord -> Chord
> boostOctave (p : ps) 
> 	| null ps = [(fst p, snd p +1)]
> 	| otherwise = (fst p, snd p +1) : (boostOctave ps)

> distance :: [Pitch] -> [Pitch] -> Int
> distance (p:ps) (q:qs)
> 	| null ps || null qs = abs ((absPitch p) - (absPitch q))
> 	| otherwise = abs ((absPitch p) - (absPitch q)) + distance ps qs 

> notify :: ([Pitch], Dur) -> [Music]
> notify pd
> 	| length (fst pd) == 0 = []
> 	| otherwise = (Note (head (fst pd)) (snd pd) v) : (notify ((tail (fst pd)), (snd pd)))

> scaleForKey :: Pitch -> Scale -> [Pitch]
> scaleForKey k s = map pitch $ map (+ (absPitch k)) (majorScales !! maybe (-1) id (elemIndex (mod (absPitch k) 12) s))

To piece everything together, we use the function below.

> autoComp :: BassStyle -> Key -> ChordProgression -> Music
> autoComp b k c = (autoBass b k c) :=: (autoChord k c)