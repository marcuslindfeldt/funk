module Pattern where
import Utilities
import Data.Maybe


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces any wildcard in pattern list with the substition list
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute w (p:ps) s
	| p == w = s ++ substitute w ps s
	| otherwise = p : substitute w ps s

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just [] 
match _ [] s = Nothing 
match _ p [] = Nothing
match wildcard (p:ps) (s:ss)  
	| p /= s && p /= wildcard = Nothing
	| p == s && p /= wildcard = match wildcard ps ss
	| p == wildcard = orElse (singleWildcardMatch (p:ps) (s:ss)) (longerWildcardMatch (p:ps) (s:ss))
 
-- Helper function to match

singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs) = mmap (\l -> [x]) (match wc ps xs) 

longerWildcardMatch (wc:ps) (x:xs) = mmap ((++) [x]) $ match wc (wc:ps) xs

-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions

-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------
-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply w f s p = mmap (substitute w (snd p) . f) $ match w (fst p) s

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply w f ps s = foldr1 (orElse) $ map (transformationApply w f s) ps
