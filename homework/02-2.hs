-- HW 02: 2/4 implement dictionary represent as (k,v) pairs


type Key = Int
type Value = String
type Entry = (Key, Value)
type Dictionary = [Entry]
type Frequency = [(Value, Int)]


-- exists function
exists :: Key -> Dictionary -> Bool
exists key dict = key `elem` (map (fst) dict)


-- get function
get :: Dictionary -> Key -> Value
get dict key = if not (null value) then concat value
		else error ("key " ++ (show key) ++ " not found")
		where value = map (snd) $ filter fstEqualsKey dict
		      fstEqualsKey tpl = key == fst tpl


-- insert function; replaces if existing
insert :: Entry -> Dictionary -> Dictionary
insert entry dict = entry:(filter notFstEqualsKey dict)
	where notFstEqualsKey tpl = fst entry /= fst tpl


-- delete
delete :: Key -> Dictionary -> Dictionary
delete key dict = filter notFstEqualsKey dict
	where notFstEqualsKey tpl = key /= (fst tpl)


-- freq
freq :: Dictionary -> Frequency
freq []     = error "Dictionary is empty" 
freq (d:dictTail) = (freq' d dictTail):freqOnRemainingItems
	where nonEqualToCurrent tpl = snd d /= snd tpl
	      remainingItems = filter nonEqualToCurrent dictTail
	      freqOnRemainingItems = if null remainingItems then [] 
					else freq remainingItems

freq' :: Entry -> Dictionary -> (Value, Int) 
freq' entry dict = (snd entry, 1 + length (filter sndEqualsEntry dict))
	where sndEqualsEntry tpl = snd entry == snd tpl
