
canVote :: (Integral a) => a -> String
canVote x
	| x < 18 = "Cannot vote."
	| otherwise = "Can vote!"

parseCanVote :: (Integral a) => a -> String 
parseCanVote x
	| canVote x == "Cannot vote." = "The function said no!"
	| otherwise = "Function might have said yes."

