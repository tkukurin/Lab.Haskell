type Position = (Integer, Integer)
data Orientation = West | East | North | South deriving (Eq, Show)
data TurnDir = CW | CCW deriving (Eq, Show)

data Turtle = Turtle { position :: Position,
                       orientation :: Orientation  } deriving Show


newTurtle :: Turtle
newTurtle = Turtle (0, 0) North

turtleX :: Turtle -> Integer
turtleX = fst . position

turtleY :: Turtle -> Integer
turtleY = snd . position

newPosition :: Orientation -> Integer -> Position -> Position
newPosition North move position = ((fst position) + move, snd position)
newPosition South move position = ((fst position) - move, snd position)
newPosition East move position = (fst position, (snd position) + move)
newPosition West move position = (fst position, (snd position) - move)

move :: Integer -> Turtle -> Turtle
move amount turtle = Turtle (newPosition tOrientation amount tPosition) tOrientation
  where tOrientation = orientation turtle
        tPosition = position turtle

newOrientation :: Orientation -> TurnDir -> Orientation
newOrientation North CW = East
newOrientation North CCW = West
newOrientation South CW = West
newOrientation South CCW = East
newOrientation East CW = South
newOrientation East CCW = North
newOrientation West CW = North
newOrientation West CCW = South

turn :: TurnDir -> Turtle -> Turtle
turn dir turtle = Turtle tPosition (newOrientation tOrientation dir) 
  where tOrientation = orientation turtle
        tPosition = position turtle

runTurtle :: [Turtle -> Turtle] -> Turtle -> Turtle
runTurtle ops turtle = foldl (\t f -> f t) turtle ops

