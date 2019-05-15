module Structs where


    import Graphics.Gloss.Data.Point

    -- | Velocity of the ghost's and pacman. Used for movement
    type Velocity = (Float, Float)

    -- | FPS of the game. Default is 60
    fps :: Int
    fps = 60

    -- | How long the ghosts will go into frighten mode
    frightenLength :: Float
    frightenLength = 4.0

    -- | Data describing the different names of the PacMan Characters
    data Name = Pacman | Pinky | Inky | Blinky | Clyde deriving Eq

    -- | Data describing the directions of Pacman
    data Direction = UP | DOWN | LEFT | RIGHT | STOP deriving (Eq, Show, Ord, Enum, Bounded)

    -- | Data describing win/loss
    data GameStatus = WON | LOST | PLAYING deriving (Eq, Show)

    -- | Data for ghost modes (Scatter, Chase, Frightened)
    data Mode = SCATTER | CHASE | FRIGHTENED deriving (Eq, Show)

    -- | Blinky's corner of the map
    blinkyScatterTarget :: Point
    blinkyScatterTarget = (8,10)

    -- | Inky's corner of the map
    inkyScatterTarget :: Point
    inkyScatterTarget = (8,-12)

    -- | Pinky's corner of the map
    pinkyScatterTarget :: Point
    pinkyScatterTarget = (-8,10)

    -- | Clyde's corner of the map
    clydeScatterTarget :: Point
    clydeScatterTarget = (-8,-12)


    -- | Data describing Pacman
    data Characters = Characters
        { cName :: Name             -- ^ Name of character from Name types
        , speed :: Velocity         -- ^ Constant speed of character
        , location :: Point         -- ^ Location of character in Point form
        } deriving Eq

    -- | Data describing the Ghosts (Blinky, Pinky, Inky, Clyde)
    data Ghost = Ghost
        { gName :: Name
        , gSpeed :: Velocity
        , gLocation :: Point
        , gTarget :: Point
        , gLastMove :: Direction
        , gDirection :: Direction
        }


    -- | Data describing the state of the game 
    data PacGame = Game
       { lives :: Int               -- ^ Number of lives the Pacman has
       , gameStatus :: GameStatus   -- ^ Status of the current game (WON/LOSS)
       , pacman :: Characters       -- ^ Pacman character data
       , pinky :: Ghost             -- ^ Pinky character data
       , inky :: Ghost              -- Inky character data
       , blinky :: Ghost            -- blinky character data
       , clyde :: Ghost             -- clyde character data
       , time :: Float              -- ^ Used to determine when to release the other 3 characters
       , score :: Int               -- ^ Keeps track of how many pellets have been eaten!
       , pellets :: [Point]         -- ^ List of pellet locations in the maze
       , direction :: Direction     -- ^ Current direction of Pacman
       , bufDirection :: Direction  -- ^ Buffered direction of movement
       , gMode :: Mode              -- ^ Mode of the ghosts, which can either be Scatter, Chase, Frightened 
       , lcrB :: Point              -- ^ Last cross road for Blinky
       , lcrP :: Point              -- ^ Last cross road for Pinky
       , lcrI :: Point              -- ^ Last cross road for Inky
       , lcrC :: Point              -- ^ Last cross road for Clyde
       , pPellets :: [Point]        -- ^ List of the 4 power pellet locations in the maze
       , fTime :: Float             -- ^ Time that ghost go into frightened mode for (5 seconds)
       }