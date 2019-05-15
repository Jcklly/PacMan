module Maze where

    import Graphics.Gloss

    -- | Defines the list of points that make up the maze of Pacman
    coords :: [Point]
    coords = [(0,1),(-9,-11),(-9,-10),(-9,-9),(-9,-8),(-9,-7),(-9,-6),(-9,-5),(-9,-4),(-9,-3),(-9,-1),(-9,1),(-9,3),(-9,4),(-9,5),(-9,6),(-9,7),(-9,8),(-9,9),(9,-10),(9,-9),(9,-8),(9,-7),(9,-6),(9,-5),(9,-4),(9,-3),(9,-1),(9,1),(9,3),(9,4),(9,5),(9,6),(9,7),(9,8),(9,9),(-9,-11),(-8,-11),(-7,-11),(-6,-11),(-5,-11),(-4,-11),(-3,-11),(-2,-11),(-1,-11),(0,-11),(1,-11),(2,-11),(3,-11),(4,-11),(5,-11),(6,-11),(7,-11),(8,-11),(9,-11),(-9,9),(-8,9),(-7,9),(-6,9),(-5,9),(-4,9),(-3,9),(-2,9),(-1,9),(0,9),(1,9),(2,9),(3,9),(4,9),(5,9),(6,9),(7,9),(8,9),(9,9),(-2,0),(2,0),(1,1),(-1,1),(2,1),(-2,1),(-2,-1),(-1,-1),(0,-1),(1,-1),(2,-1),(0,3),(0,4),(0,5),(0,7),(0,8),(0,-3),(0,-4),(0,-5),(0,-7),(0,-8),(0,-9),(1,5),(1,-3),(1,-7),(2,3),(2,5),(2,7),(2,-3),(2,-5),(2,-7),(2,-9),(3,3),(3,7),(3,-5),(3,-9),(4,1),(4,2),(4,3),(4,4),(4,5),(4,7),(4,-1),(4,-2),(4,-3),(4,-5),(4,-7),(4,-8),(4,-9),(5,-9),(6,1),(6,2),(6,3),(6,5),(6,7),(6,-1),(6,-2),(6,-3),(6,-5),(6,-6),(6,-7),(6,-9),(7,1),(7,3),(7,5),(7,7),(7,-1),(7,-3),(7,-5),(7,-9),(8,1),(8,3),(8,-1),(8,-3),(8,-7),(10,1),(10,-1),(2,0),(-2,0),(-1,1),(1,1),(-2,1),(2,1),(2,-1),(1,-1),(0,-1),(-1,-1),(-2,-1),(0,3),(0,4),(0,5),(0,7),(0,8),(0,-3),(0,-4),(0,-5),(0,-7),(0,-8),(0,-9),(-1,5),(-1,-3),(-1,-7),(-2,3),(-2,5),(-2,7),(-2,-3),(-2,-5),(-2,-7),(-2,-9),(-3,3),(-3,7),(-3,-5),(-3,-9),(-4,1),(-4,2),(-4,3),(-4,4),(-4,5),(-4,7),(-4,-1),(-4,-2),(-4,-3),(-4,-5),(-4,-7),(-4,-8),(-4,-9),(-5,-9),(-6,1),(-6,2),(-6,3),(-6,5),(-6,7),(-6,-1),(-6,-2),(-6,-3),(-6,-5),(-6,-6),(-6,-7),(-6,-9),(-7,1),(-7,3),(-7,5),(-7,7),(-7,-1),(-7,-3),(-7,-5),(-7,-9),(-8,1),(-8,3),(-8,-1),(-8,-3),(-8,-7),(-10,1),(-10,-1)]


    -- | Defines the list of pellets that are around the maze.
    pelletsL :: [Point]
    pelletsL = [(-8,-10),(-7,-10),(-6,-10),(-5,-10),(-4,-10),(-3,-10),(-2,-10),(-1,-10),(-5,-8),(-5,-7),(-5,-6),(-5,-5),(-5,-4),(-5,-3),(-5,-2),(-5,-1),(-5,0),(-5,1),(-5,2),(-5,3),(-5,4),(-5,5),(-5,6),(-5,7),(-5,8),(-8,-9),(-8,-8),(-7,-8),(-6,-8),(-7,-7),(-7,-6),(-8,-5),(-8,-4),(-1,-4),(-2,-4),(-3,-4),(-4,-4),(-6,-4),(-7,-4),(-8,-4),(-1,-5),(-5,-6),(-1,-6),(-2,-6),(-3,-6),(-4,-6),(-3,-7),(-3,-8),(-1,-8),(-2,-8),(-1,-9),(-1,4),(-1,6),(-1,7),(-1,8),(-2,4),(-2,6),(-2,8),(-3,4),(-3,5),(-3,6),(-3,8),(-4,6),(-4,8),(-6,4),(-6,6),(-6,8),(-7,4),(-7,6),(-7,8),(-8,4),(-8,5),(-8,6),(-8,8),(8,-10),(7,-10),(6,-10),(5,-10),(4,-10),(3,-10),(2,-10),(1,-10),(5,-8),(5,-7),(5,-6),(5,-5),(5,-4),(5,-3),(5,-2),(5,-1),(5,0),(5,1),(5,2),(5,3),(5,4),(5,5),(5,6),(5,7),(5,8),(8,-9),(8,-8),(7,-8),(6,-8),(7,-7),(7,-6),(8,-5),(8,-4),(1,-4),(2,-4),(3,-4),(4,-4),(6,-4),(7,-4),(8,-4),(1,-5),(5,-6),(1,-6),(2,-6),(3,-6),(4,-6),(3,-7),(3,-8),(1,-8),(2,-8),(1,-9),(1,4),(1,6),(1,7),(1,8),(2,4),(2,6),(2,8),(3,4),(3,5),(3,6),(3,8),(4,6),(4,8),(6,4),(6,6),(6,8),(7,4),(7,6),(7,8),(8,4),(8,5),(8,6),(8,8),(0,-10),(0,6)]


    -- | List of the 4 power pellets
    powerPellet :: [Point]
    powerPellet = [(-8,-6),(8,-6),(-8,7),(8,7)]

    -- | List of all the corssroads for the ghosts to make movement choices.
    crossR :: [Point]
    crossR = [(1,2),(1,6),(3,6),(5,6),(8,6),(5,8),(8,6),(3,-4),(5,-4),(1,-6),(3,-6),(5,-6),(7,-8),(1,-10),(-1,2),(-1,6),(-3,6),(-5,6),(-8,6),(-5,8),(-8,6),(-3,-4),(-5,-4),(-1,-6),(-3,-6),(-5,-6),(-7,-8),(-1,-10),(-5,4),(5,4)]


    -- | Turns a single point into a block picture to be displayed on the map
    renderS :: Point -> Picture
    renderS (x, y) =
        case (x,y) of
            (_,_) -> translate (x*(30)) (y*(30)) wallBlock -- Translation function in gloss
            where
                wallBlock = color (dark blue) $ rectangleSolid (30) (30) -- Block size


    -- | Takes the list of points turns them into the Pacman maze
    renderW :: [Point] -> Picture
    renderW walls = pictures [renderS walls | walls <- coords ]


    -- | Turns a single point into a pellet picture to be displayed on the map
    renderP :: Point -> Picture
    renderP (x, y) =
        case (x,y) of
            (_,_) -> translate (x*(30)) (y*(30)) pellet   -- Translation function in gloss
            where
                pellet = color white $ circleSolid (3)  -- Block size


    -- | Takes the list of points turns them into the pellets
    renderPS :: [Point] -> Picture
    renderPS pellet = pictures [renderP pellet | pellet <- pelletsL]


    -- | Turns a single point into a power pellet picture to be displayed on the map
    renderPP :: Point -> Picture
    renderPP (x, y) =
        case (x,y) of
            (_,_) -> translate (x*(30)) (y*(30)) pPellet   -- Translation function in gloss
            where
                pPellet = color white $ circleSolid (6)  -- Block size


    -- | Takes the list of points turns them into the power pellets
    renderPPS :: [Point] -> Picture
    renderPPS pPellet = pictures [renderPP pPellet | pPellet <- powerPellet]