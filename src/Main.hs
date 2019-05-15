--{-# LANGUAGE CPP, TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}
--{-# LANGUAGE FlexibleInstances    #-}
--{-# LANGUAGE UndecidableInstances #-}


module Main where

    import Graphics.Gloss
    import Graphics.Gloss.Interface.Pure.Game
    import Graphics.Gloss.Data.Point
    import Data.Fixed

    import Maze
    import Structs
    import GhostAI


    -- | Handle user inputs. Needed for gloss play function
    handleKeys :: Event -> PacGame -> PacGame
    handleKeys event game
            | curTime < 3 = game
            | otherwise =  case event of
                EventKey (SpecialKey KeyUp) _ _ _ -> game { bufDirection = UP }
                EventKey (SpecialKey KeyDown) _ _ _ -> game { bufDirection = DOWN }
                EventKey (SpecialKey KeyLeft) _ _ _ -> game { bufDirection = LEFT }
                EventKey (SpecialKey KeyRight) _ _ _ -> game { bufDirection = RIGHT }
                EventKey (Char 'w') _ _ _ -> game { bufDirection = UP }
                EventKey (Char 's') _ _ _ -> game { bufDirection = DOWN }
                EventKey (Char 'a') _ _ _ -> game { bufDirection = LEFT }
                EventKey (Char 'd') _ _ _ -> game { bufDirection = RIGHT }
                _ -> game
            where
                curTime = time game
        

    -- | Initialize base game state
    initialize :: PacGame 
    initialize = Game
        { lives = 3
        , gameStatus = PLAYING
        , pacman = Characters {cName = Pacman, speed = (0,0), location = (0,-6)}
        , pinky = Ghost {gName = Pinky, gSpeed = (0,0), gLocation = (0,0), gTarget = pinkyScatterTarget, gLastMove = STOP, gDirection = STOP}
        , inky = Ghost {gName = Inky, gSpeed = (0,0), gLocation = (-1,0), gTarget = inkyScatterTarget, gLastMove = STOP, gDirection = STOP}
        , blinky = Ghost {gName = Blinky, gSpeed = (0,0), gLocation = (0,2), gTarget = blinkyScatterTarget, gLastMove = STOP, gDirection = STOP}
        , clyde = Ghost {gName = Clyde, gSpeed = (0,0), gLocation = (1,0), gTarget = clydeScatterTarget, gLastMove = STOP, gDirection = STOP}
        , time = 0.0
        , score = 0
        , pellets = pelletsL
        , direction = LEFT
        , bufDirection = STOP
        , gMode = SCATTER
        , lcrB = (0,0)
        , lcrP = (0,0)
        , lcrI = (0,0)
        , lcrC = (0,0)
        , pPellets = powerPellet
        , fTime = 0.0
        }

    -- | Timer that runs to let me know when I can release the other ghosts into the maze.
    beginTimer :: PacGame -> PacGame
    beginTimer game = game {time = curTime+1/(fromIntegral fps)}
        where
            curTime = time game

    -- | Checks if point given is within the points that create the walls of the maze
    checkCollision :: Point -> Bool
    checkCollision c = (c `elem` coords)

    -- | Renders pacman's image based on the gamestate.
    renderPacman :: PacGame -> Picture
    renderPacman game =
        translate (x*30) (y*30) $ color yellow $ circleSolid 13
            where 
                (x, y) = (location $ pacman game)

    -- | Renders pinky's image based on the gamestate.
    renderPinky :: PacGame -> Picture
    renderPinky game
                | mode == CHASE || mode == SCATTER = translate (x*30) (y*30) $ color (light rose) $ circleSolid 13
                | target == (0,0) = translate (x*30) (y*30) $ color (dark white) $ circleSolid 13
                | otherwise = translate (x*30) (y*30) $ color blue $ circleSolid 13
            where 
                (x, y) = (gLocation $ pinky game)
                mode = gMode game
                target = gTarget (pinky game)
    
    -- | Renders Inky's image based on the gamestate.
    renderInky :: PacGame -> Picture
    renderInky game
                | mode == CHASE || mode == SCATTER = translate (x*30) (y*30) $ color cyan $ circleSolid 13
                | target == (0,0) = translate (x*30) (y*30) $ color (dark white) $ circleSolid 13
                | otherwise = translate (x*30) (y*30) $ color blue $ circleSolid 13
                where 
                    (x, y) = (gLocation $ inky game)
                    mode = gMode game
                    target = gTarget (inky game)


    -- | Renders Blinky's image based on the gamestate.
    renderBlinky :: PacGame -> Picture
    renderBlinky game
                | mode == CHASE || mode == SCATTER = translate (x*30) (y*30) $ color red $ circleSolid 13
                | target == (0,0) = translate (x*30) (y*30) $ color (dark white) $ circleSolid 13
                | otherwise = translate (x*30) (y*30) $ color blue $ circleSolid 13
                where 
                    (x, y) = (gLocation $ blinky game)
                    mode = gMode game
                    target = gTarget (blinky game)


    -- | Renders Clyde's image based on the gamestate.
    renderClyde :: PacGame -> Picture
    renderClyde game
            | mode == CHASE || mode == SCATTER = translate (x*30) (y*30) $ color orange $ circleSolid 13
            | target == (0,0) = translate (x*30) (y*30) $ color (dark white) $ circleSolid 13
            | otherwise = translate (x*30) (y*30) $ color blue $ circleSolid 13
            where 
                (x, y) = (gLocation $ clyde game)
                mode = gMode game
                target = gTarget (clyde game)


    renderStuff :: PacGame -> Picture
    renderStuff game
                | curTime <= 3 = translate (-30) (-65) $ scale 0.15 0.15 $ color yellow $ Text "Ready!"
                | otherwise = color white $ scale 0.2 0.2 $ pictures
                    [rScore, rLives, mode] 
                    where
                        rScore = translate (-1400) (1475) $ Text $ "Score: " ++ show (score game)
                        rLives = translate (-1800) (-1880) $ Text $ "Lives: "
                        mode = translate (200) (1475) $ Text $ "Mode: " ++ show (gMode game)
                        curTime = time game

    -- | Renders the pellets images based on the gamestate.
    renderPellets :: PacGame -> Picture
    renderPellets game = pictures $ [translate (x*30) (y*30) $ color white $ circleSolid 4 | (x,y) <- pellets game]

    -- | Renders the power pellet images based on the gamestate.
    renderPowerPellets :: PacGame -> Picture
    renderPowerPellets game
                        | mod' curTime 0.3 >= 0 && mod' curTime 0.3 <= 0.15 = pictures $ [translate (x*30) (y*30) $ color white $ circleSolid 8 | (x,y) <- pPellets game]
                        | otherwise = Blank
                        where
                            curTime = time game

    -- | Render the lives on the bottom of the screen
    renderLives :: PacGame -> Picture
    renderLives game
                | curTime <= 3 = Blank
                | curLives >= 3 = pictures $ [renderOne, renderTwo, renderThree]
                | curLives == 2 = pictures $ [renderOne, renderTwo]
                | curLives == 1 = pictures $ [renderOne]
                | otherwise = Blank
                where
                    curTime = time game
                    curLives = lives game
                    renderOne = translate (-270) (-370) $ color yellow $ circleSolid 13
                    renderTwo = translate (-235) (-370) $ color yellow $ circleSolid 13
                    renderThree = translate (-200) (-370) $ color yellow $ circleSolid 13


    -- | Main display window.
    displayWindow :: Display
    displayWindow = InWindow "Pac-Man - by Jack Kelly" (800,800) (350, 350)

    -- | Renders the victory screen
    renderVictory :: Picture
    renderVictory = translate (-250) (0) $ scale 0.3 0.3 $ color white $ Text "Congradulations! You Win!"

    -- | Renders the defeat screen
    renderDefeat :: Picture
    renderDefeat = translate (-170) (0) $ scale 0.4 0.4 $ color white $ Text "Game Over"


    -- | Main render function used in play. List of multiple other render functions that layer on one another.
    render :: PacGame -> Picture
    render game = case gameStatus game of
        WON -> renderVictory
        LOST -> renderDefeat
        PLAYING -> pictures $ [(renderLives game),(renderPowerPellets game),(renderW coords),(renderPacman game),(renderStuff game),(renderPellets game),(renderPinky game),(renderInky game),(renderBlinky game),(renderClyde game)]


    -- | Main play function of the game
    main :: IO ()
    main = do 
        play displayWindow black fps initialize render handleKeys step
        where
            step :: Float -> PacGame -> PacGame
            step sec game = sMove sec $ releaseInky $ releasePinky $ releaseClyde $ moveToTargetC $ moveToTargetI $ moveToTargetP $ moveToTargetB $ ghostModeSwitch $ beginTimer $ ghostCollisionSwitch $ pacScore $ pacEat $ pacEatP $ wallCollision $ execM $ checkTeleport game
    

    -- | This is the standard movement function. Take the velocity and position, then update it with (position + velocity * time)
    sMove :: Float -> PacGame -> PacGame
    sMove sec game = game {pacman = Characters {cName = Pacman, location = (x', y'), speed = (vx,vy)}, 
                            blinky = Ghost {gName = Blinky, gLocation = (bx',by'), gSpeed = (bvx,bvy), gTarget = curTargetB, gLastMove = curLMB, gDirection = curDirectionB},
                            pinky = Ghost {gName = Pinky, gLocation = (px',py'), gSpeed = (pvx,pvy), gTarget = curTargetP, gLastMove = curLMP, gDirection = curDirectionP},
                            inky = Ghost {gName = Inky, gLocation = (ix',iy'), gSpeed = (ivx,ivy), gTarget = curTargetI, gLastMove = curLMI, gDirection = curDirectionI},
                            clyde = Ghost {gName = Clyde, gLocation = (cx',cy'), gSpeed = (cvx,cvy), gTarget = curTargetC, gLastMove = curLMC, gDirection = curDirectionC}
                            }
        where
            (x, y) = (location $ pacman game)
            (vx, vy) = (speed $ pacman game)
            x' = x + vx * sec
            y' = y + vy * sec

            ------ Blinky
            (bx, by) = (gLocation (blinky game))
            (bvx, bvy) = (gSpeed (blinky game))
            bx' = bx + bvx * sec
            by' = by + bvy * sec
            curTargetB = (gTarget (blinky game))
            curLMB = (gLastMove (blinky game))
            curDirectionB = (gDirection (blinky game))

            ------ Pinky
            (px, py) = (gLocation (pinky game))
            (pvx, pvy) = (gSpeed (pinky game))
            px' = px + pvx * sec
            py' = py + pvy * sec
            curTargetP = (gTarget (pinky game))
            curLMP = (gLastMove (pinky game))
            curDirectionP = (gDirection (pinky game))

            ------ Inky
            (ix, iy) = (gLocation (inky game))
            (ivx, ivy) = (gSpeed (inky game))
            ix' = ix + ivx * sec
            iy' = iy + ivy * sec
            curTargetI = (gTarget (inky game))
            curLMI = (gLastMove (inky game))
            curDirectionI = (gDirection (inky game))

            ------ Clyde
            (cx, cy) = (gLocation (clyde game))
            (cvx, cvy) = (gSpeed (clyde game))
            cx' = cx + cvx * sec
            cy' = cy + cvy * sec
            curTargetC = (gTarget (clyde game))
            curLMC = (gLastMove (clyde game))
            curDirectionC = (gDirection (clyde game))

        
    -- | Returns true if pacman is stopped.
    notMoving :: PacGame -> Bool
    notMoving game = if direction game == STOP then True else False


    -- | Function that handles pacmans movements
    execM :: PacGame -> PacGame
    execM game
        | curTime <= 2.5 = game
        | direction game == bufDirection game = game                -- If the buffer direction and the game direction are equal, keep going
        | direction game /= bufDirection game = newgame             -- If the buffer direction and the game direction are not euqal, use newgame
        | otherwise = game                                          -- This shouldn't trigger, but there for compiling sake
        where
            curTime = time game
            newgame 
                | (vx, vy) == (0,0) = case bufDirection game of     -- Check the cases of the buffer if the pacman is stopped.
                    UP -> up                                        -- Will move the pacman UP
                    DOWN -> down                                    -- Will move the pacman DOWN
                    LEFT -> left                                    -- Will move the pacman LEFT
                    RIGHT -> right                                  -- Will move the pacman RIGHT
                    STOP -> stop                                    -- Will stop the pacman
                | (vx, vy) /= (0, 0) = case bufDirection game of    -- Case where the pacman is moving but the buffer is different. Makes sure the pacman can change directions when moving into other paths
                    UP -> tUP                                       -- Will change direction to UP
                    DOWN -> tDOWN                                   -- Will change direction to DOWN
                    LEFT -> tLEFT                                   -- Will change direction to LEFT
                    RIGHT -> tRIGHT                                 -- Will change direction to RIGHT
                    STOP -> tSTOP                                   -- Will stop the pacman
                | otherwise = game
                where
                    (x,y) = (location (pacman game))
                    (vx, vy) = (speed (pacman game))
                    up = game {direction = UP, pacman = Characters {cName = Pacman, speed = (0,4), location = (x,y)}}
                    down = game {direction = DOWN, pacman = Characters {cName = Pacman, speed = (0,-4), location = (x,y)}}
                    left = game {direction = LEFT, pacman = Characters {cName = Pacman, speed = (-4,0), location = (x,y)}}
                    right = game {direction = RIGHT, pacman = Characters {cName = Pacman, speed = (4,0), location = (x,y)}}
                    stop = game {direction = STOP, pacman = Characters {cName = Pacman, speed = (0,0), location = (x,y)}}
                    x' = fromIntegral (round x)
                    y' = fromIntegral (round y)
                    tUP = if (elem (x', (y'+1)) coords) then game else game {direction = UP, pacman = Characters {cName = Pacman, speed = (0,4), location = (x',y')}}
                    tDOWN = if (elem (x', (y'-1)) coords) then game else game {direction = DOWN, pacman = Characters {cName = Pacman, speed = (0,-4), location = (x',y')}}
                    tLEFT = if (elem ((x'+(-1)), y') coords) then game else game {direction = LEFT, pacman = Characters {cName = Pacman, speed = (-4,0), location = (x',y')}}
                    tRIGHT = if (elem ((x'+1), y') coords) then game else game {direction = RIGHT, pacman = Characters {cName = Pacman, speed = (4,0), location = (x',y')}}
                    tSTOP = game



    -- | Collision checks on walls
    wallCollision :: PacGame -> PacGame
    wallCollision game = case direction game of         -- Checks cases for the current moving direction
        UP -> up                                        -- Checks if position above is a wall. If not then move UP
        DOWN -> down                                    -- Checks if position below is a wall. If not then move DOWN
        LEFT -> left                                    -- Checks if position left is a wall. If not then move LEFT
        RIGHT -> right                                  -- Checks if position right is a wall. If not then move RIGHTs
        STOP -> stop                                    -- stops the pacman
        where
            (x,y) = (location (pacman game))
            (vx, vy) = (speed (pacman game))
            x' = fromIntegral (round x)
            y' = fromIntegral (round y)
            yU = y' + 1
            yD = y' + (-1)
            xL = x' + (-1)
            xR = x' + 1
            up = if (elem (x',yU) coords) then game {direction = STOP, pacman = Characters {cName = Pacman, speed = (0,0), location = (x',y')}} else game
            down = if (elem (x',yD) coords) then game {direction = STOP, pacman = Characters {cName = Pacman, speed = (0,0), location = (x',y')}} else game
            left = if (elem (xL,y') coords) then game {direction = STOP, pacman = Characters {cName = Pacman, speed = (0,0), location = (x',y')}} else game
            right = if (elem (xR,y') coords) then game {direction = STOP, pacman = Characters {cName = Pacman, speed = (0,0), location = (x',y')}} else game
            stop = game {direction = STOP, pacman = Characters {cName = Pacman, speed = (0,0), location = (x',y')}}


    -- | Function that checks if pacman is over a pellet. If so then remove it from the list held in the gamestate
    pacEat :: PacGame -> PacGame
    pacEat game
            | (x',y') `elem` pelletsL = game {pellets = [x | x <- curPellets, x /= (x',y')]}
            | otherwise = game
            where
                (x,y) = (location (pacman game))
                x' = fromIntegral (round x)
                y' = fromIntegral (round y)
                curPellets = pellets game
                curScore = (score game)

    -- | Function that checks if pacman is over a power pellet. If so then remove it from the list held in the gamestate and change mode to frightened
    pacEatP :: PacGame -> PacGame
    pacEatP game
            | (x',y') `elem` pPelletsL = game {fTime = 0, gMode = FRIGHTENED, pPellets = [x | x <- curPellets, x /= (x',y')]}
            | otherwise = game
            where
                (x,y) = (location (pacman game))
                x' = fromIntegral (round x)
                y' = fromIntegral (round y)
                curPellets = pPellets game
                curScore = (score game)
                pPelletsL = pPellets game



    -- | Determines the current score of your game. Regular pellets are worth 10 points. Large Pellets are worth 50
    pacScore :: PacGame -> PacGame
    pacScore game 
                | curScore >= 1700 = game {gameStatus = WON}
                | otherwise = game {score = newscore}
                where
                    newscore = (((listLength pelletsL) - (listLength curPellets))*10) + (((listLength powerPellet) - (listLength curPowerPellets))*50)
                    curPellets = pellets game
                    curScore = score game
                    curPowerPellets = pPellets game


    -- | Check if pacman is at teleport location. If so, teleport him to the other side
    checkTeleport :: PacGame -> PacGame
    checkTeleport game
                    | (x',y') == (-10,0) = game {direction = curDirection, pacman = Characters {cName = Pacman, speed = curSpeed, location = (9,0)}}
                    | (x',y') == (10,0) = game {direction = curDirection, pacman = Characters {cName = Pacman, speed = curSpeed, location = (-9,0)}}
                    | otherwise = game
                    where
                        (x,y) = (location (pacman game))
                        x' = fromIntegral (round x)
                        y' = fromIntegral (round y)
                        curSpeed = (speed (pacman game))
                        curDirection = direction game



    -- | Checks if you ran into any of the ghosts. Decreases a life it above 0. Sets gameStatus to LOST if out of lives.
    ghostCollision :: PacGame -> PacGame
    ghostCollision game
                    | (x',y') == (bx',by') = checkLives
                    | (x',y') == (px',py') = checkLives
                    | (x',y') == (ix',iy') = checkLives
                    | (x',y') == (cx',cy') = checkLives
                    | otherwise = game
                    where
                        (x,y) = (location (pacman game))
                        x' = fromIntegral (round x)
                        y' = fromIntegral (round y)
                        ----- Ghost locations
                        (bx,by) = (gLocation (blinky game))
                        (px,py) = (gLocation (pinky game))
                        (ix,iy) = (gLocation (inky game))
                        (cx,cy) = (gLocation (clyde game))

                        bx' = fromIntegral (round bx)
                        by' = fromIntegral (round by)

                        px' = fromIntegral (round px)
                        py' = fromIntegral (round py)

                        ix' = fromIntegral (round ix)
                        iy' = fromIntegral (round iy)

                        cx' = fromIntegral (round cx)
                        cy' = fromIntegral (round cy)

                        curLives = lives game

                        checkLives = if (curLives > 1) then
                            game {gMode = SCATTER, lcrB = (0,0), lcrP = (0,0), lcrI = (0,0), lcrC = (0,0), time = 0.0, direction = STOP, bufDirection = STOP, lives = (curLives-1),
                                    pacman = Characters {cName = Pacman, speed = (0,0), location = (0,-6)}
                                    , pinky = Ghost {gName = Pinky, gSpeed = (0,0), gLocation = (0,0), gTarget = pinkyScatterTarget, gLastMove = STOP, gDirection = STOP}
                                    , inky = Ghost {gName = Inky, gSpeed = (0,0), gLocation = (-1,0), gTarget = inkyScatterTarget, gLastMove = STOP, gDirection = STOP}
                                    , blinky = Ghost {gName = Blinky, gSpeed = (0,0), gLocation = (0,2), gTarget = blinkyScatterTarget, gLastMove = STOP, gDirection = STOP}
                                    , clyde = Ghost {gName = Clyde, gSpeed = (0,0), gLocation = (1,0), gTarget = clydeScatterTarget, gLastMove = STOP, gDirection = STOP}}
                                    else
                                        game {gameStatus = LOST}