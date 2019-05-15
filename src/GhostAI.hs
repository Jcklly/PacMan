--{-# LANGUAGE FlexibleInstances    #-}
--{-# LANGUAGE UndecidableInstances #-}
module GhostAI where

    import Structs
    import Maze
    import Graphics.Gloss


    -- | Length of a list. Used for the score
    listLength :: [a] -> Int
    listLength [] = 0
    listLength (x:xs) = 1 + listLength xs


    -- | Reverse the True and False statements from the standard `elem` function
    checkInList :: Eq a => a -> [a] -> Bool
    checkInList x l 
                | x `elem` l = False
                | otherwise = True


    -- Scatter for 7 seconds, then Chase for 20 seconds.
    -- Scatter for 7 seconds, then Chase for 20 seconds.
    -- Scatter for 5 seconds, then Chase for 20 seconds.
    -- Scatter for 5 seconds, then switch to Chase mode permanently.
    -- add 3 seconds to everything because I stop the first 3 seconds of the game for the start screen
    -- | Changing from scatter to chase follow Pac-Mans rules.   
    ghostModeTimer :: PacGame -> PacGame
    ghostModeTimer game
                    | curTime < 10 = game {gMode = SCATTER, blinky = Ghost {gName = Blinky, gSpeed = curSpeedB, gLocation = curLocationB, gTarget = blinkyScatterTarget, gLastMove = lmB, gDirection = dB}, pinky = Ghost {gName = Pinky, gSpeed = curSpeedP, gLocation = curLocationP, gTarget = pinkyScatterTarget, gLastMove = lmP, gDirection = dP}, inky = Ghost {gName = Inky, gSpeed = curSpeedI, gLocation = curLocationI, gTarget = inkyScatterTarget, gLastMove = lmI, gDirection = dI}, clyde = Ghost {gName = Clyde, gSpeed = curSpeedC, gLocation = curLocationC, gTarget = clydeScatterTarget, gLastMove = lmC, gDirection = dC}}
                    | curTime >= 10 && curTime < 30 = game {gMode = CHASE, blinky = Ghost {gName = Blinky, gSpeed = curSpeedB, gLocation = curLocationB, gTarget = (x',y'), gLastMove = lmB, gDirection = dB}, pinky = Ghost {gName = Pinky, gSpeed = curSpeedP, gLocation = curLocationP, gTarget = (x'+3,y'+3), gLastMove = lmP, gDirection = dP}, inky = Ghost {gName = Inky, gSpeed = curSpeedI, gLocation = curLocationI, gTarget = targetI, gLastMove = lmI, gDirection = dI}, clyde = Ghost {gName = Clyde, gSpeed = curSpeedC, gLocation = curLocationC, gTarget = targetC, gLastMove = lmC, gDirection = dC}}
                    | curTime >= 30 && curTime < 37 = game {gMode = SCATTER, blinky = Ghost {gName = Blinky, gSpeed = curSpeedB, gLocation = curLocationB, gTarget = blinkyScatterTarget, gLastMove = lmB, gDirection = dB}, pinky = Ghost {gName = Pinky, gSpeed = curSpeedP, gLocation = curLocationP, gTarget = pinkyScatterTarget, gLastMove = lmP, gDirection = dP}, inky = Ghost {gName = Inky, gSpeed = curSpeedI, gLocation = curLocationI, gTarget = inkyScatterTarget, gLastMove = lmI, gDirection = dI}, clyde = Ghost {gName = Clyde, gSpeed = curSpeedC, gLocation = curLocationC, gTarget = clydeScatterTarget, gLastMove = lmC, gDirection = dC}}
                    | curTime >= 37 && curTime < 57 = game {gMode = CHASE, blinky = Ghost {gName = Blinky, gSpeed = curSpeedB, gLocation = curLocationB, gTarget = (x',y'), gLastMove = lmB, gDirection = dB}, pinky = Ghost {gName = Pinky, gSpeed = curSpeedP, gLocation = curLocationP, gTarget = (x'+3,y'+3), gLastMove = lmP, gDirection = dP}, inky = Ghost {gName = Inky, gSpeed = curSpeedI, gLocation = curLocationI, gTarget = targetI, gLastMove = lmI, gDirection = dI}, clyde = Ghost {gName = Clyde, gSpeed = curSpeedC, gLocation = curLocationC, gTarget = targetC, gLastMove = lmC, gDirection = dC}}
                    | curTime >= 57 && curTime < 62 = game {gMode = SCATTER, blinky = Ghost {gName = Blinky, gSpeed = curSpeedB, gLocation = curLocationB, gTarget = blinkyScatterTarget, gLastMove = lmB, gDirection = dB}, pinky = Ghost {gName = Pinky, gSpeed = curSpeedP, gLocation = curLocationP, gTarget = pinkyScatterTarget, gLastMove = lmP, gDirection = dP}, inky = Ghost {gName = Inky, gSpeed = curSpeedI, gLocation = curLocationI, gTarget = inkyScatterTarget, gLastMove = lmI, gDirection = dI}, clyde = Ghost {gName = Clyde, gSpeed = curSpeedC, gLocation = curLocationC, gTarget = clydeScatterTarget, gLastMove = lmC, gDirection = dC}}
                    | curTime >= 62 && curTime < 82 = game {gMode = CHASE, blinky = Ghost {gName = Blinky, gSpeed = curSpeedB, gLocation = curLocationB, gTarget = (x',y'), gLastMove = lmB, gDirection = dB}, pinky = Ghost {gName = Pinky, gSpeed = curSpeedP, gLocation = curLocationP, gTarget = (x'+3,y'+3), gLastMove = lmP, gDirection = dP}, inky = Ghost {gName = Inky, gSpeed = curSpeedI, gLocation = curLocationI, gTarget = targetI, gLastMove = lmI, gDirection = dI}, clyde = Ghost {gName = Clyde, gSpeed = curSpeedC, gLocation = curLocationC, gTarget = targetC, gLastMove = lmC, gDirection = dC}}
                    | curTime >= 82 && curTime < 87 = game {gMode = SCATTER, blinky = Ghost {gName = Blinky, gSpeed = curSpeedB, gLocation = curLocationB, gTarget = blinkyScatterTarget, gLastMove = lmB, gDirection = dB}, pinky = Ghost {gName = Pinky, gSpeed = curSpeedP, gLocation = curLocationP, gTarget = pinkyScatterTarget, gLastMove = lmP, gDirection = dP}, inky = Ghost {gName = Inky, gSpeed = curSpeedI, gLocation = curLocationI, gTarget = inkyScatterTarget, gLastMove = lmI, gDirection = dI}, clyde = Ghost {gName = Clyde, gSpeed = curSpeedC, gLocation = curLocationC, gTarget = clydeScatterTarget, gLastMove = lmC, gDirection = dC}}
                    | curTime >= 87 && curTime < 107 = game {gMode = CHASE, blinky = Ghost {gName = Blinky, gSpeed = curSpeedB, gLocation = curLocationB, gTarget = (x',y'), gLastMove = lmB, gDirection = dB}, pinky = Ghost {gName = Pinky, gSpeed = curSpeedP, gLocation = curLocationP, gTarget = (x'+3,y'+3), gLastMove = lmP, gDirection = dP}, inky = Ghost {gName = Inky, gSpeed = curSpeedI, gLocation = curLocationI, gTarget = targetI, gLastMove = lmI, gDirection = dI}, clyde = Ghost {gName = Clyde, gSpeed = curSpeedC, gLocation = curLocationC, gTarget = targetC, gLastMove = lmC, gDirection = dC}}
                    | curTime >= 107 && curTime < 112 = game {gMode = SCATTER, blinky = Ghost {gName = Blinky, gSpeed = curSpeedB, gLocation = curLocationB, gTarget = blinkyScatterTarget, gLastMove = lmB, gDirection = dB}, pinky = Ghost {gName = Pinky, gSpeed = curSpeedP, gLocation = curLocationP, gTarget = pinkyScatterTarget, gLastMove = lmP, gDirection = dP}, inky = Ghost {gName = Inky, gSpeed = curSpeedI, gLocation = curLocationI, gTarget = inkyScatterTarget, gLastMove = lmI, gDirection = dI}, clyde = Ghost {gName = Clyde, gSpeed = curSpeedC, gLocation = curLocationC, gTarget = clydeScatterTarget, gLastMove = lmC, gDirection = dC}}
                    | otherwise = game {gMode = CHASE, blinky = Ghost {gName = Blinky, gSpeed = curSpeedB, gLocation = curLocationB, gTarget = (x',y'), gLastMove = lmB, gDirection = dB}, pinky = Ghost {gName = Pinky, gSpeed = curSpeedP, gLocation = curLocationP, gTarget = (x'+3,y'+3), gLastMove = lmP, gDirection = dP}, inky = Ghost {gName = Inky, gSpeed = curSpeedI, gLocation = curLocationI, gTarget = targetI, gLastMove = lmI, gDirection = dI}, clyde = Ghost {gName = Clyde, gSpeed = curSpeedC, gLocation = curLocationC, gTarget = targetC, gLastMove = lmC, gDirection = dC}}
                    where
                        curTime = time game
                        (x,y) = (location (pacman game))
                        x' = fromIntegral (round x)
                        y' = fromIntegral (round y)

                        --- Blinky ---
                        curLocationB = (gLocation (blinky game))
                        curSpeedB = (gSpeed (blinky game))
                        lmB = (gLastMove (blinky game))
                        dB = (gDirection (blinky game))

                        --- Pinky ---
                        curLocationP = (gLocation (pinky game))
                        curSpeedP = (gSpeed (pinky game))
                        lmP = (gLastMove (pinky game))
                        dP = (gDirection (pinky game))

                        --- Inky ---
                        curLocationI = (gLocation (inky game))
                        curSpeedI = (gSpeed (inky game))
                        lmI = (gLastMove (inky game))
                        dI = (gDirection (inky game))
                        targetI = targetInky game

                        --- Clyde ---
                        curLocationC = (gLocation (clyde game))
                        curSpeedC = (gSpeed (clyde game))
                        lmC = (gLastMove (clyde game))
                        dC = (gDirection (clyde game))
                        targetC = targetClyde game



    -- | Determines whether or not pacman ate the power pellet.
    ghostModeSwitch :: PacGame -> PacGame
    ghostModeSwitch game
                        | curMode == SCATTER || curMode == CHASE = ghostModeTimer game
                        | frTime >= frightenLength = ghostModeTimer game
                        | frTime < frightenLength = game {fTime = frTime+1/(fromIntegral fps)}
                        | otherwise = game
                        where
                            curMode = gMode game
                            frTime = fTime game



    -- | Function to release Inky. Originally tried to fit them all in one, but it did not work
    releaseInky :: PacGame -> PacGame
    releaseInky game = if curTime > 4 && (dotsLeft >= 10) && (((px,py) == (-1,0)) || (dotsLeft >= 10 && (px,py) == (0,0)) || (dotsLeft >= 10 && (px,py) == (1,0)))
                        then game {inky = Ghost {gName = Inky, gSpeed = (0,0), gLocation = (0,2), gTarget = curTargetI, gLastMove = STOP, gDirection = STOP}}
                        else game
                        where
                            curTime = time game
                            curTargetI = (gTarget (inky game))
                            dotsLeft = (listLength pelletsL) - (listLength curPellets)
                            curPellets = pellets game
                            (px,py) = (gLocation (inky game))
                            px' = fromIntegral (round px)
                            py' = fromIntegral (round py)


    -- | Function to release Clyde.
    releaseClyde :: PacGame -> PacGame
    releaseClyde game = if curTime > 4 && (dotsLeft >= 50) && (((cx',cy') == (-1,0)) || (dotsLeft >= 50 && (cx',cy') == (0,0)) || (dotsLeft >= 50 && (cx',cy') == (1,0)))
                        then game {clyde = Ghost {gName = Clyde, gSpeed = (0,0), gLocation = (0,2), gTarget = curTargetC, gLastMove = STOP, gDirection = STOP}}
                        else game
                        where
                            curTime = time game
                            curTargetC = (gTarget (clyde game))
                            dotsLeft = (listLength pelletsL) - (listLength curPellets)
                            curPellets = pellets game
                            (cx,cy) = (gLocation (clyde game))
                            cx' = fromIntegral (round cx)
                            cy' = fromIntegral (round cy)



    -- | Function to release Pinky, Inky, and Clyde at their appropriate times.
    releasePinky :: PacGame -> PacGame
    releasePinky game = if curTime > 4 && posPinky == (0,0) then game {pinky = Ghost {gName = Pinky, gSpeed = (0,0), gLocation = (0,2), gTarget = curTargetP, gLastMove = STOP, gDirection = STOP}}
                        else game
                    where
                        curTime = time game
                        curTargetP = (gTarget (pinky game))
                        dotsLeft = (listLength pelletsL) - (listLength curPellets)
                        curPellets = pellets game
                        posPinky = (gLocation (pinky game))


    -- | Calculate distance between 2 points
    distance :: Point -> Point -> Float
    distance (x1,y1) (x2,y2) = sqrt ((x'*x') + (y'*y'))
        where
            x' = x2 - x1
            y' = y2 - y1


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


    -- | Checks if you ran into any of the ghosts. Decreases a life it above 0. Sets gameStatus to LOST if out of lives.
    ghostCollisionSwitch :: PacGame -> PacGame
    ghostCollisionSwitch game
                    | mode == FRIGHTENED = case (checkEaten game) of
                        1 -> eatBlinky
                        2 -> eatPinky
                        3 -> eatInky
                        4 -> eatClyde
                        5 -> game
                    | otherwise = ghostCollision game
                    where
                        eatBlinky =  game {blinky = Ghost {gName = Blinky, gSpeed = (0,0), gLocation = (bx,by), gTarget = (0,0), gLastMove = STOP, gDirection = STOP}}
                        eatPinky =  game {pinky = Ghost {gName = Pinky, gSpeed = (0,0), gLocation = (px,py), gTarget = (0,0), gLastMove = STOP, gDirection = STOP}}
                        eatInky =  game {inky = Ghost {gName = Inky, gSpeed = (0,0), gLocation = (ix,iy), gTarget = (0,0), gLastMove = STOP, gDirection = STOP}}
                        eatClyde =  game {clyde = Ghost {gName = Clyde, gSpeed = (0,0), gLocation = (cx,cy), gTarget = (0,0), gLastMove = STOP, gDirection = STOP}}

                        curScore = score game
                        mode = gMode game
                        (bx,by) = (gLocation (blinky game))
                        (px,py) = (gLocation (pinky game))
                        (ix,iy) = (gLocation (inky game))
                        (cx,cy) = (gLocation (clyde game))


                
    checkEaten :: PacGame -> Int
    checkEaten game
                | (x',y') == (bx',by') = 1
                | (x',y') == (px',py') = 2
                | (x',y') == (ix',iy') = 3
                | (x',y') == (cx',cy') = 4
                | otherwise = 5
                where
                    (x,y) = (location (pacman game))
                    x' = fromIntegral (round x)
                    y' = fromIntegral (round y)
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
                
            
-------------------- BLINKY ------------------ 

    -- | Moves Blinky to the target...which is either the top right corner or pacman
    moveToTargetB :: PacGame -> PacGame
    moveToTargetB game
                | curTime <= 3 = game
                | (elem (x',y') crossR) && (lastCR /= (x',y')) = game {lcrB = (x',y'), blinky = Ghost {gName = Blinky, gSpeed = (0,0), gLocation = (x,y), gTarget = curTarget, gLastMove = lm, gDirection = cD}}
                | (vx,vy) == (0,0) = case bWalls of
                    1 -> b1
                    2 -> b2
                    3 -> b3
                    4 -> b4
                    5 -> b5
                    10 -> b10
                | (vx,vy) /= (0,0) = case (gDirection (blinky game)) of
                    UP -> testUp
                    DOWN -> testDown
                    LEFT -> testLeft
                    RIGHT -> testRight
                    STOP -> stop
                | otherwise = game
                where
                    (tx,ty) = (gTarget (blinky game))
                    (vx,vy) = (gSpeed (blinky game))
                    (x,y) = (gLocation (blinky game))
                    x' = fromIntegral (round x)
                    y' = fromIntegral (round y)
                    yU = y' + 1
                    yD = y' + (-1)
                    xL = x' + (-1)
                    xR = x' + 1
                    lastCR = lcrB game
                    curTarget = (gTarget (blinky game))
                    lm = (gLastMove (blinky game))
                    cD = (gDirection (blinky game))
                    curTime = time game

                    b1 = if lm /= UP && (distance (x',yU) (tx,ty)) < (distance (x',yD) (tx,ty)) && (distance (x',yU) (tx,ty)) < (distance (xR,y') (tx,ty)) && (distance (x',yU) (tx,ty)) < (distance (xL,y') (tx,ty))
                        then goUp
                    else if lm /= DOWN && (distance (x',yD) (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (x',yD) (tx,ty)) < (distance (xR,y') (tx,ty)) && (distance (x',yD) (tx,ty)) < (distance (xL,y') (tx,ty))
                        then goDown
                    else if lm /= RIGHT && (distance (xR,y') (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (xR,y') (tx,ty)) < (distance (xL,y') (tx,ty)) && (distance (xR,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                        then goRight
                    else if lm /= LEFT && (distance (xL,y') (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (xL,y') (tx,ty)) < (distance (xR,y') (tx,ty)) && (distance (xL,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                        then goLeft
                    else if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords) then goDown
                    else game

                    b2 = if lm /= UP && (distance (x',yU) (tx,ty)) <= (distance (xR,y') (tx,ty)) && (distance (x',yU) (tx,ty)) <= (distance (xL,y') (tx,ty))
                        then goUp
                    else if lm /= RIGHT && (distance (xR,y') (tx,ty)) <= (distance (x',yU) (tx,ty)) && (distance (xR,y') (tx,ty)) <= (distance (xL,y') (tx,ty))
                        then goRight
                    else if lm /= LEFT && (distance (xL,y') (tx,ty)) <= (distance (x',yU) (tx,ty)) && (distance (xL,y') (tx,ty)) <= (distance (xR,y') (tx,ty))
                        then goLeft
                    else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords) then goDown
                    else game

                    b3 = if lm /= UP && (distance (x',yU) (tx,ty)) < (distance (x',yD) (tx,ty)) && (distance (x',yU) (tx,ty)) < (distance (xR,y') (tx,ty))
                        then goUp
                    else if lm /= DOWN && (distance (x',yD) (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (x',yD) (tx,ty)) < (distance (xR,y') (tx,ty))
                        then goDown
                    else if lm /= RIGHT && (distance (xR,y') (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (xR,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                        then goRight
                    else if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords) then goDown
                    else game

                    b4 = if lm /= DOWN && (distance (x',yD) (tx,ty)) < (distance (xR,y') (tx,ty)) && (distance (x',yD) (tx,ty)) < (distance (xL,y') (tx,ty))
                        then goDown
                    else if lm /= RIGHT && (distance (xR,y') (tx,ty)) < (distance (xL,y') (tx,ty)) && (distance (xR,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                        then goRight
                    else if lm /= LEFT && (distance (xL,y') (tx,ty)) < (distance (xR,y') (tx,ty)) && (distance (xL,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                        then goLeft
                    else if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords) then goDown
                    else game

                    b5 = if lm /= UP && (distance (x',yU) (tx,ty)) < (distance (x',yD) (tx,ty)) && (distance (x',yU) (tx,ty)) < (distance (xL,y') (tx,ty))
                        then goUp
                    else if lm /= DOWN && (distance (x',yD) (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (x',yD) (tx,ty)) < (distance (xL,y') (tx,ty))
                        then goDown
                    else if lm /= LEFT && (distance (xL,y') (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (xL,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                        then goLeft
                    else if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords) then goDown
                    else game

                    b10 = if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords)
                        then goDown
                    else game 

                    -- Determines where the walls are for a specific ghost location
                    bWalls = findWalls game 1

                    -- Movement
                    goRight = game {blinky = Ghost {gName = Blinky, gSpeed = (3.7,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = LEFT, gDirection = RIGHT}}
                    goLeft = game {blinky = Ghost {gName = Blinky, gSpeed = (-3.7,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = RIGHT, gDirection = LEFT}}
                    goUp = game {blinky = Ghost {gName = Blinky, gSpeed = (0,3.7), gLocation = (x',y'), gTarget = curTarget, gLastMove = DOWN, gDirection = UP}}
                    goDown = game {blinky = Ghost {gName = Blinky, gSpeed = (0,-3.7), gLocation = (x',y'), gTarget = curTarget, gLastMove = UP, gDirection = DOWN}}

                    -- When velocity isn't at 0, check collision of wall.
                    testUp = if (elem (x',yU) coords) then game {blinky = Ghost {gName = Blinky, gSpeed = (0,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = lm, gDirection = cD}} else game
                    testDown = if (elem (x',yD) coords) then game {blinky = Ghost {gName = Blinky, gSpeed = (0,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = lm, gDirection = cD}} else game
                    testLeft = if (elem (xL,y') coords) then game {blinky = Ghost {gName = Blinky, gSpeed = (0,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = lm, gDirection = cD}} else game
                    testRight = if (elem (xR,y') coords) then game {blinky = Ghost {gName = Blinky, gSpeed = (0,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = lm, gDirection = cD}} else game
                    stop = game {blinky = Ghost {gName = Blinky, gSpeed = (0,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = lm, gDirection = cD}}


    -------------------- PINKY ------------------

    -- | Moves Pinky to the target...which is either the top left corner or a few spaces in front of pacman
    moveToTargetP :: PacGame -> PacGame
    moveToTargetP game
                | curTime <= 4 = game
                | (elem (x',y') crossR) && (lastCR /= (x',y')) = game {lcrP = (x',y'), pinky = Ghost {gName = Pinky, gSpeed = (0,0), gLocation = (x,y), gTarget = (curTarget), gLastMove = lm, gDirection = cD}}
                | (vx,vy) == (0,0) = case pWalls of
                    1 -> p1
                    2 -> p2
                    3 -> p3
                    4 -> p4
                    5 -> p5
                    10 -> p10
                | (vx,vy) /= (0,0) = case (gDirection (pinky game)) of
                    UP -> testUp
                    DOWN -> testDown
                    LEFT -> testLeft
                    RIGHT -> testRight
                    STOP -> stop
                | otherwise = game
                where
                    (tx,ty) = (gTarget (pinky game))
                    (vx,vy) = (gSpeed (pinky game))
                    (x,y) = (gLocation (pinky game))
                    x' = fromIntegral (round x)
                    y' = fromIntegral (round y)
                    yU = y' + 1
                    yD = y' + (-1)
                    xL = x' + (-1)
                    xR = x' + 1
                    lastCR = lcrP game
                    curTarget = (gTarget (pinky game))
                    lm = (gLastMove (pinky game))
                    cD = (gDirection (pinky game))
                    curTime = time game

                    -- Determines where the walls are for a specific ghost location
                    pWalls = findWalls game 2


                    p1 = if lm /= UP && (distance (x',yU) (tx,ty)) < (distance (x',yD) (tx,ty)) && (distance (x',yU) (tx,ty)) < (distance (xR,y') (tx,ty)) && (distance (x',yU) (tx,ty)) < (distance (xL,y') (tx,ty))
                        then goUp
                    else if lm /= DOWN && (distance (x',yD) (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (x',yD) (tx,ty)) < (distance (xR,y') (tx,ty)) && (distance (x',yD) (tx,ty)) < (distance (xL,y') (tx,ty))
                        then goDown
                    else if lm /= RIGHT && (distance (xR,y') (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (xR,y') (tx,ty)) < (distance (xL,y') (tx,ty)) && (distance (xR,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                        then goRight
                    else if lm /= LEFT && (distance (xL,y') (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (xL,y') (tx,ty)) < (distance (xR,y') (tx,ty)) && (distance (xL,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                        then goLeft
                    else if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords) then goDown
                    else game

                    p2 = if lm /= UP && (distance (x',yU) (tx,ty)) <= (distance (xR,y') (tx,ty)) && (distance (x',yU) (tx,ty)) <= (distance (xL,y') (tx,ty))
                        then goUp
                    else if lm /= RIGHT && (distance (xR,y') (tx,ty)) <= (distance (x',yU) (tx,ty)) && (distance (xR,y') (tx,ty)) <= (distance (xL,y') (tx,ty))
                        then goRight
                    else if lm /= LEFT && (distance (xL,y') (tx,ty)) <= (distance (x',yU) (tx,ty)) && (distance (xL,y') (tx,ty)) <= (distance (xR,y') (tx,ty))
                        then goLeft
                    else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords) then goDown
                    else game

                    p3 = if lm /= UP && (distance (x',yU) (tx,ty)) < (distance (x',yD) (tx,ty)) && (distance (x',yU) (tx,ty)) < (distance (xR,y') (tx,ty))
                        then goUp
                    else if lm /= DOWN && (distance (x',yD) (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (x',yD) (tx,ty)) < (distance (xR,y') (tx,ty))
                        then goDown
                    else if lm /= RIGHT && (distance (xR,y') (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (xR,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                        then goRight
                    else if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords) then goDown
                    else game

                    p4 = if lm /= DOWN && (distance (x',yD) (tx,ty)) < (distance (xR,y') (tx,ty)) && (distance (x',yD) (tx,ty)) < (distance (xL,y') (tx,ty))
                        then goDown
                    else if lm /= RIGHT && (distance (xR,y') (tx,ty)) < (distance (xL,y') (tx,ty)) && (distance (xR,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                        then goRight
                    else if lm /= LEFT && (distance (xL,y') (tx,ty)) < (distance (xR,y') (tx,ty)) && (distance (xL,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                        then goLeft
                    else if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords) then goDown
                    else game

                    p5 = if lm /= UP && (distance (x',yU) (tx,ty)) < (distance (x',yD) (tx,ty)) && (distance (x',yU) (tx,ty)) < (distance (xL,y') (tx,ty))
                        then goUp
                    else if lm /= DOWN && (distance (x',yD) (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (x',yD) (tx,ty)) < (distance (xL,y') (tx,ty))
                        then goDown
                    else if lm /= LEFT && (distance (xL,y') (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (xL,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                        then goLeft
                    else if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords) then goDown
                    else game

                    p10 = if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords)
                        then goDown
                    else game 


                    -- Movement
                    goRight = game {pinky = Ghost {gName = Pinky, gSpeed = (3.7,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = LEFT, gDirection = RIGHT}}
                    goLeft = game {pinky = Ghost {gName = Pinky, gSpeed = (-3.7,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = RIGHT, gDirection = LEFT}}
                    goUp = game {pinky = Ghost {gName = Pinky, gSpeed = (0,3.7), gLocation = (x',y'), gTarget = curTarget, gLastMove = DOWN, gDirection = UP}}
                    goDown = game {pinky = Ghost {gName = Pinky, gSpeed = (0,-3.7), gLocation = (x',y'), gTarget = curTarget, gLastMove = UP, gDirection = DOWN}}

                    -- When velocity isn't at 0, check collision of wall.
                    testUp = if (elem (x',yU) coords) then game {pinky = Ghost {gName = Pinky, gSpeed = (0,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = lm, gDirection = cD}} else game
                    testDown = if (elem (x',yD) coords) then game {pinky = Ghost {gName = Pinky, gSpeed = (0,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = lm, gDirection = cD}} else game
                    testLeft = if (elem (xL,y') coords) then game {pinky = Ghost {gName = Pinky, gSpeed = (0,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = lm, gDirection = cD}} else game
                    testRight = if (elem (xR,y') coords) then game {pinky = Ghost {gName = Pinky, gSpeed = (0,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = lm, gDirection = cD}} else game
                    stop = game {pinky = Ghost {gName = Pinky, gSpeed = (0,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = lm, gDirection = cD}}

    -------------------- Inky ------------------

    -- | Determines Inky's target based on the location && direction of Pacman and the location of Blinky
    targetInky :: PacGame -> Point
    targetInky game = case pacDirection of
                UP -> up
                DOWN -> down
                LEFT -> left
                RIGHT -> right
                STOP -> up
                where
                    (bx,by) = (gLocation (blinky game))
                    bx' = fromIntegral (round bx)
                    by' = fromIntegral (round by)
                    pacDirection = direction game
                    (px,py) = (location (pacman game))
                    px' = fromIntegral (round px)
                    py' = fromIntegral (round py)
                    
                    -- Calculating Ink'y target using pacman position and direction.
                    -- Original game is +2, but since I use a smaller board I just do +1
                    up = (((px'-bx')+px'),(((py'+1)-by')+py'))
                    down = (((px'-bx')+px'),(((py'-1)-by')+py'))
                    left = ((((px'-1)-bx')+px'),((py'-by')+py'))
                    right = ((((px'+1)-bx')+px'),((py'-by')+py'))

    -- | Moves Inky to the target
    moveToTargetI :: PacGame -> PacGame
    moveToTargetI game
                | curTime <= 4 = game
                | dotsLeft <= 10 = game
                | (elem (x',y') crossR) && (lastCR /= (x',y')) = game {lcrI = (x',y'), inky = Ghost {gName = Inky, gSpeed = (0,0), gLocation = (x,y), gTarget = (curTarget), gLastMove = lm, gDirection = cD}}
                | (vx,vy) == (0,0) = case iWalls of
                    1 -> p1
                    2 -> p2
                    3 -> p3
                    4 -> p4
                    5 -> p5
                    10 -> p10
                | (vx,vy) /= (0,0) = case (gDirection (inky game)) of
                    UP -> testUp
                    DOWN -> testDown
                    LEFT -> testLeft
                    RIGHT -> testRight
                    STOP -> stop
                | otherwise = game
                where
                    curPellets = pellets game
                    dotsLeft = (listLength pelletsL) - (listLength curPellets)
                    (tx,ty) = (gTarget (inky game))
                    (vx,vy) = (gSpeed (inky game))
                    (x,y) = (gLocation (inky game))
                    x' = fromIntegral (round x)
                    y' = fromIntegral (round y)
                    yU = y' + 1
                    yD = y' + (-1)
                    xL = x' + (-1)
                    xR = x' + 1
                    lastCR = lcrI game
                    curTarget = (gTarget (inky game))
                    lm = (gLastMove (inky game))
                    cD = (gDirection (inky game))
                    curTime = time game

                    -- Determines where the walls are for a specific ghost location
                    iWalls = findWalls game 3

                    p1 = if lm /= UP && (distance (x',yU) (tx,ty)) < (distance (x',yD) (tx,ty)) && (distance (x',yU) (tx,ty)) < (distance (xR,y') (tx,ty)) && (distance (x',yU) (tx,ty)) < (distance (xL,y') (tx,ty))
                        then goUp
                    else if lm /= DOWN && (distance (x',yD) (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (x',yD) (tx,ty)) < (distance (xR,y') (tx,ty)) && (distance (x',yD) (tx,ty)) < (distance (xL,y') (tx,ty))
                        then goDown
                    else if lm /= RIGHT && (distance (xR,y') (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (xR,y') (tx,ty)) < (distance (xL,y') (tx,ty)) && (distance (xR,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                        then goRight
                    else if lm /= LEFT && (distance (xL,y') (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (xL,y') (tx,ty)) < (distance (xR,y') (tx,ty)) && (distance (xL,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                        then goLeft
                    else if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords) then goDown
                    else game

                    p2 = if lm /= UP && (distance (x',yU) (tx,ty)) <= (distance (xR,y') (tx,ty)) && (distance (x',yU) (tx,ty)) <= (distance (xL,y') (tx,ty))
                        then goUp
                    else if lm /= RIGHT && (distance (xR,y') (tx,ty)) <= (distance (x',yU) (tx,ty)) && (distance (xR,y') (tx,ty)) <= (distance (xL,y') (tx,ty))
                        then goRight
                    else if lm /= LEFT && (distance (xL,y') (tx,ty)) <= (distance (x',yU) (tx,ty)) && (distance (xL,y') (tx,ty)) <= (distance (xR,y') (tx,ty))
                        then goLeft
                    else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords) then goDown
                    else game

                    p3 = if lm /= UP && (distance (x',yU) (tx,ty)) < (distance (x',yD) (tx,ty)) && (distance (x',yU) (tx,ty)) < (distance (xR,y') (tx,ty))
                        then goUp
                    else if lm /= DOWN && (distance (x',yD) (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (x',yD) (tx,ty)) < (distance (xR,y') (tx,ty))
                        then goDown
                    else if lm /= RIGHT && (distance (xR,y') (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (xR,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                        then goRight
                    else if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords) then goDown
                    else game

                    p4 = if lm /= DOWN && (distance (x',yD) (tx,ty)) < (distance (xR,y') (tx,ty)) && (distance (x',yD) (tx,ty)) < (distance (xL,y') (tx,ty))
                        then goDown
                    else if lm /= RIGHT && (distance (xR,y') (tx,ty)) < (distance (xL,y') (tx,ty)) && (distance (xR,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                        then goRight
                    else if lm /= LEFT && (distance (xL,y') (tx,ty)) < (distance (xR,y') (tx,ty)) && (distance (xL,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                        then goLeft
                    else if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords) then goDown
                    else game

                    p5 = if lm /= UP && (distance (x',yU) (tx,ty)) < (distance (x',yD) (tx,ty)) && (distance (x',yU) (tx,ty)) < (distance (xL,y') (tx,ty))
                        then goUp
                    else if lm /= DOWN && (distance (x',yD) (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (x',yD) (tx,ty)) < (distance (xL,y') (tx,ty))
                        then goDown
                    else if lm /= LEFT && (distance (xL,y') (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (xL,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                        then goLeft
                    else if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords) then goDown
                    else game

                    p10 = if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords)
                        then goDown
                    else game 


                    -- Movement
                    goRight = game {inky = Ghost {gName = Inky, gSpeed = (3.7,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = LEFT, gDirection = RIGHT}}
                    goLeft = game {inky = Ghost {gName = Inky, gSpeed = (-3.7,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = RIGHT, gDirection = LEFT}}
                    goUp = game {inky = Ghost {gName = Inky, gSpeed = (0,3.7), gLocation = (x',y'), gTarget = curTarget, gLastMove = DOWN, gDirection = UP}}
                    goDown = game {inky = Ghost {gName = Inky, gSpeed = (0,-3.7), gLocation = (x',y'), gTarget = curTarget, gLastMove = UP, gDirection = DOWN}}

                    -- When velocity isn't at 0, check collision of wall.
                    testUp = if (elem (x',yU) coords) then game {inky = Ghost {gName = Inky, gSpeed = (0,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = lm, gDirection = cD}} else game
                    testDown = if (elem (x',yD) coords) then game {inky = Ghost {gName = Inky, gSpeed = (0,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = lm, gDirection = cD}} else game
                    testLeft = if (elem (xL,y') coords) then game {inky = Ghost {gName = Inky, gSpeed = (0,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = lm, gDirection = cD}} else game
                    testRight = if (elem (xR,y') coords) then game {inky = Ghost {gName = Inky, gSpeed = (0,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = lm, gDirection = cD}} else game
                    stop = game {inky = Ghost {gName = Inky, gSpeed = (0,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = lm, gDirection = cD}}

-------------------- Clyde ------------------ 


    -- | Computes the proper target of clyde. If Clyde gets within a 4 block radius, sets target to clydescattertarget
    targetClyde :: PacGame -> Point
    targetClyde game
                | (px' - cx' <= 4) || (py' - cy' <= 4) = clydeScatterTarget
                | otherwise = (px',py')
                where
                    (px,py) = (location (pacman game))
                    px' = fromIntegral (round px)
                    py' = fromIntegral (round py)
                    (cx,cy) = (gLocation (clyde game))
                    cx' = fromIntegral (round cx)
                    cy' = fromIntegral (round cy)     


    -- | Moves Clyde to the target. Doesn't come out until at least 50 pellets are eaten! (roughly 1/3 total)
    moveToTargetC :: PacGame -> PacGame
    moveToTargetC game
                | curTime <= 3 = game
                | dotsLeft <= 50 = game
                | (elem (x',y') crossR) && (lastCR /= (x',y')) = game {lcrC = (x',y'), clyde = Ghost {gName = Clyde, gSpeed = (0,0), gLocation = (x,y), gTarget = curTarget, gLastMove = lm, gDirection = cD}}
                | (vx,vy) == (0,0) = case bWalls of
                    1 -> b1
                    2 -> b2
                    3 -> b3
                    4 -> b4
                    5 -> b5
                    10 -> b10
                | (vx,vy) /= (0,0) = case (gDirection (clyde game)) of
                    UP -> testUp
                    DOWN -> testDown
                    LEFT -> testLeft
                    RIGHT -> testRight
                    STOP -> stop
                | otherwise = game
                where
                    curPellets = pellets game
                    dotsLeft = (listLength pelletsL) - (listLength curPellets)
                    (tx,ty) = (gTarget (clyde game))
                    (vx,vy) = (gSpeed (clyde game))
                    (x,y) = (gLocation (clyde game))
                    x' = fromIntegral (round x)
                    y' = fromIntegral (round y)
                    yU = y' + 1
                    yD = y' + (-1)
                    xL = x' + (-1)
                    xR = x' + 1
                    lastCR = lcrC game
                    curTarget = (gTarget (clyde game))
                    lm = (gLastMove (clyde game))
                    cD = (gDirection (clyde game))
                    curTime = time game

                    b1 = if lm /= UP && (distance (x',yU) (tx,ty)) < (distance (x',yD) (tx,ty)) && (distance (x',yU) (tx,ty)) < (distance (xR,y') (tx,ty)) && (distance (x',yU) (tx,ty)) < (distance (xL,y') (tx,ty))
                        then goUp
                    else if lm /= DOWN && (distance (x',yD) (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (x',yD) (tx,ty)) < (distance (xR,y') (tx,ty)) && (distance (x',yD) (tx,ty)) < (distance (xL,y') (tx,ty))
                        then goDown
                    else if lm /= RIGHT && (distance (xR,y') (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (xR,y') (tx,ty)) < (distance (xL,y') (tx,ty)) && (distance (xR,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                        then goRight
                    else if lm /= LEFT && (distance (xL,y') (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (xL,y') (tx,ty)) < (distance (xR,y') (tx,ty)) && (distance (xL,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                        then goLeft
                    else if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords) then goDown
                    else game

                    b2 = if lm /= UP && (distance (x',yU) (tx,ty)) <= (distance (xR,y') (tx,ty)) && (distance (x',yU) (tx,ty)) <= (distance (xL,y') (tx,ty))
                        then goUp
                    else if lm /= RIGHT && (distance (xR,y') (tx,ty)) <= (distance (x',yU) (tx,ty)) && (distance (xR,y') (tx,ty)) <= (distance (xL,y') (tx,ty))
                        then goRight
                    else if lm /= LEFT && (distance (xL,y') (tx,ty)) <= (distance (x',yU) (tx,ty)) && (distance (xL,y') (tx,ty)) <= (distance (xR,y') (tx,ty))
                        then goLeft
                    else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords) then goDown
                    else game

                    b3 = if lm /= UP && (distance (x',yU) (tx,ty)) < (distance (x',yD) (tx,ty)) && (distance (x',yU) (tx,ty)) < (distance (xR,y') (tx,ty))
                        then goUp
                    else if lm /= DOWN && (distance (x',yD) (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (x',yD) (tx,ty)) < (distance (xR,y') (tx,ty))
                        then goDown
                    else if lm /= RIGHT && (distance (xR,y') (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (xR,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                        then goRight
                    else if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords) then goDown
                    else game

                    b4 = if lm /= DOWN && (distance (x',yD) (tx,ty)) < (distance (xR,y') (tx,ty)) && (distance (x',yD) (tx,ty)) < (distance (xL,y') (tx,ty))
                        then goDown
                    else if lm /= RIGHT && (distance (xR,y') (tx,ty)) < (distance (xL,y') (tx,ty)) && (distance (xR,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                        then goRight
                    else if lm /= LEFT && (distance (xL,y') (tx,ty)) < (distance (xR,y') (tx,ty)) && (distance (xL,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                        then goLeft
                    else if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords) then goDown
                    else game

                    b5 = if lm /= UP && (distance (x',yU) (tx,ty)) < (distance (x',yD) (tx,ty)) && (distance (x',yU) (tx,ty)) < (distance (xL,y') (tx,ty))
                        then goUp
                    else if lm /= DOWN && (distance (x',yD) (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (x',yD) (tx,ty)) < (distance (xL,y') (tx,ty))
                        then goDown
                    else if lm /= LEFT && (distance (xL,y') (tx,ty)) < (distance (x',yU) (tx,ty)) && (distance (xL,y') (tx,ty)) < (distance (x',yD) (tx,ty))
                        then goLeft
                    else if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords) then goDown
                    else game

                    b10 = if lm /= RIGHT && (checkInList (xR, y') coords) then goRight else if lm /= UP && (checkInList (x', yU) coords) then goUp else if lm /= LEFT && (checkInList (xL, y') coords) then goLeft else if lm /= DOWN && (checkInList (x', yD) coords)
                        then goDown
                    else game 

                    -- Determines where the walls are for a specific ghost location
                    bWalls = findWalls game 4

                    -- Movement
                    goRight = game {clyde = Ghost {gName = Clyde, gSpeed = (3.7,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = LEFT, gDirection = RIGHT}}
                    goLeft = game {clyde = Ghost {gName = Clyde, gSpeed = (-3.7,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = RIGHT, gDirection = LEFT}}
                    goUp = game {clyde = Ghost {gName = Clyde, gSpeed = (0,3.7), gLocation = (x',y'), gTarget = curTarget, gLastMove = DOWN, gDirection = UP}}
                    goDown = game {clyde = Ghost {gName = Clyde, gSpeed = (0,-3.7), gLocation = (x',y'), gTarget = curTarget, gLastMove = UP, gDirection = DOWN}}

                    -- When velocity isn't at 0, check collision of wall.
                    testUp = if (elem (x',yU) coords) then game {clyde = Ghost {gName = Clyde, gSpeed = (0,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = lm, gDirection = cD}} else game
                    testDown = if (elem (x',yD) coords) then game {clyde = Ghost {gName = Clyde, gSpeed = (0,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = lm, gDirection = cD}} else game
                    testLeft = if (elem (xL,y') coords) then game {clyde = Ghost {gName = Clyde, gSpeed = (0,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = lm, gDirection = cD}} else game
                    testRight = if (elem (xR,y') coords) then game {clyde = Ghost {gName = Clyde, gSpeed = (0,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = lm, gDirection = cD}} else game
                    stop = game {clyde = Ghost {gName = Clyde, gSpeed = (0,0), gLocation = (x',y'), gTarget = curTarget, gLastMove = lm, gDirection = cD}}




    -- Represents where walls are in relative to position
    -- 1 = all 4 sides
    -- 2 = Left, up, right
    -- 3 = up, right, down
    -- 4 = right, down, left
    -- 5 = down, left, up
    -- 6 = up, right
    -- 7 = right, down
    -- 8 = down, left
    -- 9 = left up
    -- | Used for chase AI, Checks current position with walls, returning a number which represents how and where the walls are.
    findWalls :: PacGame -> Int -> Int
    findWalls game x = case x of 
                    1 -> checkBlinky
                    2 -> checkPinky
                    3 -> checkInky
                    4 -> checkClyde
                    where
                        --- Blinky ---
                        (bx,by) = (gLocation (blinky game))
                        bx' = fromIntegral (round bx)
                        by' = fromIntegral (round by)
                        bU = by' + 1
                        bD = by' + (-1)
                        bL = bx' + (-1)
                        bR = bx' + (1)
                        checkBlinky =
                            if (checkInList (bL, by') coords) && (checkInList (bR, by') coords) && (checkInList (bx', bU) coords) && (checkInList (bx', bD) coords) then 1
                            else if (checkInList (bL, by') coords) && (checkInList (bR, by') coords) && (checkInList (bx', bU) coords) && (elem (bx', bD) coords) then 2
                            else if (elem (bL, by') coords) && (checkInList (bR, by') coords) && (checkInList (bx', bU) coords) && (checkInList (bx', bD) coords) then 3
                            else if (checkInList (bL, by') coords) && (checkInList (bR, by') coords) && (elem (bx', bU) coords) && (checkInList (bx', bD) coords) then 4
                            else if (checkInList (bL, by') coords) && (elem (bR, by') coords) && (checkInList (bx', bU) coords) && (checkInList (bx', bD) coords) then 5
                            else 10
                            
                        --- Pinky ---
                        (px,py) = (gLocation (pinky game))
                        px' = fromIntegral (round px)
                        py' = fromIntegral (round py)
                        pU = py' + 1
                        pD = py' + (-1)
                        pL = px' + (-1)
                        pR = px' + (1)
                        checkPinky = 
                            if (checkInList (pL, py') coords) && (checkInList (pR, py') coords) && (checkInList (px', pU) coords) && (checkInList (px', pD) coords) then 1
                            else if (checkInList (pL, py') coords) && (checkInList (pR, py') coords) && (checkInList (px', pU) coords) && (elem (px', pD) coords) then 2
                            else if (elem (pL, py') coords) && (checkInList (pR, py') coords) && (checkInList (px', pU) coords) && (checkInList (px', pD) coords) then 3
                            else if (checkInList (pL, py') coords) && (checkInList (pR, py') coords) && (elem (px', pU) coords) && (checkInList (px', pD) coords) then 4
                            else if (checkInList (pL, py') coords) && (elem (pR, py') coords) && (checkInList (px', pU) coords) && (checkInList (px', pD) coords) then 5
                            else 10

                        --- Inky ---
                        (ix,iy) = (gLocation (inky game))
                        ix' = fromIntegral (round ix)
                        iy' = fromIntegral (round iy)
                        iU = iy' + 1
                        iD = iy' + (-1)
                        iL = ix' + (-1)
                        iR = ix' + (1)
                        checkInky = 
                            if (checkInList (iL, iy') coords) && (checkInList (iR, iy') coords) && (checkInList (ix', iU) coords) && (checkInList (ix', iD) coords) then 1
                            else if (checkInList (iL, iy') coords) && (checkInList (iR, iy') coords) && (checkInList (ix', iU) coords) && (elem (ix', iD) coords) then 2
                            else if (elem (iL, iy') coords) && (checkInList (iR, iy') coords) && (checkInList (ix', iU) coords) && (checkInList (ix', iD) coords) then 3
                            else if (checkInList (iL, iy') coords) && (checkInList (iR, iy') coords) && (elem (ix', iU) coords) && (checkInList (ix', iD) coords) then 4
                            else if (checkInList (iL, iy') coords) && (elem (iR, iy') coords) && (checkInList (ix', iU) coords) && (checkInList (ix', iD) coords) then 5
                            else 10

                        --- Clyde ---
                        (cx,cy) = (gLocation (clyde game))
                        cx' = fromIntegral (round cx)
                        cy' = fromIntegral (round cy)
                        cU = cy' + 1
                        cD = cy' + (-1)
                        cL = cx' + (-1)
                        cR = cx' + (1)
                        checkClyde = 
                            if (checkInList (cL, cy') coords) && (checkInList (cR, cy') coords) && (checkInList (cx', cU) coords) && (checkInList (cx', cD) coords) then 1
                            else if (checkInList (cL, cy') coords) && (checkInList (cR, cy') coords) && (checkInList (cx', cU) coords) && (elem (cx', cD) coords) then 2
                            else if (elem (cL, cy') coords) && (checkInList (cR, cy') coords) && (checkInList (cx', cU) coords) && (checkInList (cx', cD) coords) then 3
                            else if (checkInList (cL, cy') coords) && (checkInList (cR, cy') coords) && (elem (cx', cU) coords) && (checkInList (cx', cD) coords) then 4
                            else if (checkInList (cL, cy') coords) && (elem (cR, cy') coords) && (checkInList (cx', cU) coords) && (checkInList (cx', cD) coords) then 5
                            else 10
