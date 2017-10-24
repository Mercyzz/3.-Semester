type Player = Bool


data Game s m = Game {
        startState  :: s,
        showGame    :: s -> String,
        move        :: Player -> s -> m -> (Player, s),
        moves       :: Player -> s -> [m],
        value       :: Player -> s -> Double
    }

data AddGame = AddGame Int Int

startStateImpl :: AddGame -> Int
startStateImpl _ = undefined

moveImpl :: AddGame -> Player -> Int -> Int -> (Player, Int)
moveImpl (AddGame n k) p s m = undefined

movesImpl :: AddGame -> Player -> Int -> [Int]
movesImpl (AddGame n k) p s = undefined

valueImpl :: AddGame -> Player -> Int -> Double
valueImpl (AddGame n k) p s = undefined
                            

addGame :: AddGame -> Game Int Int
addGame g = Game {
    startState = startStateImpl g,
    showGame   = show,
    move       = moveImpl g,
    moves      = movesImpl g,
    value      = valueImpl g
  }
  


leftmostGame :: Game s m -> Player -> s -> [s]
leftmostGame g p s = undefined

rightmostGame :: Game s m -> Player -> s -> [s]
rightmostGame g p s = undefined




data Game1D = Game1D Int

data Direction = L | R deriving Show
type Position = Int
data Move1D = Computer Position Int | Human Direction deriving Show


game1d :: Game1D -> Game [Int] Int

game1d g = Game {
    startState = startStateImpl' g,
    showGame   = showGameImpl' g,
    move       = moveImpl' g,
    moves      = movesImpl' g,
    value      = valueImpl' g
  }
  

startStateImpl' :: Game1D -> [Int]
startStateImpl' (Game1D n) = undefined

showGameImpl' :: Game1D -> [Int] -> String
showGameImpl' (Game1D n) = undefined

moveImpl' :: Game1D -> Player -> [Int] -> Int -> (Player, [Int])
moveImpl' (Game1D n) p s m = undefined

movesImpl' :: Game1D -> Player -> [Int] -> [Int]
movesImpl' (Game1D n) p s = undefined

valueImpl' :: Game1D -> Player -> [Int] -> Double
valueImpl' (Game1D n) p s = undefined