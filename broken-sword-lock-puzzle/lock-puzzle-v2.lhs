This solves a puzzle in the iPhone game Broken Sword.  This is version 2.

Note that to make Eq and Ord work, blocks inside each Lock must be sorted, so are the coords inside each Block.




Define the data and how we print it.

> import Data.List (delete, sort)
> import Data.Set (Set)
> import qualified Data.Set as Set

> data Block = Block {
>   coords      :: [(Int, Int)],
>   orientation :: Char
> } deriving (Eq, Ord, Show)

> data Lock = Lock {
>   size         :: Int,          -- 6x6 in the game Broken Sword
>   unlockCoords :: [(Int, Int)], -- move latch to these coordinates to unlock
>   latch        :: Block,
>   blocks       :: [Block]
> } deriving (Eq, Ord)

> instance Show Lock where
>   show l@(Lock _ _ _ _) = "\n" ++ showRows l

> showRows :: Lock -> String
> showRows (Lock size unlockCoords latch blocks) = iter 0
>   where
>     iter row | row == size = ""
>     iter row | otherwise      = showRow row ++ iter (row+1)
>     showRow row = concat [ showCell row col | col <- [0..size-1] ] ++ "\n"
>     showCell :: Int -> Int -> String
>     showCell x y | any ((x, y) ==) hCellCoords    = "-"
>                  | any ((x, y) ==) vCellCoords    = "|"
>                  | any ((x, y) ==) (coords latch) = "*"
>                  | any ((x, y) ==) unlockCoords   = "x"
>                  | otherwise                      = " "
>     hCellCoords = cellCoords 'h'
>     vCellCoords = cellCoords 'v'
>     cellCoords o = concat [ coords b | b <- blocks, orientation b == o ]

LockTransformation is simply a list of Locks.  It keeps our moves - each of the
two adjacent Locks can be reached from the other with one move.

> type LockTransformation = [Lock]




Algorithms...

> solve lock = transformLocks [[lock]] (Set.singleton lock) 1

> transformLocks :: [LockTransformation] -> Set Lock -> Int -> ([LockTransformation], Set Lock, Bool)
> transformLocks [] cache _ = ([], cache, False)
> transformLocks (tf:tfs) cache cnt = if cnt == limit then error "We've reached the limit!"
>                                     else
>                                       let currLock = last tf
>                                           nextLocks = filter ((flip Set.notMember) cache) (newLocks currLock)
>                                           unlocked = filter (\lck -> unlockCoords currLock == (coords . latch) lck) nextLocks
>                                           nextCache = (foldl (flip Set.insert) cache nextLocks)
>                                           nextTrans = [ tf ++ [lck] | lck <- nextLocks ]
>                                       in

                                          We've found a solution...

>                                         if (not . null) unlocked then
>                                           ([ tf ++ [lck] | lck <- unlocked ], nextCache, True)
>                                         else

                                          Haven't found a solution.  So we continue to try this branch and update the cache for other branches to use later.

>                                           let r@(_, updatedCache, win) = transformLocks nextTrans nextCache (cnt+1) in
>                                             if win then r else transformLocks tfs updatedCache (cnt+1)

Given a lock, this figures out all the possible next steps by moving one block only one cell away.
Sure this will include some previous patterns so we need to filter those out in its caller.

> newLocks :: Lock -> [Lock]
> newLocks l@(Lock size unlockCoords latch blocks) = concat (map byMovingOneBlock blocks) ++ byMovingLatch
>   where

      Use nextBlocks to obtain new positions of a block, pass that to buildNewLock to get new locks.

>     byMovingOneBlock :: Block -> [Lock]
>     byMovingOneBlock b = map (buildNewLock b) (nextBlocks b)

>     buildNewLock :: Block -> Block -> Lock
>     buildNewLock oldBlock newBlock = Lock size unlockCoords latch (sort (newBlock : (delete oldBlock blocks)))

>     nextBlocks :: Block -> [Block]
>     nextBlocks b = let blockCoords = coords b
>                        minCoord = minimum blockCoords
>                        maxCoord = maximum blockCoords
>                        o = orientation b
>                        cellNextToMin = case o of
>                                          'h' -> (fst minCoord, (snd minCoord) - 1)
>                                          'v' -> ((fst minCoord) - 1, snd minCoord)
>                                          _   -> error "Invalid orientation."
>                        cellNextToMax = case o of
>                                         'h' -> (fst maxCoord, (snd maxCoord) + 1)
>                                         'v' -> ((fst maxCoord) + 1, snd maxCoord)
>                                         _   -> error "Invalid orientation."
>                      in
>                        (if isCellEmpty cellNextToMin then
>                          -- blockCoords is in ascending order, and we maintain it
>                          [ Block (cellNextToMin : (take (length blockCoords - 1) blockCoords)) o ]
>                        else
>                          [])
>                        ++
>                        (if isCellEmpty cellNextToMax then
>                          -- blockCoords is in ascending order, and we maintain it
>                          [ Block ((tail blockCoords) ++ [cellNextToMax]) o ]
>                        else
>                          [])
>

>     byMovingLatch = map (\newLatch -> Lock size unlockCoords newLatch blocks) (nextBlocks latch)

>     isCellEmpty (x, y) = all ((x, y) /=) allOccupiedCells && x /= -1 && x /= size && y /= -1 && y /= size
>     allOccupiedCells = concat (map coords blocks ) ++ coords latch




> lock0 = Lock 3
>              [(2,2)]
>              (Block [(2,0)] 'h')
>              (sort [Block [(0,0)] 'h', Block [(0,1)] 'h', Block [(0,2)] 'v', Block [(1,1),(2,1)] 'v'])

> lock1 = Lock 6
>              [(2,1),(2,2)]
>              (Block [(2,4),(2,5)] 'h')
>              (sort [
>                 Block [(0,3),(0,4),(0,5)] 'h',
>                 Block [(1,0),(1,1)]       'h',
>                 Block [(3,1),(3,2)]       'h',
>                 Block [(4,1),(4,2),(4,3)] 'h',
>                 Block [(5,2),(5,3)]       'h',
>                 Block [(5,4),(5,5)]       'h',
>                 Block [(0,2),(1,2)]       'v',
>                 Block [(1,3),(2,3),(3,3)] 'v',
>                 Block [(3,0),(4,0),(5,0)] 'v',
>                 Block [(3,4),(4,4)]       'v',
>                 Block [(3,5),(4,5)]       'v'
>              ])

> lock2 = Lock 6
>              [(2,4),(2,5)]
>              (Block [(2,0),(2,1)] 'h')
>              (sort [
>                 Block [(0,3),(0,4),(0,5)] 'h',
>                 Block [(3,1),(3,2),(3,3)] 'h',
>                 Block [(5,4),(5,5)]       'h',
>                 Block [(0,0),(1,0)]       'v',
>                 Block [(0,1),(1,1)]       'v',
>                 Block [(1,5),(2,5)]       'v',
>                 Block [(2,4),(3,4),(4,4)] 'v',
>                 Block [(3,0),(4,0)]       'v',
>                 Block [(3,5),(4,5)]       'v',
>                 Block [(4,2),(5,2)]       'v',
>                 Block [(4,3),(5,3)]       'v'
>              ])




> limit = 2000
> main = do
>   print lock2
>   print $ solve lock2

