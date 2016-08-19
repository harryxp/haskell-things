ASSUMPTION latch is horizontal

This solves a puzzle in the iPhone game Broken Sword.

Note that to make Eq and Ord work, hBlocks and vBlocks inside each Lock must be sorted, so are the coords inside each Block.




First let's define our data structures... and how to pretty print it.

> import Data.List (delete, sort)
> import Data.Set (Set)
> import qualified Data.Set as Set

> data Block = Block {
>   coords :: [(Int, Int)]
> } deriving (Eq, Ord, Show)

> data Lock = Lock {
>   size         :: Int,          -- 6x6 in the game Broken Sword
>   unlockCoords :: [(Int, Int)], -- move latch to these coordinates to unlock
>   latch        :: Block,
>   hBlocks      :: [Block],      -- keep vertical and horizontal blocks separate
>   vBlocks      :: [Block]
> } deriving (Eq, Ord)

Utility functions to collect all cells occupied by horizontal/vertical blocks.

> hCellCoords hBlocks = concat (map coords hBlocks)
> vCellCoords vBlocks = concat (map coords vBlocks)

LockTransformation is simply a list of Locks.  It keeps our moves - each of the
two adjacent Locks can be reached from the other with one move.

> type LockTransformation = [Lock]

Define how a Lock looks like...

> instance Show Lock where
>   show l@(Lock _ _ _ _ _) = "\n" ++ showRows l

> showRows :: Lock -> String
> showRows (Lock size unlockCoords latch hBlocks vBlocks) = iter 0
>   where
>     iter row | row == size = ""
>     iter row | otherwise      = showRow row ++ iter (row+1)
>     showRow row = concat [ showCell row col | col <- [0..size-1] ] ++ "\n"
>     showCell :: Int -> Int -> String
>     showCell x y | any ((x, y) ==) hCellCoords'   = "-"
>                  | any ((x, y) ==) vCellCoords'   = "|"
>                  | any ((x, y) ==) (coords latch) = "*"
>                  | any ((x, y) ==) unlockCoords   = "x"
>                  | otherwise                      = " "
>     hCellCoords' = hCellCoords hBlocks
>     vCellCoords' = vCellCoords vBlocks




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
> newLocks l@(Lock size unlockCoords latch hBlocks vBlocks) = byMovingHBlocks
>                                                         ++ byMovingVBlocks
>                                                         ++ byMovingLatch
>   where

>     byMovingHBlocks :: [Lock]
>     byMovingHBlocks = concat (map byMovingOneHBlock hBlocks)

      Use moveHBlock to obtain new positions of a block, pass that to buildNewLockH to get new locks.

>     byMovingOneHBlock :: Block -> [Lock]
>     byMovingOneHBlock b = map (buildNewLockH b) (moveHBlock b)

>     buildNewLockH :: Block -> Block -> Lock
>     buildNewLockH oldBlock newBlock = Lock size unlockCoords latch (sort (newBlock : (delete oldBlock hBlocks))) vBlocks

>     moveHBlock :: Block -> [Block]
>     moveHBlock hb = let hbCoords = coords hb
>                         leftest = minimum hbCoords
>                         rightest = maximum hbCoords
>                         cellOnTheLeft = (fst leftest, (snd leftest) - 1)
>                         cellOnTheRight = (fst rightest, (snd rightest) + 1)
>                     in
>                       (if isCellEmpty cellOnTheLeft then
>                         -- hbCoords is in ascending order, and we maintain it
>                         [ Block (cellOnTheLeft : (take (length hbCoords - 1) hbCoords)) ]
>                       else
>                         [])
>                       ++
>                       (if isCellEmpty cellOnTheRight then
>                         -- hbCoords is in ascending order, and we maintain it
>                         [ Block ((tail hbCoords) ++ [cellOnTheRight]) ]
>                       else
>                         [])
>

>     byMovingVBlocks :: [Lock]
>     byMovingVBlocks = concat (map byMovingOneVBlock vBlocks)

>     byMovingOneVBlock :: Block -> [Lock]
>     byMovingOneVBlock b = map (buildNewLockV b) (moveVBlock b)

>     buildNewLockV :: Block -> Block -> Lock
>     buildNewLockV oldBlock newBlock = Lock size unlockCoords latch hBlocks (sort (newBlock : (delete oldBlock vBlocks)))

>     moveVBlock :: Block -> [Block]
>     moveVBlock vb = let vbCoords = coords vb
>                         top = minimum vbCoords
>                         bottom = maximum vbCoords
>                         cellAbove = (fst top - 1, snd top)
>                         cellBelow = (fst bottom + 1, snd bottom)
>                     in
>                       (if isCellEmpty cellAbove then
>                         -- vbCoords is in ascending order, and we maintain it
>                         [ Block (cellAbove : (take (length vbCoords - 1) vbCoords)) ]
>                       else
>                         [])
>                       ++
>                       (if isCellEmpty cellBelow then
>                         -- hbCoords is in ascending order, and we maintain it
>                         [ Block ((tail vbCoords) ++ [cellBelow]) ]
>                       else
>                         [])

>     byMovingLatch :: [Lock]
>     byMovingLatch = map (\newLatch -> Lock size unlockCoords newLatch hBlocks vBlocks) (moveHBlock latch)  -- ASSUMPTION latch is horizontal

>     isCellEmpty (x, y) = all ((x, y) /=) allOccupiedCells && x /= -1 && x /= size && y /= -1 && y /= size
>     allOccupiedCells = hCellCoords hBlocks ++ vCellCoords vBlocks ++ coords latch




> lock0 = Lock 3
>              [(2,2)]
>              (Block [(2,0)])
>              [Block [(0,0)], Block [(0,1)]]
>              [Block [(0,2)], Block [(1,1),(2,1)]]

> lock1 = Lock 6
>              [(2,1),(2,2)]
>              (Block [(2,4),(2,5)])
>              [
>                 Block [(0,3),(0,4),(0,5)],
>                 Block [(1,0),(1,1)],
>                 Block [(3,1),(3,2)],
>                 Block [(4,1),(4,2),(4,3)],
>                 Block [(5,2),(5,3)],
>                 Block [(5,4),(5,5)]
>              ]
>              [
>                 Block [(0,2),(1,2)],
>                 Block [(1,3),(2,3),(3,3)],
>                 Block [(3,0),(4,0),(5,0)],
>                 Block [(3,4),(4,4)],
>                 Block [(3,5),(4,5)]
>              ]

> lock2 = Lock 6
>              [(2,4),(2,5)]
>              (Block [(2,0),(2,1)])
>              [
>                 Block [(0,3),(0,4),(0,5)],
>                 Block [(3,1),(3,2),(3,3)],
>                 Block [(5,4),(5,5)]
>              ]
>              [
>                 Block [(0,0),(1,0)],
>                 Block [(0,1),(1,1)],
>                 Block [(1,5),(2,5)],
>                 Block [(2,4),(3,4),(4,4)],
>                 Block [(3,0),(4,0)],
>                 Block [(3,5),(4,5)],
>                 Block [(4,2),(5,2)],
>                 Block [(4,3),(5,3)]
>              ]




> limit = 2000
> main = do
>   print lock0
>   print $ solve lock0

