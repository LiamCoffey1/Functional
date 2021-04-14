----------------------------------------------------------------------------------
-------------------------------------- HW 3 --------------------------------------
----------------------------------------------------------------------------------
--------------------------------------- Q1 ---------------------------------------
----------------------------------------------------------------------------------

-- All cuts: divide an array into sub arrays in all possible combinations
allCuts :: [a] -> [[[a]]]
allCuts [] = []
allCuts list = cutEach (allBin ((length list) - 1)) list

-- Generate an array of all binary strings of given length
-- Each bit represents a cut between a pair after the index of bit
allBin :: (Eq a, Num a) => a -> [[Char]]
allBin 0 = [[]]
allBin n = map ('0':) (allBin (n-1)) ++ map ('1':) (allBin(n-1))

-- Loop through binary strings and append cuts
cutEach :: [[Char]] -> [a] -> [[[a]]]
cutEach seq list
    | null seq = []
    | otherwise = [cutFromBinary 1 list (head seq)] ++ cutEach (tail seq) list  

-- Predicate True if current bit is 1 (char)
canCut binary i = (binary !! (i)) == '1'

-- Cuts a list according to bits of binary string
cutFromBinary :: Int -> [a] -> [Char] -> [[a]]
cutFromBinary i list binary
    -- Base case, return rest of list and stop
    | (null list || null binary || i == (length list)) = [take (i) list]
    -- if bit = 1 store i (dist. of bit to start) eles, reset counter and repeat on rest
    | canCut binary (i-1) = [take (i) list] ++ cutFromBinary 1 (drop (i) list) (drop (i) binary) 
    -- if bit = 0 store nothing, increment counter and continue
    | otherwise = cutFromBinary (i+1) list binary

---------------------------------------------------------------------------------
--------------------------------------- Q2 --------------------------------------
---------------------------------------------------------------------------------

data Tree = LV String | Node Tree Tree | Const String
   deriving (Show, Eq)
type Unifier = [(String,Tree)]
type Unity = (String,Tree)
unify :: Tree -> Tree -> Unifier -> Maybe Unifier
unify t1 t2 acc
    | t1 == t2 = Just []
    | otherwise = do
        let results = toList t1 t2 []
        -- Hacky error handling mechanism
        if lookup "e" results /= Nothing then Nothing else Just results
        
unJust (Just something) = something

-- Simultaneous inorder traversel of two binary trees
-- Compare on discovery
toList (Node l1 r1) (Node l2 r2) acc = toList l1 l2 (acc ++ toList l1 l2 acc)
    ++ toList r1 r2 (acc ++ toList r1 r2 acc)                                
toList x y acc = do
    let comparison = comparet x y
    -- Needs back verification
    if comparison /= Nothing then [unJust comparison] else []


-- Pattern matching for Unifications    
comparet :: Tree -> Tree -> Maybe Unity
comparet (Const c1) (Const c2) = if c1 == c2 then Nothing else Just ("e", Const "e")
comparet (Node l r) (LV lv) = Just (lv, Node l r)
comparet (LV lv) (Node l r) = Just (lv, Node l r)
comparet (LV l1) (LV l2) = if l1 /= l2 then Just (l1, LV l2) else Nothing
comparet (LV l1) (Const c1) = Just (l1, Const c1)
comparet (Const c1) (LV l1) = Just (l1, Const c1)
comparet _ _ = Nothing

---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
