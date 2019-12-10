module ClassState
where
 

import Data.Map (Map)
import qualified Data.Map as Map

 -- data Map k a
-- Utilizat pentru a obține informații despre Variabile sau Funcții
data InstrType = Var | Func  deriving (Show, Eq)

-- TODO - Trebuie definit ClassState

data ClassState = Clasa (Map Int [[String]]) --ClassState este un Map cu cheia numar intreg si valoarea o lista de liste de String
initEmptyClass :: ClassState

initEmptyClass = Clasa Map.empty

insertIntoClass :: ClassState -> InstrType -> [String] -> ClassState --cheia = 1 pentru Var si cheia = 0 pentru Func
insertIntoClass (Clasa m) tip input 
                    | tip == Var = Clasa (Map.insertWith (++) (1) ([input]) m)
                    | otherwise = Clasa  (Map.insertWith (++) (0) ([input]) m)

getValues :: ClassState -> InstrType -> [[String]]
getValues (Clasa m) tip                  
                    | tip == Var = Map.findWithDefault [] 1 (m)
                    | otherwise =  Map.findWithDefault [] 0 (m)
                    

