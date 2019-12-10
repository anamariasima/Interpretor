module Parser
where

import Util
import Data.Maybe
import Data.String
import InferenceDataType
import Data.Map (Map)
import qualified Data.Map as Map
import ClassState
-- Definire Program

data Program = P(Map (String) (String, ClassState)) -- Program este un Map cu cheia String si valoarea un tuplu de String si ClassState - cheia o sa fie numele clasei, 
                                                    --  iar valoarea o sa contina numele parintelui si ClassState-ul corespunzator

initEmptyProgram :: Program                         
initEmptyProgram = P (Map.insert "Global" ("Global", initEmptyClass) Map.empty)

insertIntoProgram :: Program -> String->String->ClassState-> Program -- functie ce introduce o noua intrare in Program
insertIntoProgram (P program) nume nume_parinte clasa = P (Map.insert (nume) (nume_parinte, clasa) program)

getVars :: Program -> [[String]]
getVars (P program) = getValues (snd (Map.findWithDefault ("Global", initEmptyClass) "Global" (program) )) Var

getClasses :: Program -> [String]
getClasses (P program) = Map.keys program

getParentClass :: String -> Program -> String
getParentClass nume (P program) =  fst( (Map.findWithDefault ("Global", initEmptyClass) nume (program)) )

getFuncsForClass :: String -> Program -> [[String]]
getFuncsForClass nume (P program) = getValues (snd ( (Map.findWithDefault ("Global", initEmptyClass) nume (program)) )) Func

replace :: String -> String -- functie ce inlocuieste aparitiile caracterelor de care nu e nevoie cu spatiu
replace [] = []
replace s 
        | head(s) == ',' = " " ++ replace(tail (s))
        | head(s) == ')' = " " ++ replace(tail (s))
        | head(s) == '(' = " " ++ replace(tail (s))
        | head(s) == ':' = " " ++ replace(tail (s))
        | head(s) == '=' = " " ++ replace(tail (s))
        | otherwise = head(s):replace(tail(s))

findClass :: String-> Program -> Bool -- functie ce returneaza True daca exista o anumita clasa in Program si False in caz contrar
findClass nume (P program) 
                            | null( filter (==nume) (getClasses (P program)) ) == True = False
                            | otherwise = True



-- Instruction poate fi ce considerați voi
type Instruction = [String] -- Insttruction este o lista de String

parse :: String -> [Instruction]
parse p = filter (/= []) (map words (map replace (lines(p))) )


interpret :: Instruction -> Program -> Program
interpret  instructiune (P program)
                -- clasa fara parinte specificat
                | head(instructiune) == "class" && null(drop 2(instructiune)) = 
                    insertIntoProgram (P program) (head(tail(instructiune))) ("Global") (initEmptyClass)
                -- clasa cu parinte specificat de tip valid
                | head(instructiune) == "class" && (last(take 3 (instructiune)) == "extends") && findClass(last(instructiune)) (P program) =
                    insertIntoProgram (P program) (head(tail(instructiune)) ) (last(instructiune)) (initEmptyClass)
                -- clasa cu parinte specificat de un tip invalid
                | head(instructiune) == "class" && (last(take 3 (instructiune)) == "extends") =
                    insertIntoProgram (P program) (head(tail(instructiune)) ) ("Global") (initEmptyClass)
                
                -- variabila de tip valid
                | head(instructiune) == "newvar" && 
                    findClass (last(instructiune)) (P program)  = 
                    insertIntoProgram 
                    (P program)
                    ("Global")
                    ("Global")
                    (insertIntoClass
                    (snd (Map.findWithDefault ("Global", initEmptyClass) "Global" (program) )) 
                    (Var)
                    ([last(take 2 (instructiune)), last(instructiune)]) )
                
                -- functie
                | findClass(head(instructiune))(P program) && findClass(head(tail(instructiune)))(P program)  =
                    insertIntoProgram 
                    (P program)
                    (instructiune !! 1)
                    (fst (Map.findWithDefault ("Global", initEmptyClass) (instructiune !! 1) (program) ))
                    (insertIntoClass
                    (snd (Map.findWithDefault ("Global", initEmptyClass) (instructiune !! 1) (program) )) 
                    (Func)
                    ([instructiune !! 2,instructiune !! 0] ++ (drop 3 instructiune) ) )
                
                | otherwise = P program

infer :: Expr -> Program -> Maybe String
infer = undefined

