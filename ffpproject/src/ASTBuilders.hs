{-# LANGUAGE TemplateHaskell #-}
module ASTBuilders where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

-- | generate a data type declaration with the given String type name, String constructor name prefix and Int number (>=1) of numbered constructors without type variables. An additional 'Bool' argument decides if another constructor with suffix "_None" is added after the numbered ones. The generated type will automatically derive instances of the types in the given list. Usage examples:
--
-- @genNumberedDataType "MyNumData" "MND" 3 False [''Eq]@ will create: @data MyNumData = MND1 | MND2 | MND3 deriving Eq@
-- 
-- @genNumberedDataType "MyNumData" "MND" 2 True [''Show, ''Eq]@ will create: @data MyNumData = MND1 | MND2 | MND_None deriving (Show, Eq)@
genNumberedDataType :: String -> String -> Int -> Bool -> [Name] -> Q [Dec]
genNumberedDataType tName cPrefix num addNone deriveList =
    if num<1
    then fail ("genNumberedDataType: given number of constructors has to be >=1 but was given as "++show num)
    else do
        -- create the type name: use the given name as-is
        let dName = mkName tName
        -- create the numbered constructor names with the given name as prefix followed by the number (from 1 to the given number)
        let cNamesNumbered = fmap (mkName . (cPrefix ++) . show) [1..num]
        -- if True is passed for addNone, add another constructor with the suffix "_None" after the given name
        let cNames = if addNone then cNamesNumbered ++ [mkName $ cPrefix ++ "_None"] else cNamesNumbered
        -- build the constructors as NormalC Cons without BangTypes
        let cs = fmap (\name -> NormalC name []) cNames
        -- build type list from the given derives
        let derives = fmap ConT deriveList
        -- build deriveClause(s) (no DerivStrategy, would have to be explicitly turned on)
        let deriveClauses = [DerivClause Nothing derives]
        -- build and return the final DataDs
        return [DataD [] dName [] Nothing cs deriveClauses]

-- CSV format as 2D list
newtype CSV = CSV [[String]]

-- | custom Show instance of CSV, essentially just gets rid of the constructor that is needed for newtype (which in turn is needed to define a Lift instance later) and uses built-in Show instances for the String lists.
instance Show CSV where
    show (CSV csv2d) = show csv2d

-- | read a CSV file as String and return a list of lists (CSV rows / lines) of strings (CSV values)
fromCSV :: String -> CSV
fromCSV s = CSV $ fmap splitByComma (lines $ normalize s)

-- | remove all carriage returns, tabs and duplicate newlines from a string, as well as spaces following or preceding newlines and commas
normalize :: String -> String
normalize s = norm s "" "" True False where
    -- if the end of the string is reached, return the filtered result as-is and remove any trailing newlines.
    norm []        res _   newline _     = if newline && not (null res)
        then init res
        else res
    -- discard all Windows carriage returns and tabs.
    norm ('\r':cs) res cur newline comma = norm cs res cur newline comma
    norm ('\t':cs) res cur newline comma = norm cs res cur newline comma
    -- if we read a newline, append it to the result only if there wasn't already one before it and discard any stored spaces.
    norm ('\n':cs) res cur newline comma = if newline
        then norm cs res         "" True False
        else norm cs (res++"\n") "" True False
    -- if we read a comma, append it to the result and discard any stored spaces
    norm (',':cs)  res cur newline comma = norm cs (res++",") "" False True
    -- if we read a space, add it to the stored spaces only if it's not at the start of the string, after a newline or after a comma
    norm (' ':cs)  res cur newline comma = if null res || newline || comma
        then norm cs res cur       newline comma
        else norm cs res (' ':cur) newline comma
    -- if we read any other character, append it and any stored spaces to the result and clear the stored spaces
    norm (c:cs)    res cur newline comma = norm cs (res++cur++[c]) "" False False

-- | split a given String into a list of Strings by breaking it up with each comma
splitByComma :: String -> [String]
splitByComma s = sbc s [] "" where
    sbc :: String -> [String] -> String -> [String]
    sbc []       res ""  = res
    sbc []       []  akk = [akk]
    sbc []       res akk = res++[akk]
    sbc (',':cs) res akk = sbc cs (res++[akk]) ""
    sbc (c:cs)   res akk = sbc cs res (akk++[c])

-- | read a 2D list and return a CSV-style string
toCSV :: CSV -> String
toCSV (CSV sss) = concatLines sss where
    concatLines ls = concatWith '\n' $ fmap concatValues ls
    concatValues vs = concatWith ',' vs

-- | concat a 2D list with a given delimiter to a 1D list
concatWith :: a -> [[a]] -> [a]
concatWith delim ls = if null ls then [] else tail $ concatMap (\x -> delim:x) ls

-- | custom "csv" quasiquoter, only makes sense for expressions
csv :: QuasiQuoter
csv = QuasiQuoter {
    quoteExp = compileCSV,
    quotePat = err "Patterns",
    quoteType = err "Types",
    quoteDec = err "Declarations"
    }
    where err s = error $ s ++ "are not compatible with CSV format!"

-- | extracted compiler function to ExpQ
compileCSV :: String -> Q Exp
compileCSV = lift . fromCSV

-- | custom Lift instance to use lift, delegates lifting of the regular string lists to built-in functions and only explicitly handles the CSV constructor
instance Lift CSV where
  lift (CSV csv2d) = appE (conE 'CSV) (lift csv2d)
  liftTyped = unsafeTExpCoerce . lift
