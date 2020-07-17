{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
module Main where

import Control.Monad
import Data.List
import Data.Random
import Data.Random.Distribution.Bernoulli
import Diagrams.Prelude hiding (sample)
import Diagrams.Backend.SVG.CmdLine

data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance Distribution Uniform Digit where
    rvarT (Uniform a b) = toEnum <$> uniformT (fromEnum a) (fromEnum b)

instance Distribution StdUniform Digit where
    rvarT ~StdUniform = uniformT minBound maxBound

boxColor D0 = black
boxColor D1 = brown
boxColor D2 = red
boxColor D3 = orange
boxColor D4 = yellow
boxColor D5 = green
boxColor D6 = blue
boxColor D7 = violet
boxColor D8 = grey
boxColor D9 = white

textColor _ False = black
textColor D0 _ = white
textColor D1 _ = white
textColor D2 _ = white
textColor D3 _ = black
textColor D4 _ = black
textColor D5 _ = white
textColor D6 _ = white
textColor D7 _ = white
textColor D8 _ = black
textColor D9 _ = black

digitString :: Digit -> String
digitString = show . fromEnum

digitText :: Maybe Digit -> Bool -> Diagram B
digitText Nothing  _ = mempty
digitText (Just d) b =  text (digitString d)
    # translateY (-0.1)
    # fc (textColor d b)

digitRect :: Maybe Digit -> Bool -> Diagram B
digitRect Nothing  True  = roundedRect 1 1 0.1
digitRect Nothing  False = roundedRect 1 1 0.1
    # lc grey
    # dashingN [0.015,0.015] 0
digitRect (Just d) b = digitRect Nothing b # fc (boxColor d)

digit :: Maybe Digit -> Maybe Digit -> Diagram B
digit d1 d2  = digitText d1 (d2 /= Nothing) `atop` digitRect d2 (d1 /= Nothing || d2 /= Nothing)

problemCell' :: Maybe Digit -> Maybe Digit -> Diagram B
problemCell' d1 d2 =
    digit Nothing d2
    ===
    digit d1 Nothing

data ProblemCell
    = EncodingProblemCell Digit
    | DecodingProblemCell Digit
    | CheckProblemCell Digit Digit

drawProblemCell (EncodingProblemCell d) =
    digit Nothing Nothing
    ===
    digit (Just d) Nothing

drawProblemCell (DecodingProblemCell d) =
    digit Nothing (Just d)
    ===
    digit Nothing Nothing

drawProblemCell (CheckProblemCell d1 d2) =
    digit Nothing (Just d2)
    ===
    digit (Just d1) Nothing

newEncodingProblemCell = EncodingProblemCell <$> stdUniform
newDecodingProblemCell = DecodingProblemCell <$> stdUniform
newCheckProblemCell pCorrect = do
    d1 <- stdUniform
    correct <- bernoulli (pCorrect :: Float)
    if correct
        then return (CheckProblemCell d1 d1)
        else do
            d2 <- randomElement ([D0 .. D9] \\ [d1])
            return (CheckProblemCell d1 d2)

newProblemCell pCheckCorrect =
    join $ randomElement
        [ newEncodingProblemCell
        , newDecodingProblemCell
        , newCheckProblemCell pCheckCorrect
        ]

type Problem = [ProblemCell]
drawProblem = bgFrame 0.1 white . hcat . map drawProblemCell

newProblem :: Int -> RVar ProblemCell -> RVar Problem
newProblem = replicateM

newDecodingProblem n = newProblem n newDecodingProblemCell
newEncodingProblem n = newProblem n newEncodingProblemCell
newCheckProblem n pCheckCorrect = newProblem n (newCheckProblemCell pCheckCorrect)
newMixedProblem n pCheckCorrect = newProblem n (newProblemCell pCheckCorrect)

type ProblemSet = [[Problem]]
drawProblemSet = vcat . map (hcat . map drawProblem)

newProblemSet :: Int -> Int -> RVar Problem -> RVar ProblemSet
newProblemSet rows cols = replicateM rows . replicateM cols

drawLegend = drawProblem [CheckProblemCell d d | d <- [minBound .. maxBound]]

withLegend x = vcat [center $ bgFrame 0.3 white drawLegend, center x]

diagram :: IO (Diagram B)
diagram = do
    problems <- sample (newProblemSet 6 4 (newMixedProblem 3 0.8))
    return (withLegend (drawProblemSet problems))

main = mainWith diagram
