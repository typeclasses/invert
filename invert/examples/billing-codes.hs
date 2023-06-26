import Invert
import Prelude
import System.Exit (die)

data Product = Basic | Standard | Pro
  deriving stock (Generic, Show, Eq)
  deriving anyclass (GEnum)

data Frequency = Monthly | Annual
  deriving stock (Generic, Show, Eq)
  deriving anyclass (GEnum)

data Bill = Bill Product Frequency
  deriving stock (Generic, Show, Eq)
  deriving anyclass (GEnum)

encodeProduct :: Product -> String
encodeProduct x = case x of
  Basic -> "p1"
  Standard -> "p2"
  Pro -> "p3"

encodeBill :: Bill -> Integer
encodeBill x = case x of
  Bill Basic Monthly -> 10
  Bill Basic Annual -> 11
  Bill Standard Monthly -> 20
  Bill Standard Annual -> 21
  Bill Pro Monthly -> 30
  Bill Pro Annual -> 31

decodeProduct :: String -> Maybe Product
decodeProduct = Invert.injection hashTable genum encodeProduct

decodeBill :: Integer -> Maybe Bill
decodeBill = Invert.injection hashTable genum encodeBill

main :: IO ()
main = do
  encodeProduct Basic === "p1"
  encodeProduct Standard === "p2"

  decodeProduct "p1" === Just Basic
  decodeProduct "xyz" === Nothing

  encodeBill (Bill Basic Annual) === 11
  encodeBill (Bill Pro Monthly) === 30

  decodeBill 31 === Just (Bill Pro Annual)
  decodeBill 50 === Nothing

(===) :: (Eq a, Show a) => a -> a -> IO ()
a === b
  | a == b = pure ()
  | otherwise = die (show a <> " /= " <> show b)
