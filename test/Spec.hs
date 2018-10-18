import           Lib
import           Test.QuickCheck

todosIndexIdentity :: [(String, Bool)] -> Bool
todosIndexIdentity ts = (getTodoInfo $  indexed ts) == ts

main :: IO ()
main = do
    quickCheck todosIndexIdentity