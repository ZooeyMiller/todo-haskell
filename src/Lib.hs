module Lib where

import           Text.Read (readMaybe)

type Todo = (Int, (String, Bool))

type Todos = [Todo]

indexed :: [a] -> [(Int, a)]
indexed xs = go xs [] 1
    where go [] ys _     = ys
          go (x:xs) ys i = go xs (ys ++ [(i, x)]) (i + 1)

getTodoInfo :: Todos -> [(String, Bool)]
getTodoInfo = map snd

reIndex :: Todos -> Todos
reIndex = indexed . getTodoInfo

addTodo :: Todos -> String -> Todos
addTodo ts t = indexed $ getTodoInfo ts ++ [(t, False)]

removeTodo :: Todos -> Int -> Todos
removeTodo ts i = reIndex $ filter ((/=i) . fst) ts

toggleTodo :: Todos -> Int -> Todos
toggleTodo ts i = map update ts
    where update td@(j, (t, b))
            | j == i = (j, (t, not b))
            | otherwise = td

runAdd :: Todos -> IO Todos
runAdd ts = do
    putStrLn "please enter your todo:"
    todo <- getLine
    return $ addTodo ts todo

runIndexFn :: (Todos -> Int -> Todos) -> Todos -> IO Todos
runIndexFn f ts = do
    charI <- getLine
    let i = (readMaybe charI :: Maybe Int)
    case i of
        (Just i) -> return $ f ts i
        Nothing -> do putStrLn "please enter an integer as the index"
                      return ts

runToggle :: Todos -> IO Todos
runToggle ts = do
    putStrLn "please enter the index of the todo you want to toggle:"
    runIndexFn toggleTodo ts

runDelete :: Todos -> IO Todos
runDelete ts = do
    putStrLn "please enter the index of the todo you want to delete"
    runIndexFn removeTodo ts
