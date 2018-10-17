module Lib where

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
