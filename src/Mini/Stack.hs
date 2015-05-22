module Mini.Stack where

newtype Stack a = Stack [a]

{- stack funcs-}
push :: Stack a -> a -> Stack a
push (Stack stack) item = Stack $ item:stack

pop :: Stack a -> (a, Stack a)
pop (Stack []) = error "empty stack"
pop (Stack (x:xs)) = (x, Stack xs)

emptyStack :: Stack a -> Bool
emptyStack (Stack []) = True
emptyStack (Stack _) = False
