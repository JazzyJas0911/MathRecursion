-- Write the recursive Haskell functions, mult(mult'), power(power'), and sub(sub'),
--      using ONLY logical expression, assignment, succ, pred, and if-else (or guard) expressions.


add :: Int -> Int -> Int
add x y =
    if y == 0 then x
    else if y > 0 then add (succ x) (pred y)
    else add (pred x) (succ y)

add' :: Int -> Int -> Int
add' x y
    | y == 0 = x
    | y > 0 = add' (succ x) (pred y)
    | otherwise = add' (pred x) (succ y)


mult :: Int -> Int -> Int
mult x y =
    if y == 0 then 0
    else if y > 0 then add x (mult x (pred y))
    else -(mult x (-y))

mult' :: Int -> Int -> Int
mult' x y
    | y == 0 = 0
    | y > 0 = add' x (mult x (pred y))
    | otherwise = -(mult x (-y))


power :: Int -> Int -> Int
power x y =
    if y == 0 then 1
    else mult x (power x (pred y))

power' :: Int -> Int -> Int
power' x y
    | y == 0 = 1
    | otherwise = mult' x (power' x (pred y))


sub :: Int -> Int -> Int
sub x y =
    if y == 0 then x
    else if y > 0 then sub (pred x) (pred y)
    else sub (succ x) (succ y)

sub' :: Int -> Int -> Int
sub' x y
    | y == 0 = x
    | y > 0 = sub' (pred x) (pred y)
    | otherwise = sub' (succ x) (succ y)


main = do
    print (add 2 3)
    print (add' 3 2)
    print (add (-2) (-3))
    print (add' (-2) (-3))

    print (mult 2 3)
    print (mult' 2 3)
    print (mult (-2) (-2))
    print (mult' (2) (-3))

    print (power 2 3)
    print (power' 2 3)

    print (sub 2 3)
    print (sub' 2 3)
    print (sub (-2) (-3))
    print (sub' (-2) (-3))
