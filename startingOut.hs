doubleMe x = x * 2

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                      then x
                      else doubleMe x

doubleSmallNumber' x = (if x > 100 then x else doubleMe x)

conanO'Brien = "It's a-me, Conan O'Brien"

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x ]

length' xs = sum [ 1 | _ <- xs ]

removeNonUppercase st = [ l | l <- st, l `elem` ['A'..'Z'] ]
