module Y01 where

inc x = x + 1
twice x = x * 2

-- `twice` after `inc`
-- f2 10 -> 22
-- `$` means brackets ()
f2 x = twice $ inc x

-- `twice` after `inc`
-- f3 10 -> 22
-- `.` means function composition
f3 = twice . inc
