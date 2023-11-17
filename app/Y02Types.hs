module Y02Types where

-- one cell - just a char
type Value = Char -- can be Int, just to print them easier
-- one row = array of `a`
type Row a = [a]
-- matrix = 2d array of `a`
type Matrix a = [Row a]
-- grid = 2d array of Value
type Grid = Matrix Value
-- choices for one cell = array of values
type Choices = [Value]
-- Matrix Choices = 
-- matrix where each cell contains list of all possible comibations
