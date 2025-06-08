{- 
--- Day 23: Opening the Turing Lock ---

Little Jane Marie just got her very first computer for Christmas from some unknown benefactor. It comes with instructions and an example program, but the computer itself seems to be malfunctioning. She's curious what the program does, and would like you to help her run it.

The manual explains that the computer supports two registers and six instructions (truly, it goes on to remind the reader, a state-of-the-art technology). The registers are named a and b, can hold any non-negative integer, and begin with a value of 0. The instructions are as follows:

    hlf r sets register r to half its current value, then continues with the next instruction.
    tpl r sets register r to triple its current value, then continues with the next instruction.
    inc r increments register r, adding 1 to it, then continues with the next instruction.
    jmp offset is a jump; it continues with the instruction offset away relative to itself.
    jie r, offset is like jmp, but only jumps if register r is even ("jump if even").
    jio r, offset is like jmp, but only jumps if register r is 1 ("jump if one", not odd).

All three jump instructions work with an offset relative to that instruction. The offset is always written with a prefix + or - to indicate the direction of the jump (forward or backward, respectively). For example, jmp +1 would simply continue with the next instruction, while jmp +0 would continuously jump back to itself forever.

The program exits when it tries to run an instruction beyond the ones defined.

For example, this program sets a to 2, because the jio instruction causes it to skip the tpl instruction:

inc a
jio a, +2
tpl a
inc a

What is the value in register b when the program in your puzzle input is finished executing?

-}
import Data.List.Extra ( (!?) )
import Debug.Trace ( trace )
import System.IO ( hFlush, stderr )

eval :: (String, String) -> Int -> Int -> Int -> (Int, Int, Int)
eval (op, arg) regA regB pc =
    let r = head arg in
    case op of  
    "hlf" -> case r of
            'a' -> (regA `div` 2, regB, pc+1)
            'b' -> (regA, regB `div` 2, pc+1)  
    "tpl" -> case r of
            'a' -> (regA * 3, regB, pc+1)
            'b' -> (regA, regB * 3, pc+1)  
    "inc" -> case r of
            'a' -> (regA + 1, regB, pc+1)
            'b' -> (regA, regB + 1, pc+1) 
    "jmp" -> (regA, regB, pc + strToInt arg)
    "jie" -> let cond = if r == 'a' then even regA else even regB in
             if cond then (regA, regB, pc + strToInt (drop 3 arg))
                     else (regA, regB, pc + 1)
    "jio" -> let cond = if r == 'a' then regA==1 else regB==1 in
             if cond then (regA, regB, pc + strToInt (drop 3 arg))
                     else (regA, regB, pc + 1)
    _     -> (999, 999, 999) -- fail

strToInt :: [Char] -> Int
strToInt xs = let x = read (tail xs) :: Int in
    case head xs of
        '-' -> -x
        _   -> x

runP :: [(String, String)] -> Int -> Int -> Int -> (Int, Int, Int)
runP program regA regB pc = 
    case program !? pc of
        Just x -> let (newA, newB, newPC) = eval x regA regB pc in
                    -- trace ("PC = "++ show pc) $ 
                    runP program newA newB newPC
        _      -> -- trace "EOF" 
                    (regA, regB, pc)

main :: IO ()
main = do
    program <- fmap (\x -> (take 3 x, drop 4 x)) 
            . lines 
            <$> readFile "./2015/day23/day.txt"

    let result@(regA, regB, pc) = runP program 0 0 0
    _ <- result `seq` hFlush stderr   -- flush trace output 
    putStrLn "Answer part 1:"
    putStrLn $ "Reg A = " ++ show regA 
           ++ " Reg B = " ++ show regB
           ++ " PC = "    ++ show pc

    let result@(regA, regB, pc) = runP program 1 0 0
    _ <- result `seq` hFlush stderr   -- flush trace output 
    putStrLn "----------"
    putStrLn "Answer part 2:"
    putStrLn $ "Reg A = " ++ show regA 
           ++ " Reg B = " ++ show regB
           ++ " PC = "    ++ show pc
