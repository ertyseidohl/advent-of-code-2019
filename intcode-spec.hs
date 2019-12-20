import Test.Hspec
import IntCode (runMachine, HaltMode (RUNALL, PIPEMODE))

main :: IO ()
main = hspec $ do
    let runMachineThrough = runMachine RUNALL
    describe "Day 2" $ do
        it "calculates using opcodes 1 and 2" $
            runMachineThrough [1,9,10,3,2,3,11,0,99,30,40,50] [] `shouldBe` (3500 ,[])
        it "overwrites middle values" $
            runMachineThrough [1,1,1,4,99,5,6,0,99] [] `shouldBe` (30, [])
    describe "Day 5" $ do
        it "Echos input to output" $
            runMachineThrough [3,0,4,0,99] [44] `shouldBe` (44, [44])
        it "Handles immediate mode" $
            runMachineThrough [1002,4,3,4,33] [] `shouldBe` (1002, [])
        it "Handles negative immediate numbers" $
            runMachineThrough [1101,100,-1,4,0] [] `shouldBe` (1101, [])
        it "Runs all diagnostic tests" $
            runMachineThrough diagnostic [1] `shouldBe` (3,[5821753,0,0,0,0,0,0,0,0,0])
        it "Using position mode, consider whether the input is equal to 8" $
            runMachineThrough [3,9,8,9,10,9,4,9,99,-1,8] [8] `shouldBe` (3, [1])
        it "Using position mode, consider whether the input is not equal to 8" $
            runMachineThrough [3,9,8,9,10,9,4,9,99,-1,8] [9] `shouldBe` (3, [0])
        it "Using position mode, consider whether the input is less than 8" $
            runMachineThrough [3,9,7,9,10,9,4,9,99,-1,8] [7] `shouldBe` (3, [1])
        it "Using position mode, consider whether the input is not less than 8" $
            runMachineThrough [3,9,7,9,10,9,4,9,99,-1,8] [8] `shouldBe` (3, [0])
        it "Using immediate mode, consider whether the input is equal to 8" $
            runMachineThrough [3,3,1108,-1,8,3,4,3,99] [8] `shouldBe` (3, [1])
        it "Using immediate mode, consider whether the input is not equal to 8" $
            runMachineThrough [3,3,1108,-1,8,3,4,3,99] [9] `shouldBe` (3, [0])
        it "Using immediate mode, consider whether the input is less than 8" $
            runMachineThrough [3,3,1107,-1,8,3,4,3,99] [7] `shouldBe` (3, [1])
        it "Using immediate mode, consider whether the input is not less than 8" $
            runMachineThrough [3,3,1107,-1,8,3,4,3,99] [8] `shouldBe` (3, [0])
        it "Should jump properly using position mode, if the input is 0 output 0" $
            runMachineThrough [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] [0] `shouldBe` (3, [0])
        it "Should jump properly using position mode, if the input is nonzero output 1" $
            runMachineThrough [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] [2] `shouldBe` (3, [1])
        it "Should output 999 if the input is below 8" $
            runMachineThrough greaterEqualLess [4] `shouldBe` (3, [999])
        it "Should output 1000 if the input is equal to 8" $
            runMachineThrough greaterEqualLess [8] `shouldBe` (3, [1000])
        it "Should output 1001 if the input is greater than 8" $
            runMachineThrough greaterEqualLess [10] `shouldBe` (3, [1001])


diagnostic :: [Int]
diagnostic = [3,225,1,225,6,6,1100,1,238,225,104,0,1101,32,43,225,101,68,192,224,1001,224,-160,224,4,224,102,8,223,223,1001,224,2,224,1,223,224,223,1001,118,77,224,1001,224,-87,224,4,224,102,8,223,223,1001,224,6,224,1,223,224,223,1102,5,19,225,1102,74,50,224,101,-3700,224,224,4,224,1002,223,8,223,1001,224,1,224,1,223,224,223,1102,89,18,225,1002,14,72,224,1001,224,-3096,224,4,224,102,8,223,223,101,5,224,224,1,223,224,223,1101,34,53,225,1102,54,10,225,1,113,61,224,101,-39,224,224,4,224,102,8,223,223,101,2,224,224,1,223,224,223,1101,31,61,224,101,-92,224,224,4,224,102,8,223,223,1001,224,4,224,1,223,224,223,1102,75,18,225,102,48,87,224,101,-4272,224,224,4,224,102,8,223,223,1001,224,7,224,1,224,223,223,1101,23,92,225,2,165,218,224,101,-3675,224,224,4,224,1002,223,8,223,101,1,224,224,1,223,224,223,1102,8,49,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1107,226,226,224,1002,223,2,223,1005,224,329,1001,223,1,223,1007,677,226,224,1002,223,2,223,1006,224,344,1001,223,1,223,108,677,226,224,102,2,223,223,1006,224,359,1001,223,1,223,7,226,226,224,1002,223,2,223,1005,224,374,101,1,223,223,107,677,677,224,1002,223,2,223,1006,224,389,1001,223,1,223,1007,677,677,224,1002,223,2,223,1006,224,404,1001,223,1,223,1107,677,226,224,1002,223,2,223,1005,224,419,1001,223,1,223,108,226,226,224,102,2,223,223,1006,224,434,1001,223,1,223,1108,226,677,224,1002,223,2,223,1006,224,449,1001,223,1,223,1108,677,226,224,102,2,223,223,1005,224,464,1001,223,1,223,107,226,226,224,102,2,223,223,1006,224,479,1001,223,1,223,1008,226,226,224,102,2,223,223,1005,224,494,101,1,223,223,7,677,226,224,1002,223,2,223,1005,224,509,101,1,223,223,8,226,677,224,1002,223,2,223,1006,224,524,1001,223,1,223,1007,226,226,224,1002,223,2,223,1006,224,539,101,1,223,223,1008,677,677,224,1002,223,2,223,1006,224,554,101,1,223,223,1108,677,677,224,102,2,223,223,1006,224,569,101,1,223,223,1107,226,677,224,102,2,223,223,1005,224,584,1001,223,1,223,8,677,226,224,1002,223,2,223,1006,224,599,101,1,223,223,1008,677,226,224,102,2,223,223,1006,224,614,1001,223,1,223,7,226,677,224,1002,223,2,223,1005,224,629,101,1,223,223,107,226,677,224,102,2,223,223,1005,224,644,101,1,223,223,8,677,677,224,102,2,223,223,1005,224,659,1001,223,1,223,108,677,677,224,1002,223,2,223,1005,224,674,101,1,223,223,4,223,99,226]

greaterEqualLess :: [Int]
greaterEqualLess = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]