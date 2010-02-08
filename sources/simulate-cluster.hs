-- @+leo-ver=4-thin
-- @+node:gcross.20100130190931.1265:@thin simulate-cluster.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100130190931.1266:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
-- @-node:gcross.20100130190931.1266:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20100130190931.1267:<< Import needed modules >>
import Acme.Dont

import Control.Applicative.Infix
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans

import Data.Complex
import Data.ConfigFile
import Data.IORef
import Data.Time.Clock

import System.Environment
import System.Posix.Clock
import System.Exit

import Text.Printf

import VMPS.Algorithms
import VMPS.EnergyMinimizationChain
import VMPS.Models
import VMPS.Operators
import VMPS.Paulis
import VMPS.States
import VMPS.Tensors

import Debug.Trace
-- @-node:gcross.20100130190931.1267:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100130190931.1274:Operator tensors
makeModelOperatorSiteTensors :: Double -> Int -> Int -> [OperatorSiteTensor]
makeModelOperatorSiteTensors factor width height =
    let horizontal_scale = (1-factor) :+ 0
        vertical_scale = factor :+ 0

        horizontal_index = 3
        horizontal_range = [horizontal_index,horizontal_index+1]

        vertical_start = last horizontal_range + 1
        vertical_range = [vertical_start..vertical_start + 2*width - 1]

        bandwidth = maximum
            [last horizontal_range
            ,last vertical_range
            ]

        passRange = concat . map pass

        pass index =
            [(index_ --> index_) pI]
          where
            index_ = fromIntegral index

        throw index =
            [(1 --> fromIntegral index) pZ]
        fly scale index1 index2 =
            [(fromIntegral index1 --> fromIntegral index2) (scale *: pX)]
        catch index =
            [(fromIntegral index --> 2) pZ]
        catchTerminal index =
            [(fromIntegral index --> 1) pZ]

        createFor scale index =
            [throw index ++ fly scale 1 (index+1)
            ,throw index ++ fly scale index (index+1) ++ catch (index+1)
            ,fly scale index 2 ++ catch (index+1)
            ,fly scale index 1 ++ catchTerminal (index+1)
            ]

        [initial_horizontal,middle_horizontal,final_horizontal,terminal_horizontal] = createFor horizontal_scale horizontal_index

        createForColumn = createFor vertical_scale . (+ vertical_start) . (* 2)
        initialVerticalForColumn = (!! 0) . createForColumn
        middleVerticalForColumn = (!! 1) . createForColumn
        finalVerticalForColumn = (!! 2) . createForColumn
        terminal_vertical = (!! 3) . createForColumn $ (width-1)

        passVerticalUnlessColumn column =
            passRange
            .
            map (vertical_start +)
            $
            [0..column*2-1] ++ [(column+1)*2..width*2-1]

        top =
            [makeOperatorSiteTensorFromSpecification 1 bandwidth $
             pass 1
                ++ initial_horizontal ++ initialVerticalForColumn 0
            ]
            ++
            [makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
             passRange [1,2] ++ passVerticalUnlessColumn column
                ++ middle_horizontal ++ initialVerticalForColumn column
            | column <- [1..width-2]
            ]
            ++
            [makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
             passRange [1,2] ++ passVerticalUnlessColumn (width-1)
                ++ final_horizontal ++ initialVerticalForColumn (width-1)
            ]

        middle =
            [makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
             passRange [1,2] ++ passVerticalUnlessColumn 0
                ++ initial_horizontal ++ middleVerticalForColumn 0
            ]
            ++
            [makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
             passRange [1,2] ++ passVerticalUnlessColumn column
                ++ middle_horizontal ++ middleVerticalForColumn column
            | column <- [1..width-2]
            ]
            ++
            [makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
             passRange [1,2] ++ passVerticalUnlessColumn (width-1)
                ++ final_horizontal ++ middleVerticalForColumn (width-1)
            ]

        bottom =
            [makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
             passRange [1,2] ++ passVerticalUnlessColumn 0
                ++ initial_horizontal ++ finalVerticalForColumn 0
            ]
            ++
            [makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
             passRange [1,2] ++ passVerticalUnlessColumn column
                ++ middle_horizontal ++ finalVerticalForColumn column
            | column <- [1..width-2]
            ]
            ++
            [makeOperatorSiteTensorFromSpecification bandwidth 1 $
                [(2 --> 1) pI] ++ terminal_horizontal ++ terminal_vertical
            ]

    in top ++ (concat . replicate (height-2)) middle ++ bottom
-- @-node:gcross.20100130190931.1274:Operator tensors
-- @+node:gcross.20100130190931.1269:analyzeTrialEnergies
data TrialAnalysis = TrialDidBetter | TrialDidWorse | TrialDidTheSame

analyzeTrialEnergies tolerance best_energy trial_energy
    | best_energy - trial_energy > tolerance = TrialDidBetter
    | trial_energy - best_energy > tolerance = TrialDidWorse
    | otherwise = TrialDidTheSame
-- @-node:gcross.20100130190931.1269:analyzeTrialEnergies
-- @+node:gcross.20100130190931.1270:main
main = do
    args <- getArgs

    let angle = read $ args !! 0
        width = read $ args !! 1
        height = read $ args !! 2
        operator_site_tensors = makeModelOperatorSiteTensors angle width height
        initial_bandwidth = 2
        nextBandwidth = (+ 6)
        -- nextBandwidth = ceiling . (* 1.05) . fromIntegral
        bandwidth_increase_energy_change_convergence_criterion = 1e-3
        multisweep_energy_change_convergence_criterion = 1e-3
        tolerance = 1e-4

    putStrLn $ "Angle = " ++ show angle
    putStrLn $ "Width = " ++ show width
    putStrLn $ "Height = " ++ show height
    putStrLn $ "Total = " ++ show (length operator_site_tensors)

    -- @    << Define callbacks >>
    -- @+node:gcross.20100130190931.1271:<< Define callbacks >>
    current_bandwidth_ref <- newIORef initial_bandwidth
    next_bandwidth_ref <- newIORef . nextBandwidth $ initial_bandwidth
    level_number_ref <- newIORef 1

    last_site_time_ref <- getCurrentTime >>= newIORef

    let getHeading = liftM (printf "LEVEL %i: ") (readIORef level_number_ref :: IO Int)
        callback_to_show_site_status (Right number_of_iterations) _ (EnergyMinimizationChain {siteNumber = site_number, chainEnergy = current_energy}) = do
            current_bandwidth <- readIORef current_bandwidth_ref
            last_site_time <- readIORef last_site_time_ref
            current_site_time <- getCurrentTime
            unless (current_bandwidth <= 20) $
                putStrLn $ printf "\tOptimized site %i in %i iterations over %s of time; current energy = %f" site_number number_of_iterations (show $ current_site_time `diffUTCTime` last_site_time) current_energy
            writeIORef last_site_time_ref current_site_time        

        callback_to_show_site_status _ _ _ = return ()
        callback_to_decide_whether_to_declare_victory_with_trial chain = do
            heading <- getHeading
            putStrLn $ heading ++ " energy = " ++ (show . chainEnergy $ chain)
            level_number <- readIORef level_number_ref
            let new_level_number = level_number + 1
            putStrLn $ printf "Now starting on level %i... (bandwidth=2 sweeps will not be displayed)" new_level_number
            writeIORef level_number_ref new_level_number
            alwaysDeclareVictory chain
        callback_to_increase_bandwidth chain = do
            next_bandwidth <- readIORef next_bandwidth_ref
            writeIORef current_bandwidth_ref next_bandwidth
            writeIORef next_bandwidth_ref (nextBandwidth next_bandwidth)
            increaseChainBandwidth 2 next_bandwidth chain
        callback_after_each_sweep victory_flag latest_chain = do
            heading <- getHeading
            current_bandwidth <- readIORef current_bandwidth_ref
            unless (current_bandwidth <= 2) $
                putStrLn $ heading ++ (printf "bandwidth = %i, sweep energy = %f" current_bandwidth (chainEnergy latest_chain) )
    -- @-node:gcross.20100130190931.1271:<< Define callbacks >>
    -- @nl

    -- @    << Run simulation >>
    -- @+node:gcross.20100130190931.1272:<< Run simulation >>
    (energies,_,_) <- fmap unzip3 $
        solveForMultipleLevelsWithCallbacks
            callback_to_decide_whether_to_declare_victory_with_trial
            (newChainCreator
                (writeIORef current_bandwidth_ref initial_bandwidth
                 >> 
                 writeIORef next_bandwidth_ref (nextBandwidth initial_bandwidth)
                )
                operator_site_tensors
                2 initial_bandwidth
            )
            callback_to_increase_bandwidth
            callback_after_each_sweep
            callback_to_show_site_status
            bandwidth_increase_energy_change_convergence_criterion
            multisweep_energy_change_convergence_criterion
            tolerance
            1000
            3
            []
    -- @-node:gcross.20100130190931.1272:<< Run simulation >>
    -- @nl

    putStrLn ""
    putStrLn "The energy levels are:"
    forM_ energies $ \energy -> do
        putStr "\t"
        putStrLn . show $ energy

    TimeSpec time_in_seconds _ <- getTime ProcessCPUTime

    putStrLn $ "The elapsed CPU time for this run was " ++ show time_in_seconds ++ " seconds."

-- @-node:gcross.20100130190931.1270:main
-- @-others
-- @-node:gcross.20100130190931.1265:@thin simulate-cluster.hs
-- @-leo
