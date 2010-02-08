-- @+leo-ver=4-thin
-- @+node:gcross.20100201141018.1256:@thin simulate-cluster-1d.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100201141018.1257:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
-- @-node:gcross.20100201141018.1257:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20100201141018.1258:<< Import needed modules >>
import Acme.Dont

import Control.Applicative.Infix
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans

import Data.Complex
import Data.ConfigFile
import Data.IORef
import Data.UUID

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
-- @-node:gcross.20100201141018.1258:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100201141018.1259:Operator tensors
makeModelOperatorSiteTensors :: Double -> Int -> [OperatorSiteTensor]
makeModelOperatorSiteTensors factor width =
    let horizontal_index = 3
        horizontal_range = [horizontal_index,horizontal_index+1]

        bandwidth = maximum
            [last horizontal_range
            ]

        passRange = concat . map pass

        pass index =
            [(index_ --> index_) pI]
          where
            index_ = fromIntegral index

        throw index =
            [(1 --> fromIntegral index) pZ]
        fly index1 index2 =
            [(fromIntegral index1 --> fromIntegral index2) pX]
        catch index =
            [(fromIntegral index --> 2) pZ]
        catchTerminal index =
            [(fromIntegral index --> 1) pZ]

        createFor index =
            [throw index ++ fly 1 (index+1)
            ,throw index ++ fly index (index+1) ++ catch (index+1)
            ,fly index 2 ++ catch (index+1)
            ,fly index 1 ++ catchTerminal (index+1)
            ]

        [initial_horizontal,middle_horizontal,final_horizontal,terminal_horizontal] = createFor horizontal_index

    in
            [makeOperatorSiteTensorFromSpecification 1 bandwidth $
             pass 1 ++ initial_horizontal
            ]
            ++
            [makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
             passRange [1,2] ++ middle_horizontal
            | column <- [1..width-2]
            ]
            ++
            [makeOperatorSiteTensorFromSpecification bandwidth 1 $
                [(2 --> 1) pI] ++ terminal_horizontal
            ]
-- @-node:gcross.20100201141018.1259:Operator tensors
-- @+node:gcross.20100201141018.1260:analyzeTrialEnergies
data TrialAnalysis = TrialDidBetter | TrialDidWorse | TrialDidTheSame

analyzeTrialEnergies tolerance best_energy trial_energy
    | best_energy - trial_energy > tolerance = TrialDidBetter
    | trial_energy - best_energy > tolerance = TrialDidWorse
    | otherwise = TrialDidTheSame
-- @-node:gcross.20100201141018.1260:analyzeTrialEnergies
-- @+node:gcross.20100201141018.1261:main
main = do
    args <- getArgs

    let angle = read $ args !! 0
        width = read $ args !! 1
        operator_site_tensors = makeModelOperatorSiteTensors angle width
        bandwidth_increment = 1
        initial_bandwidth = 2
        bandwidth_increase_energy_change_convergence_criterion = 1e-2
        multisweep_energy_change_convergence_criterion = 1e-2
        level_similarity_tolerance = 1e-3

    putStrLn $ "Angle = " ++ show angle
    putStrLn $ "Width = " ++ show width
    putStrLn $ "Total = " ++ show (length operator_site_tensors)

    -- @    << Define callbacks >>
    -- @+node:gcross.20100201141018.1262:<< Define callbacks >>
    next_bandwidth_ref <- newIORef initial_bandwidth
    level_number_ref <- newIORef 1

    let getHeading = liftM (printf "LEVEL %i: ") (readIORef level_number_ref :: IO Int)
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
            writeIORef next_bandwidth_ref (next_bandwidth+bandwidth_increment)
            increaseChainBandwidth 2 next_bandwidth chain
        callback_after_each_sweep victory_flag latest_chain = do
            heading <- getHeading
            next_bandwidth <- readIORef next_bandwidth_ref
            let current_bandwidth = next_bandwidth-bandwidth_increment
            unless (current_bandwidth <= 2) $
                putStrLn $ heading ++ (printf "bandwidth = %i, sweep energy = %f" current_bandwidth (chainEnergy latest_chain) )
    -- @-node:gcross.20100201141018.1262:<< Define callbacks >>
    -- @nl

    -- @    << Run simulation >>
    -- @+node:gcross.20100201141018.1263:<< Run simulation >>
    (energies,_,_) <- fmap unzip3 $
        solveForMultipleLevelsWithCallbacks
            callback_to_decide_whether_to_declare_victory_with_trial
            (newChainCreator
                (writeIORef next_bandwidth_ref (initial_bandwidth+bandwidth_increment))
                operator_site_tensors
                2 initial_bandwidth
            )
            callback_to_increase_bandwidth
            callback_after_each_sweep
            ignoreSiteCallback
            bandwidth_increase_energy_change_convergence_criterion
            multisweep_energy_change_convergence_criterion
            0
            1000
            3
            []
    -- @-node:gcross.20100201141018.1263:<< Run simulation >>
    -- @nl

    putStrLn ""
    putStrLn "The energy levels are:"
    forM_ energies $ \energy -> do
        putStr "\t"
        putStrLn . show $ energy

    TimeSpec time_in_seconds _ <- getTime ProcessCPUTime

    putStrLn $ "The elapsed CPU time for this run was " ++ show time_in_seconds ++ " seconds."

-- @-node:gcross.20100201141018.1261:main
-- @-others
-- @-node:gcross.20100201141018.1256:@thin simulate-cluster-1d.hs
-- @-leo
