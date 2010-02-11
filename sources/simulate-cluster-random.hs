-- @+leo-ver=4-thin
-- @+node:gcross.20100130190931.1265:@thin simulate-cluster-random.hs
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
import Control.Exception
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans

import Data.Complex
import Data.ConfigFile
import Data.IORef
import Data.Time.Clock

import System.Environment
import System.Exit
import System.Posix.Clock
import System.Random

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
makeModelOperatorSiteTensors :: Double -> Int -> IO [OperatorSiteTensor]
makeModelOperatorSiteTensors factor size =
    -- replicateM ((size*(size+1)) `div` 2) randomIO
    return [0.583088081582237,0.9894952859006578,0.9599318104935651,0.8989266910122071,0.8079246918922115,0.2644370328086515,0.11354908664094959,4.9575392750458636e-2,6.565561670941666e-2,0.29497428838977924,0.7974899172078562,0.34365357431668175,0.7541184558659136,0.8260322104501613,0.689705144890981,0.91054946496397,0.21410991594058226,0.2572252142143494,0.43267948854078525,0.28065492952723403]
    >>=
    \angles -> return $
    let width = size
        height = size

        getAngleAt_ x y = (angles !! ((larger*(larger+1)) `div` 2 + smaller))
          where
            larger = max x y
            smaller = min x y
        getAngleAt x y = assert (v1 == v2) $ v1
          where
            v1 = getAngleAt_ x y
            v2 = getAngleAt_ y x

        horizontal_scale = (1-factor) :+ 0
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
        fly scale angle index1 index2 =
            [(fromIntegral index1 --> fromIntegral index2) (scale *: pR)]
          where
            angle_in_radians = angle * 2 * pi
            pR = ((cos angle_in_radians :+ 0) *: pX) + ((sin angle_in_radians :+ 0) *: pY)
        catch index =
            [(fromIntegral index --> 2) pZ]
        catchTerminal index =
            [(fromIntegral index --> 1) pZ]

        createFor scale index =
            [\angle -> throw index ++ fly scale angle 1 (index+1)
            ,\angle -> throw index ++ fly scale angle index (index+1) ++ catch (index+1)
            ,\angle -> fly scale angle index 2 ++ catch (index+1)
            ,\angle -> fly scale angle index 1 ++ catchTerminal (index+1)
            ]

        [initial_horizontal,middle_horizontal,final_horizontal,terminal_horizontal] = createFor horizontal_scale horizontal_index

        createForColumn = createFor vertical_scale . (+ vertical_start) . (* 2)
        initialVerticalForColumn = ($ 0) . (!! 0) . createForColumn
        middleVerticalForColumn = ($ 0) . (!! 1) . createForColumn
        finalVerticalForColumn = ($ 0) . (!! 2) . createForColumn
        terminal_vertical = ($ 0) . (!! 3) . createForColumn $ (width-1)

        passVerticalUnlessColumn column =
            passRange
            .
            map (vertical_start +)
            $
            [0..column*2-1] ++ [(column+1)*2..width*2-1]

        top =
            [makeOperatorSiteTensorFromSpecification 1 bandwidth $
             pass 1
                ++ initial_horizontal (getAngleAt 0 0) ++ initialVerticalForColumn 0
            ]
            ++
            [makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
             passRange [1,2] ++ passVerticalUnlessColumn column
                ++ middle_horizontal (getAngleAt 0 column) ++ initialVerticalForColumn column
            | column <- [1..width-2]
            ]
            ++
            [makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
             passRange [1,2] ++ passVerticalUnlessColumn (width-1)
                ++ final_horizontal (getAngleAt 0 (width-1)) ++ initialVerticalForColumn (width-1)
            ]

        middle row =
            [makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
             passRange [1,2] ++ passVerticalUnlessColumn 0
                ++ initial_horizontal (getAngleAt row 0) ++ middleVerticalForColumn 0
            ]
            ++
            [makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
             passRange [1,2] ++ passVerticalUnlessColumn column
                ++ middle_horizontal (getAngleAt row column) ++ middleVerticalForColumn column
            | column <- [1..width-2]
            ]
            ++
            [makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
             passRange [1,2] ++ passVerticalUnlessColumn (width-1)
                ++ final_horizontal (getAngleAt row (width-1)) ++ middleVerticalForColumn (width-1)
            ]

        bottom =
            [makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
             passRange [1,2] ++ passVerticalUnlessColumn 0
                ++ initial_horizontal (getAngleAt (height-1) 0) ++ finalVerticalForColumn 0
            ]
            ++
            [makeOperatorSiteTensorFromSpecification bandwidth bandwidth $
             passRange [1,2] ++ passVerticalUnlessColumn column
                ++ middle_horizontal (getAngleAt (height-1) column) ++ finalVerticalForColumn column
            | column <- [1..width-2]
            ]
            ++
            [makeOperatorSiteTensorFromSpecification bandwidth 1 $
                [(2 --> 1) pI] ++ terminal_horizontal (getAngleAt (height-1) (width-1)) ++ terminal_vertical
            ]

    in top ++ concat [middle row | row <- [1..height-2]] ++ bottom
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

    let size = read $ args !! 0
        initial_bandwidth = 2
        -- nextBandwidth = (+ 2)
        nextBandwidth = ceiling . (* 1.25) . fromIntegral
        bandwidth_increase_energy_change_convergence_criterion = 1e-3
        multisweep_energy_change_convergence_criterion = 1e-3
        tolerance = 1e-4

    operator_site_tensors <- makeModelOperatorSiteTensors 0.5 size

    putStrLn $ "Size = " ++ show size
    unless (length operator_site_tensors == size * size) $ do
        printf 
            "PROGRAMMER ERROR:  number of sites (%i) /= size^2 (%i)^2"
            (length operator_site_tensors) size
        exitFailure
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
            unless (current_bandwidth <= 100) $
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
            2
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
-- @-node:gcross.20100130190931.1265:@thin simulate-cluster-random.hs
-- @-leo
