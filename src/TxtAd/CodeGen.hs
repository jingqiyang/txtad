{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveLift #-}
module TxtAd.CodeGen where

import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
import Language.Haskell.TH hiding (Name)
import Language.Haskell.TH.Lift
import System.IO
import System.Exit
import System.Console.Haskeline
import Control.Monad (when)
import Data.Maybe (isJust, isNothing)
-- import Debug.Trace

import Instances.TH.Lift

import TxtAd.Syntax


----- compile time analyses -----

{- errors/warnings implemented:
  * error: multiple transitions for single input
  * warning: no defined scenes (runs game that does nothing and exits)
  * error: declared start scene undefined (if there is at least 1 defined scene)
  * error: no specified start scene
  * warning: multiple declarations of start scene (defaults to last declared)
  * warning: no specified error message (defaults to empty string)
  * error: undefined scene in scene transition
  * warning: no specified final scene
  * warning: unreachable final scene(s)
 -}

-- walks a program to get scenes and build sceneText, sceneChanges, finalScenes
-- sceneText: maps scene names to text to display
-- sceneChanges: maps scene names to map of scene changes (user input to scene
-- names)
-- finalScenes: set of final scene names
allScenes :: [TxtAd] -> Maybe (Map Input String, Map Name (Map Input Name), Set Name)
allScenes (Def name sceneInfo : lines) = do
  let maybesceneInfo = allScenes lines
  when (isNothing maybesceneInfo) (Nothing)
  let Just (sceneText, sceneChanges, finalScenes) = maybesceneInfo
      (final, txt, changeslist) = sceneInfo
      changes = M.fromList changeslist

  -- check for multiple transition declarations for a single input
  when (M.size changes < length changeslist) (Nothing)

  let newText = M.insert name txt sceneText
      newChanges =
        if not (M.null changes) then M.insert name changes sceneChanges
        else sceneChanges
      newFinals =
        if final then S.insert name finalScenes
        else finalScenes
  return (newText, newChanges, newFinals)

allScenes (_:lines) = allScenes lines
allScenes [] = return (M.empty, M.empty, S.empty)


-- walks a program to grab the last defined start scene
-- takes in list of ASTs, map of scenes to scene text
-- returns the pair start scene, number of declared start scenes
startScene :: [TxtAd] -> Map Name String -> Maybe (String, Int)
startScene (Start s : lines) sceneText = do
  when (not (M.member s sceneText)) Nothing
  let maybeLaterStartInfo = startScene lines sceneText
  when (isNothing maybeLaterStartInfo) Nothing
  let Just (laterScene, numStarts) = maybeLaterStartInfo
  if numStarts == 0 then Just (s, 1)
  else Just (laterScene, numStarts + 1)

startScene (_:lines) sceneText = startScene lines sceneText
startScene [] sceneText = Just ("", 0)


-- walks a program to grab the first defined error message
errorMsg :: [TxtAd] -> Maybe String
errorMsg (Msg m : lines) = Just m
errorMsg (d : lines) = errorMsg lines
errorMsg [] = Nothing


-- params: scene text, scene changes, final scenes, visited scenes, start scene
-- returns: pair of bool, int
-- bool is true if all scenes listed in scene transitions are reachable
-- int is the number of final states that are reachable
analyzeScenes :: Map Name String -> Map Name (Map Input Name) -> Set Name
  -> Set Name -> String -> (Bool, Int)
analyzeScenes sceneText sceneChanges finalScenes visited scene =
  let maybeChanges = (M.lookup scene sceneChanges)

  -- check if visited
  in if S.member scene visited then (True, 0)

    -- scene is a final scene
    else case maybeChanges of
      Nothing ->
        if S.member scene finalScenes then (True, 1)
        -- i decided to not report a warning if a scene with no transitions
        -- is not declared as a final scene
        else if M.member scene sceneText then (True, 0)
        else (False, 0)

      -- check children to see if all scenes reachable
      -- and count up reachable final scenes
      Just changes ->
        let children = M.map (analyzeScenes sceneText sceneChanges finalScenes (S.insert scene visited)) changes
        in M.foldr (\(b1, n1) (b2, n2) -> (b1 && b2, n1 + n2)) (True, 0) children


----- runtime system -----

-- -- gets user input
-- processInput :: String -> Map Input String -> String -> IO String
-- processInput scene changes msg = do

--   input <- runInputT defaultSettings $ do
--     maybeInput <- getInputLine "> "
--     case maybeInput of
--       Nothing -> 
--       Just input -> return input

--   -- input <- getLine
--   let maybeNewScene = M.lookup input changes
--   case maybeNewScene of
--     Just newScene -> return newScene
--     Nothing -> do
--       putStr msg
--       processInput scene changes msg


-- -- game loop
-- runGame :: String -> Map Input String -> Map Name (Map Input Name) -> String -> IO ()
-- runGame scene sceneText sceneChanges msg = do
--   let maybeTxt = M.lookup scene sceneText
--   let txt = case maybeTxt of
--               Just t -> t
--               Nothing -> ""
--   putStr txt
--   let maybeChanges = M.lookup scene sceneChanges
--   let changes = case maybeChanges of
--               Just c -> c
--               Nothing -> M.empty
--   -- if reached a scene with no transitions, end the game
--   if M.null changes then return ()
--   else do
--     newScene <- processInput scene changes msg
--     runGame newScene sceneText sceneChanges msg

------------

-- gets user input
processInput :: String -> Map Input String -> String -> InputT IO String
processInput scene changes msg = do

  maybeInput <- getInputLine "> "
  input <- case maybeInput of
    Nothing -> processInput scene changes msg
    Just input -> return input

  let maybeNewScene = M.lookup input changes
  case maybeNewScene of
    Just newScene -> return newScene
    Nothing -> do
      outputStr msg
      processInput scene changes msg


-- game loop
runGame :: String -> Map Input String -> Map Name (Map Input Name) -> String -> InputT IO ()
runGame scene sceneText sceneChanges msg = do
  let maybeTxt = M.lookup scene sceneText
  let txt = case maybeTxt of
              Just t -> t
              Nothing -> ""
  outputStr txt
  let maybeChanges = M.lookup scene sceneChanges
  let changes = case maybeChanges of
                  Just c -> c
                  Nothing -> M.empty
  -- if reached a scene with no transitions, end the game
  if M.null changes then return ()
  else do
    newScene <- processInput scene changes msg
    runGame newScene sceneText sceneChanges msg


-- game that does nothing (when no scenes are defined)
emptyGame :: InputT IO ()
emptyGame = return ()


----- metaprogram -----

txtadCodeGen :: [TxtAd] -> Q Exp
txtadCodeGen prog = do
  -- validate scene transitions
  let maybesceneInfo = allScenes prog
  when (isNothing maybesceneInfo) (fail "Multiple transitions for single input")
  let Just (sceneText, sceneChanges, finalScenes) = maybesceneInfo
  if M.null sceneText then do
    reportWarning "No defined scenes"
    [| do runInputT defaultSettings $ emptyGame |]

  else do
    -- validate start scene declaration
    let maybeStartInfo = startScene prog sceneText
    when (isNothing maybesceneInfo) (fail "Declared start scene is undefined")
    let Just (start, numStarts) = maybeStartInfo
    when (numStarts == 0) (fail "No specified start scene")
    when (numStarts > 1) (reportWarning "Multiple declarations of start scene, last declaration used")

    -- validate error message declaration
    let maybeMsg = errorMsg prog
        msg = case maybeMsg of
                Nothing -> ""
                Just m -> m
    when (isNothing maybeMsg) (reportWarning "No specified error message, defaulting to empty string")

    -- traverse scenes via BFS and validate scenes/final scenes
    let numFinals = S.size finalScenes
    let (allReachable, numFinalReachable) = analyzeScenes sceneText sceneChanges finalScenes S.empty start
    when (not allReachable) (reportError "Undefined scene in scene transition(s)")
    if numFinals == 0 then reportWarning "No specified final scene(s)"
    else when (numFinalReachable < numFinals) (reportWarning "Unreachable final scene(s)")

    -- generate code to run at runtime
    [| do runInputT defaultSettings $ runGame start sceneText sceneChanges msg |]
