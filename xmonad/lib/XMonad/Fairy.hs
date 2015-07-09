{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module XMonad.Fairy (
  TopicItem (..),
  changeDir,
  checkTopics,
  dynamiclogCurrent,
  dynamiclogUrgent,
  dynamiclogVisible,
  emacsKeys,
  fairyConfig,
  layoutsConfig,
  XMonad.Fairy.manageHook,
  XMonad.FairyTheme.normalBorder,
  promptConfig,
  runColourTerm,
  termCmd,
  editorCmd,
  topicsConfig,
  topicsList,
  topicsTable,
  workspaceTagSep
  ) where

-- XMonad:
import XMonad hiding ( (|||) )
import XMonad.Layout.LayoutCombinators
-- import qualified XMonad.Operations as Op

import qualified XMonad.StackSet as StackSet

-- Haskell:
import qualified Control.Monad as Monad
import qualified Data.Char as Char
-- import qualified Data.Either.Utils
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid
import qualified System.Environment
import qualified System.Exit
import qualified System.IO
import qualified System.Directory

-- Contrib:
-- import qualified XMonad.Actions.Commands as Actions.Commands -- unused
import qualified XMonad.Actions.CycleWS as Actions.CycleWS
import qualified XMonad.Actions.CycleRecentWS as Actions.CycleRecentWS
import qualified XMonad.Actions.CycleWindows as Actions.CycleWindows
-- import qualified XMonad.Actions.DeManage as Actions.DeManage -- unused
import qualified XMonad.Actions.DwmPromote as Actions.DwmPromote
import qualified XMonad.Actions.DynamicWorkspaces as Actions.DynamicWorkspaces
import qualified XMonad.Actions.FlexibleResize as Actions.FlexibleResize
-- import qualified XMonad.Actions.FlexibleManipulate as Actions.FlexibleManipulate -- unused
-- import qualified XMonad.Actions.FloatKeys as Actions.FloatKeys -- unused
import qualified XMonad.Actions.FloatSnap as Actions.FloatSnap
import qualified XMonad.Actions.GroupNavigation as Actions.GroupNavigation
-- import qualified XMonad.Actions.Navigation2D as Actions.Navigation2D
import qualified XMonad.Actions.RandomBackground as Actions.RandomBackground
import qualified XMonad.Actions.TopicSpace as Actions.TopicSpace
import qualified XMonad.Actions.UpdatePointer as Actions.UpdatePointer
import qualified XMonad.Actions.WithAll as Actions.WithAll
import qualified XMonad.Hooks.DynamicLog as Hooks.DynamicLog
import qualified XMonad.Hooks.EwmhDesktops as Hooks.EwmhDesktops
import qualified XMonad.Hooks.ManageDocks as Hooks.ManageDocks
import qualified XMonad.Hooks.ManageHelpers as Hooks.ManageHelpers
import qualified XMonad.Hooks.Minimize as Hooks.Minimize
import qualified XMonad.Hooks.Place as Hooks.Place
import qualified XMonad.Layout.BoringWindows as Layout.BoringWindows
import qualified XMonad.Layout.Circle as Layout.Circle
import qualified XMonad.Layout.Combo as Layout.Combo
import qualified XMonad.Layout.FixedColumn as Layout.FixedColumn
import qualified XMonad.Layout.Gaps as Layout.Gaps
-- import qualified XMonad.Layout.GridVariants as Layout.GridVariants
import qualified XMonad.Layout.Grid as Layout.Grid
import qualified XMonad.Layout.LimitWindows as Layout.LimitWindows
import qualified XMonad.Layout.Minimize as Layout.Minimize
import qualified XMonad.Layout.MultiColumns as Layout.MultiColumns
import qualified XMonad.Layout.NoBorders as Layout.NoBorders
import qualified XMonad.Layout.PerWorkspace as Layout.PerWorkspace
import qualified XMonad.Layout.Renamed as Layout.Renamed
import qualified XMonad.Layout.ResizableTile as Layout.ResizableTile
-- import qualified XMonad.Layout.SimpleFloat as Layout.SimpleFloat -- unused
-- import qualified XMonad.Layout.Tabbed as Layout.Tabbed -- unused
-- import qualified XMonad.Layout.Spacing as Layout.Spacing -- unused
import qualified XMonad.Layout.ThreeColumns as Layout.ThreeColumns
import qualified XMonad.Layout.WindowNavigation as Layout.WindowNavigation
-- import qualified XMonad.Layout.WorkspaceDir as Layout.WorkspaceDir
import qualified XMonad.Prompt as Prompt
import qualified XMonad.Prompt.Directory as Prompt.Directory
-- import qualified XMonad.Prompt.Input as Prompt.Input -- unused
import qualified XMonad.Prompt.Shell as Prompt.Shell
-- import qualified XMonad.Prompt.Ssh as Prompt.Ssh -- unused
import qualified XMonad.Prompt.Window as Prompt.Window
import qualified XMonad.Prompt.Workspace as Prompt.Workspace
import qualified XMonad.Util.ExtensibleState as Util.ExtensibleState
import qualified XMonad.Util.EZConfig as Util.EZConfig
-- import qualified XMonad.Util.Loggers as Util.Loggers -- unused
import qualified XMonad.Util.NamedWindows as Util.NamedWindows
import qualified XMonad.Util.Run as Util.Run
import qualified XMonad.Util.Stack as Util.Stack
import qualified XMonad.Util.WindowProperties as Util.WindowProperties
import qualified XMonad.Util.WorkspaceCompare as Util.WorkspaceCompare
import qualified XMonad.Util.XSelection as Util.XSelection

import qualified XMonad.L as L
import XMonad.FairyTheme

-- Utils:
data TopicItem = TopicItem {
  topicName :: Actions.TopicSpace.Topic,
  topicAccel :: String,
  topicDir :: String,
  topicAction :: X ()
}

topicsConfig = Actions.TopicSpace.TopicConfig {
  Actions.TopicSpace.topicDirs =
     (Map.fromList (map (\(TopicItem n _ d _) -> (n, d)) topicsTable)),
  Actions.TopicSpace.topicActions =
    (Map.fromList (map (\(TopicItem n _ _ a) -> (n, a)) topicsTable)),
  Actions.TopicSpace.defaultTopicAction = const (return ()),
  Actions.TopicSpace.defaultTopic = "*scratch*",
  Actions.TopicSpace.maxTopicHistory = 128
}

topicsList = map (\(TopicItem n _ _ _) -> n) topicsTable
checkTopics = Actions.TopicSpace.checkTopicConfig topicsList topicsConfig

-- Creates the workspace if needed.
workspaceGoto :: Actions.TopicSpace.Topic -> X ()
workspaceGoto t = (workspaceNew t) >>
                  (Actions.TopicSpace.switchTopic topicsConfig t)

workspaceShift :: Actions.TopicSpace.Topic -> X ()
workspaceShift t = (workspaceNew t) >> ((windows . StackSet.shift) t)

-- isWorkspace sc w =
--   w `elem` map StackSet.tag (StackSet.current w : StackSet.visible w)

workspaceTagSep = '/'
workspaceTagGroups = (Actions.CycleWS.WSTagGroup workspaceTagSep)
workspaceTagNonGroups =
  (Actions.CycleWS.WSIs
   (do cur <- (fmap
               (groupName . StackSet.workspace . StackSet.current)
               (gets windowset))
       return ((\ x -> (cur /= x)) . groupName)))
  where groupName = (takeWhile (/= workspaceTagSep) . StackSet.tag)

workspaceNew :: WorkspaceId -> X ()
workspaceNew w = do exists <- workspaceIdExist w
                    if (not exists)
                    then (Actions.DynamicWorkspaces.addHiddenWorkspace w)
                    else return ()

workspaceIdExist :: WorkspaceId -> X Bool
workspaceIdExist wid = do
  xs <- get
  return (workspaceIdExists wid (windowset xs))

workspaceIdExists :: WorkspaceId ->
                     StackSet.StackSet WorkspaceId l a s sd ->
                     Bool
workspaceIdExists wid ws = elem wid (map StackSet.tag (StackSet.workspaces ws))

changeDir c = Prompt.Directory.directoryPrompt
              c "Directory: " (liftIO . System.Directory.setCurrentDirectory)

-- Prompt theme
promptConfig = Prompt.defaultXPConfig {
  Prompt.font = XMonad.FairyTheme.font,
  Prompt.bgColor = promptBg colorTheme,
  Prompt.fgColor = promptFg colorTheme,
  -- bgHLight = colorBlue,
  -- fgHLight = colorWhite,
  Prompt.borderColor = promptBorder colorTheme,
  -- promptBorderWidth = 1,
  Prompt.height = 24,
  -- position = Top,
  -- historySize = 100,
  -- historyFilter = deleteConsecutive,
  -- Prompt.autoComplete = Just 1000000, -- wait 0.1 second
  Prompt.promptKeymap =
    Map.union
    Prompt.defaultXPKeymap
    (Map.fromList
     [((controlMask, xK_b), Prompt.moveCursor Prompt.Prev),
      ((controlMask, xK_f), Prompt.moveCursor Prompt.Next),
      ((mod1Mask, xK_b), Prompt.moveWord Prompt.Prev),
      ((mod1Mask, xK_f), Prompt.moveWord Prompt.Next),
      ((mod1Mask, xK_d), Prompt.killWord Prompt.Next),
      ((mod1Mask, xK_BackSpace), Prompt.killWord Prompt.Prev),
      ((controlMask, xK_p), Prompt.moveHistory StackSet.focusUp'),
      ((controlMask, xK_n),
       Prompt.moveHistory StackSet.focusDown')]),
  Prompt.searchPredicate = isMultifixOf
}

isMultifixOf :: String -> String -> Bool
isMultifixOf needles haystack =
  (and (map (flip List.isInfixOf haystack)
       (List.words needles)))

-- Applications
termCmd = "xterm" -- "uxterm -class XTerm"
shellCmd = "bash"
runTerm :: X ()
runTerm = (spawn termCmd)

runColourTerm = do
  [x, y] <- Util.ExtensibleState.gets termBackground
  dir <- Actions.TopicSpace.currentTopicDir topicsConfig
  c <- Actions.RandomBackground.randomBg' (Actions.RandomBackground.HSV x y)
  spawn
    (termCmd ++ " -bg " ++ c ++ " -e 'cd " ++ dir ++ " ; " ++ shellCmd ++ "'")

runInColourTerm cmd = do
    [x, y] <- Util.ExtensibleState.gets termBackground
    c <- Actions.RandomBackground.randomBg' (Actions.RandomBackground.HSV x y)
    spawn (termCmd ++ " -bg " ++ c ++ " -e " ++ cmd)

runColourScreenTerm = do
  tag <- gets (StackSet.currentTag . windowset)
  runInColourTerm ("screen -xR " ++ (takeWhile (/= workspaceTagSep) tag))

-- inTerminal cmd = (termCmd ++ " -e " ++ cmd)
-- saveSession cmd = "/bin/bash -c '" ++ cmd ++ "; /bin/bash'"
-- runInTerminal f = transformPromptSelection f (termCmd ++ " -e ")
-- pasteTerminal = runInTerminal saveSession
-- manTerminal = runInTerminal manPage

runChat = runInColourTerm "ssh -t personal-server emacsclient -t"

runCloveClojure = runInColourTerm "clove -i clojure"
runGhci = runInColourTerm "ghci"
runPythonInteractive = runInColourTerm "python -i"

chromeCmd = "chromium"
runChrome = spawn chromeCmd
 -- TODO: This is unreliable; properly escape or use execve.
runInChrome uri = spawn (chromeCmd ++ " --new-window '" ++ uri ++ "'")
-- pasteChrome = safePromptSelection ChromeCmd

firefoxCmd = "firefox"
runFirefox :: X ()
runFirefox = spawn firefoxCmd
runInFirefox uri = spawn (firefoxCmd ++ " -new-window '" ++ uri ++ "'")

-- Using Gmail for mail and calendar.
runMail = runInChrome "https://mail.google.com/mail"
runCalendar = runInChrome "https://www.google.com/calendar"

runCmdLine = Prompt.Shell.shellPrompt promptConfig

fileManagerCmd = "nautilus"
runFileManager :: X ()
runFileManager = spawn fileManagerCmd

-- musicPlayerCmd = inTerminal "ncmpc"
-- runMusicPlayer = spawn musicPlayerCmd
-- pasteMusicPlayer = promptSelection musicPlayerCmd

-- mixerCmd = inTerminal "alsamixer"
-- runMixer = spawn mixerCmd

restartXmonad = (broadcastMessage ReleaseResources) >>
                (restart "xmonad" True)

-- rememberCmd = "/path/to/emacsclient-starter org-protocol:/remember:/t/foo/"
-- -- for adding quick reminders to your agenda
-- runRemember = spawn rememberCmd

editorCmd = "emacsclient -nc"

runEditor :: X ()
runEditor = spawn editorCmd

runEditorHere :: X ()
runEditorHere = spawn (editorCmd ++ " " ++ ".")

-- Topics and Premade Workspaces
topicsTable :: [TopicItem]
topicsTable = [
  TopicItem "*scratch*" "s" "~/tmp" (runColourTerm >> runEditor),
  TopicItem "terminals" "r" "~" (Monad.replicateM_ 2 runColourTerm),
  TopicItem "browse"    "y" "~" runChrome,
  TopicItem "irc" "i" "~" runChat,
  TopicItem "mail" "m" "~" runMail,
  TopicItem "agenda" "a" "~" ((spawn (editorCmd ++ " ~/agenda.org")) >>
                              (runCalendar))
  ]
-- TODO: specify perworkspace layout in topicsTable

layoutsConfig =
  Hooks.ManageDocks.avoidStruts
  (Layout.WindowNavigation.configurableNavigation
   Layout.WindowNavigation.noNavigateBorders
   (Layout.BoringWindows.boringWindows
    (Layout.Minimize.minimize
     -- Layout.PerWorkspace.onWorkspace "agenda" layoutTiled2 $
     -- Layout.PerWorkspace.onWorkspace "rtb/main" layoutGrid $
     (Layout.Gaps.gaps [(Layout.Gaps.L, 0)]
      (L.named "Based Columns" layoutBasedColumns |||
       L.named "Fixed" layoutFixed |||
       L.named "Tall" layoutTall |||
       L.named "Tall Limited" (L.limit 5 layoutTall) |||
       L.named "Wide" layoutWide |||
       L.named "Wide Limited" (L.limit 5 layoutWide) |||
       L.named "Multicol" layoutMulticol |||
       L.named "Tall3 Mid" layoutTall3mid |||
       L.named "Tall3 Left" layoutTall3 |||
       L.named "Grid" layoutGrid |||
       L.named "Isolated Left" layoutRightPaned |||
       L.named "Circle" Layout.Circle.Circle |||
       L.named "Circular" L.Circular |||
       (Layout.NoBorders.noBorders Full))))))

layoutTall = Layout.ResizableTile.ResizableTall 1 0.03 0.5 []
layoutWide = Mirror (Layout.ResizableTile.ResizableTall 0 0.03 0.8 [])
layoutBasedColumns =
  L.reflectVert
  (L.limit 5 (Mirror (Layout.ResizableTile.ResizableTall 1 0.03 0.2 [])))
layoutTall3 = Layout.ThreeColumns.ThreeCol 1 (3/100) (1/2)
layoutTall3mid = Layout.ThreeColumns.ThreeColMid 1 (3/100) (1/2)
layoutGrid = Layout.Grid.GridRatio 1.1
layoutRightPaned = layoutGrid **||* layoutTall3
layoutFixed = Layout.FixedColumn.FixedColumn 1 20 80 10
layoutMulticol = Layout.MultiColumns.multiCol [1, 1] 3 0.02 0.28

-- _onWorkspace t l = (Layout.PerWorkspace.onWorkspace
--                       t
--                       (Layout.WorkspaceDir.workspaceDir (_folderOf t) l))

-- _folderOf = (Maybe.fromMaybe "~" . flip Map.lookup _topicDirs)


-- Applications Maybe

-- TODO: Tell xmonad about prefered directory of current workspace from the
--       command line.

-- TODO: We need to be able to specify options.  We also need to modify some
--       settings while prompt is showing.  Interesting options:
--       - OnlyCurrentWorkspace. (Or OnlyVisibleWorkspace) + Also sort by recency.
--       - IncludeIconified, ExcludeIconified.
--       - Allow multiple (but this should really depend on the action we are taking).
data WindowPrompt = Goto | Focus | FocusNonIconified | Bring
instance Prompt.XPrompt WindowPrompt where
  showXPrompt Goto = "Go to window: " -- In any workspace
  showXPrompt Focus = "Focus to window: " -- In the same workspace
  showXPrompt FocusNonIconified = "Focus to non-iconified window: "
  showXPrompt Bring = "Bring window(s): "
  commandToComplete _ c = c
  nextCompletion _ = Prompt.getNextCompletion

windowPrompt :: WindowPrompt -> Prompt.XPConfig -> X ()
windowPrompt t c = do
  action <- case t of
    Goto -> fmap gotoAction $ windowMap pred
    Focus -> fmap gotoAction $ windowMap pred
    FocusNonIconified -> fmap gotoAction $ windowMap pred
    Bring -> fmap gotoAction $ windowMap pred
  wm <- windowMap pred
  Prompt.mkXPrompt t c (compList wm) action
    where
      winAction a m = flip whenJust (windows . a) . flip Map.lookup m
      gotoAction = winAction StackSet.focusWindow
      compList m s = (return .
                      filter (Prompt.searchPredicate c s) .
                      map fst .
                      Map.toList) m
      pred x = case t of
        Goto -> True
        Focus -> True -- TODO: Should look at window state (Is in current WS).
        FocusNonIconified -> True -- TODO: Should look at state.
        Bring -> True

-- | A map from window names to Windows.
windowMap :: (Window -> Bool) -> X (Map.Map String Window)
windowMap pred = do
  ws <- gets windowset
  Map.fromList `fmap` concat `fmap` mapM keyValuePairs (StackSet.workspaces ws)
    where keyValuePairs ws =
            mapM (keyValuePair ws) (StackSet.integrate' (StackSet.stack ws))
          keyValuePair ws w = flip (,) w `fmap` decorateWindowName ws w

-- | Returns the window name as will be listed in dmenu.
--   Lowercased, for your convenience (since dmenu is case-sensitive).
--   Tagged with the workspace ID, to guarantee uniqueness, and to let the user
--   know where he's going.
decorateWindowName :: WindowSpace -> Window -> X String
decorateWindowName ws w = do
  name <- fmap (map Char.toLower . show) $ Util.NamedWindows.getName w
  return ("#" ++ StackSet.tag ws ++ " " ++ name)

emacsKeys :: [(String, X())]
emacsKeys  =
  [ -- Applications
    ("M-C-S-r", runColourTerm),
    ("M-C-r", runColourScreenTerm),
    -- ("M-v M-t", pasteTerminal),
    -- ("M-v M-y", pasteChrome),
    -- ("M-h n", manTerminal),
    ("M-C-e", runEditor),
    -- ("M-C-S-e", runEditorHere),
    ("M-C-c", runChrome),
    ("M-C-y", runFirefox),
    ("M-C-u", runCloveClojure),
    ("M-C-S-u", runPythonInteractive),
    ("M-C-o", runGhci),
    ("M-S-1", runCmdLine),
    ("M-S-s", (changeDir promptConfig)),
    ("M-C-t", runFileManager)] ++

  (map (\(TopicItem n k _ _) ->
         ("M-d " ++ k, workspaceGoto n)) topicsTable) ++
  (map (\(TopicItem n k _ _) ->
         ("M-S-d " ++ k, workspaceShift n)) topicsTable) ++

  [("M-m", withFocused Layout.Minimize.minimizeWindow),
   ("M-C-m", withFocused
             (\ w -> (sendMessage (Layout.Minimize.RestoreMinimizedWin w)))),
   ("M-S-m", sendMessage Layout.Minimize.RestoreNextMinimizedWin),

   -- Layouts
   ("M-C-s", refresh),

   -- ("M-e d", (setLayout (XMonad.layoutHook conf))),
   ("M-e n", (sendMessage NextLayout))] ++

  [("M-e f", (sendMessage (JumpToLayout "Full"))),
   ("M-e 1", (sendMessage (JumpToLayout "Tall"))),
   ("M-e S-1", (sendMessage (JumpToLayout "Tall Limited"))),
   ("M-e 2", (sendMessage (JumpToLayout "Wide"))),
   ("M-e S-2", (sendMessage (JumpToLayout "Wide Limited"))),
   ("M-e 3", (sendMessage (JumpToLayout "Tall3 Mid"))),
   ("M-e S-3", (sendMessage (JumpToLayout "Tall3 Left"))),
   ("M-e g", (sendMessage (JumpToLayout "Grid"))),
   ("M-e c", (sendMessage (JumpToLayout "Circle"))),
   ("M-e S-c", (sendMessage (JumpToLayout "Circular"))),
   ("M-e l", (sendMessage (JumpToLayout "Isolated Left"))),
   ("M-e s", (sendMessage (JumpToLayout "Fixed"))),
   ("M-e b", (sendMessage (JumpToLayout "Based Columns"))),
   ("M-e m", (sendMessage (JumpToLayout "Multicol"))),

   ("M-e r", (sendMessage (Layout.Gaps.ToggleGaps))),
   ("M-e M-r M-e", (sendMessage (Layout.Gaps.DecGap 200 Layout.Gaps.L))),
   ("M-e M-r M-t", (sendMessage (Layout.Gaps.IncGap 200 Layout.Gaps.L)))
  ] ++

  (let keyDirs = ["<Up>", "<Right>", "<Down>", "<Left>"] ++
                  ["p", "f", "n", "b"]
       navDirs = [Layout.WindowNavigation.U, Layout.WindowNavigation.R,
                   Layout.WindowNavigation.D, Layout.WindowNavigation.L]
       floatsnapDirs = [Actions.FloatSnap.U, Actions.FloatSnap.R,
                         Actions.FloatSnap.D, Actions.FloatSnap.L]
   in [("M-" ++ a, sendMessage (Layout.WindowNavigation.Go b))
      | (a, b) <- zip keyDirs (cycle navDirs)] ++
      [("M-S-" ++ a, sendMessage (Layout.WindowNavigation.Swap b))
      | (a, b) <- zip keyDirs (cycle navDirs)] ++
      [("M-C-" ++ a, withFocused (Actions.FloatSnap.snapMove b Nothing))
      | (a, b) <- zip keyDirs (cycle floatsnapDirs)] ++
      [("M-M1-" ++ a, withFocused (Actions.FloatSnap.snapGrow b Nothing))
      | (a, b) <- zip keyDirs (cycle floatsnapDirs)] ++
      [("M-M1-S-" ++ a, withFocused (Actions.FloatSnap.snapShrink b Nothing))
      | (a, b) <- zip keyDirs (cycle floatsnapDirs)]) ++

  -- ("M-<Tab>", (Layout.BoringWindows.focusDown)),
  -- ("M-S-<Tab>", (Layout.BoringWindows.focusUp)),
  [(k, (Actions.CycleWindows.cycleRecentWindows [xK_Super_L] xK_Tab xK_grave) >>
       (Actions.UpdatePointer.updatePointer
        (Actions.UpdatePointer.Relative 1 1)))
   | k <- ["M-S-e", "M-<Tab>"]] ++

  [("M-<Return>", (Actions.DwmPromote.dwmpromote)),
   ("M-S-<Return>", (Layout.BoringWindows.focusMaster)),

   ("M-C-1", (screenWorkspace 0) >>= (flip whenJust (windows . StackSet.view))),
   ("M-C-2", (screenWorkspace 1) >>= (flip whenJust (windows . StackSet.view))),
   ("M-C-3", (screenWorkspace 2) >>= flip whenJust (windows . StackSet.view)),
   ("M-C-S-1", (screenWorkspace 0 >>=
                flip whenJust (windows . StackSet.shift))),
   ("M-C-S-2", (screenWorkspace 1 >>=
                flip whenJust (windows . StackSet.shift))),
   ("M-C-S-3", (screenWorkspace 2 >>=
                flip whenJust (windows . StackSet.shift))),

   ("M-S-=", sendMessage Layout.ResizableTile.MirrorShrink),
   ("M-S--", sendMessage Layout.ResizableTile.MirrorExpand),
   ("M--", sendMessage Shrink),
   ("M-=", sendMessage Expand),
   ("M-<F10>", withFocused (windows . StackSet.sink)),
   ("M-t", withFocused (windows . StackSet.sink)),
   ("M-M1-t", Actions.WithAll.withAll' StackSet.sink),

   ("M-[", sendMessage (IncMasterN 1)),
   ("M-]", sendMessage (IncMasterN (-1))),

   ("M-S-[", Layout.LimitWindows.increaseLimit),
   ("M-S-]", Layout.LimitWindows.decreaseLimit),

   -- Toggle full screen
   ("M-S-<F10>", (sendMessage Hooks.ManageDocks.ToggleStruts) >> (refresh)),
   ("M-S-t", (sendMessage Hooks.ManageDocks.ToggleStruts) >> (refresh)),

   ("M-a", Prompt.Workspace.workspacePrompt promptConfig workspaceGoto),
   ("M-S-a", Prompt.Workspace.workspacePrompt promptConfig workspaceShift),
   ("M-r", windowPrompt FocusNonIconified promptConfig),
   ("M-S-r", windowPrompt Goto promptConfig),
   ("M-M1-r", windowPrompt Focus promptConfig),

   -- ("M-M1-r", Prompt.Window.windowPromptGoto promptConfig),
   ("M-M1-S-r", Prompt.Window.windowPromptBring promptConfig),

   ("M-q", kill),
   ("M-S-q", Actions.WithAll.killAll)] ++

  [("M-j", Actions.CycleWS.moveTo Actions.CycleWS.Next workspaceTagGroups),
   ("M-k", Actions.CycleWS.moveTo Actions.CycleWS.Prev workspaceTagGroups),
   ("M-S-j", Actions.CycleWS.shiftTo Actions.CycleWS.Next workspaceTagGroups),
   ("M-S-k", Actions.CycleWS.shiftTo Actions.CycleWS.Prev workspaceTagGroups),

   ("M-.", Actions.CycleWS.moveTo Actions.CycleWS.Next workspaceTagNonGroups),
   ("M-,", Actions.CycleWS.moveTo Actions.CycleWS.Prev workspaceTagNonGroups),
   ("M-l", Actions.CycleWS.moveTo
             Actions.CycleWS.Next Actions.CycleWS.NonEmptyWS),
   ("M-h", Actions.CycleWS.moveTo
             Actions.CycleWS.Prev Actions.CycleWS.NonEmptyWS),
   ("M-S-l", Actions.CycleWS.shiftTo
               Actions.CycleWS.Next Actions.CycleWS.NonEmptyWS),
   ("M-S-h", Actions.CycleWS.shiftTo
              Actions.CycleWS.Prev Actions.CycleWS.NonEmptyWS),
   ("M-C-l", Actions.CycleWS.moveTo
              Actions.CycleWS.Next Actions.CycleWS.EmptyWS),
   ("M-C-h", Actions.CycleWS.moveTo
              Actions.CycleWS.Prev Actions.CycleWS.EmptyWS),
   ("M-C-S-l", Actions.CycleWS.shiftTo
                Actions.CycleWS.Next Actions.CycleWS.EmptyWS),
   ("M-C-S-h", Actions.CycleWS.shiftTo
                Actions.CycleWS.Prev Actions.CycleWS.EmptyWS)]
  ++
  [("M-`", Actions.CycleRecentWS.cycleRecentWS [xK_Super_L] xK_grave xK_Tab),

   ("M-S-<Backspace>", (Actions.WithAll.killAll) >>
                       (Actions.DynamicWorkspaces.removeWorkspace)),
   -- Buggy, messes with focus and creates flicker, needs to be fixed.

   ("M-C-a", Actions.DynamicWorkspaces.renameWorkspace promptConfig),

   ("M-i 1", (Util.ExtensibleState.put colorThemeDark) >>
             (spawn "xrdb -merge .local/etc/Xresources-dark") >>
             (spawn "emacsclient -e \"(set-theme 'dark-forge)\"")),
   ("M-i 2", (Util.ExtensibleState.put colorThemeLight) >>
             (spawn "xrdb -merge .local/etc/Xresources-light") >>
             (spawn "emacsclient -e \"(set-theme 'whitestone-serious)\"")),
   ("M-i 3", (Util.ExtensibleState.put colorThemeGrey) >>
             (spawn "xrdb -merge .local/etc/Xresources-dark") >>
             (spawn "emacsclient -e \"(set-theme 'fruitsalad-dark)\"")),
   ("M-i 4", (Util.ExtensibleState.put colorThemeLightWood) >>
             (spawn "xrdb -merge .local/etc/Xresources-light") >>
             (spawn "emacsclient -e \"(set-theme 'light-balcony)\"")),

   -- -- Commands
   -- , ("M-y", runCommand _commands)

   -- -- Remember
   -- , ("M-C-f", runRemember)
   -- xmonad
   ("M-C-S-q", restartXmonad)]

-- NEW (for xmonad-0.11):
-- -- Switch between layers
--   , ((modm,                 xK_space), switchLayers)

--   -- Directional navigation of windows
--   , ((modm,                 xK_Right), windowGo R False)
--   , ((modm,                 xK_Left ), windowGo L False)
--   , ((modm,                 xK_Up   ), windowGo U False)
--   , ((modm,                 xK_Down ), windowGo D False)

--   -- Swap adjacent windows
--   , ((modm .|. controlMask, xK_Right), windowSwap R False)
--   , ((modm .|. controlMask, xK_Left ), windowSwap L False)
--   , ((modm .|. controlMask, xK_Up   ), windowSwap U False)
--   , ((modm .|. controlMask, xK_Down ), windowSwap D False)

--   -- Directional navigation of screens
--   , ((modm,                 xK_r    ), screenGo R False)
--   , ((modm,                 xK_l    ), screenGo L False)
--   , ((modm,                 xK_u    ), screenGo U False)
--   , ((modm,                 xK_d    ), screenGo D False)

--   -- Swap workspaces on adjacent screens
--   , ((modm .|. controlMask, xK_r    ), screenSwap R False)
--   , ((modm .|. controlMask, xK_l    ), screenSwap L False)
--   , ((modm .|. controlMask, xK_u    ), screenSwap U False)
--   , ((modm .|. controlMask, xK_d    ), screenSwap D False)

--   -- Send window to adjacent screen
--   , ((modm .|. mod1Mask,    xK_r    ), windowToScreen R False)
--   , ((modm .|. mod1Mask,    xK_l    ), windowToScreen L False)
--   , ((modm .|. mod1Mask,    xK_u    ), windowToScreen U False)
--   , ((modm .|. mod1Mask,    xK_d    ), windowToScreen D False)

-- Mouse bindings
mouseBindings :: XConfig Layout ->
                  Map.Map (ButtonMask, Button) (Window -> X ())
mouseBindings (XConfig {XMonad.modMask = modMask}) =
               (Map.fromList
                ([((modMask, button1),
                   (\w -> (focus w) >> (mouseMoveWindow w))),
                  ((modMask, button3),
                   (\w -> (focus w) >>
                          (Actions.FlexibleResize.mouseResizeWindow w)))]))

-- Application specific window handling
manageHook =
  (XMonad.manageHook defaultConfig
   <+> composeAll
   [Hooks.ManageHelpers.isFullscreen --> Hooks.ManageHelpers.doFullFloat,
    appName =? "Dialog" --> Hooks.ManageHelpers.doCenterFloat,
    ((className =? "Emacs24") <||> (className =? "Emacs")) --> unfloat,
    (className =? "XTermEmacs") --> unfloat,
    (className =? "Thunderbird") --> unfloat,
    (((className =? "Firefox") <||> (className =? "Nightly")) <&&>
     (roleName =? "browser")) --> unfloat,
    (((className =? "Chromium") <||> (className =? "Chromium-browser")) <&&>
     (roleName =? "browser")) --> unfloat,
    -- checkDock --> doIgnore,

    (((className =? "Inkscape") <||>
      (className =? "Inkscape-bin")) <&&>
     (fmap ("- Inkscape" `List.isSuffixOf`) title)) --> unfloat]
   -- <+> composeOne [
   --       transience,
   --       className =? "Firefox" -?> doF (StackSet.shift "browse")]
   -- <+> Hooks.Place.placeHook Hooks.Place.simpleSmart
   <+> Hooks.ManageHelpers.doCenterFloat -- Float by default, at center.
   -- <+> doFloat -- Float by default.
   <+> Hooks.ManageDocks.manageDocks)
  where unfloat = ask >>= (doF . StackSet.sink)
        roleName = stringProperty "WM_WINDOW_ROLE"

fairyConfig = defaultConfig {
  borderWidth = 4,
  terminal = termCmd,
  normalBorderColor = XMonad.FairyTheme.normalBorder colorTheme, -- Well, this sucks!
  focusedBorderColor = XMonad.FairyTheme.focusedBorder colorTheme, -- Same as this.
  workspaces = topicsList,
  layoutHook = layoutsConfig,
  modMask = mod4Mask,
  logHook = Actions.GroupNavigation.historyHook,
  handleEventHook = Hooks.Minimize.minimizeEventHook,
  XMonad.mouseBindings = XMonad.Fairy.mouseBindings,
  XMonad.manageHook = XMonad.Fairy.manageHook
}
