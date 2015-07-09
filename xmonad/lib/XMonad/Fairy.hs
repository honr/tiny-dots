{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module XMonad.Fairy
       (TopicItem (..)
       , change_dir
       , check_topics
       , dynamiclog_current
       , dynamiclog_urgent
       , dynamiclog_visible
       , emacs_keys
       , fairy_config
       , focused_border
       , layouts_config
       , manage_hook
       , normal_border
       , prompt_config
       , run_colour_term
       , term_cmd
       , topics_config
       , topics_list
       , topics_table
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
import qualified XMonad.Layout.Accordion as Layout.Accordion
import qualified XMonad.Layout.BoringWindows as Layout.BoringWindows
import qualified XMonad.Layout.Circle as Layout.Circle
import qualified XMonad.Layout.Dishes as Layout.Dishes
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
data TopicItem = TopicItem {topicName :: Actions.TopicSpace.Topic,
                            topicAccel :: String,
                            topicDir :: String,
                            topicAction :: X ()}

topics_config = (Actions.TopicSpace.TopicConfig
                 {Actions.TopicSpace.topicDirs = (Map.fromList
                                                  (map
                                                   (\(TopicItem n _ d _) -> (n, d))
                                                   topics_table)),
                  Actions.TopicSpace.topicActions = (Map.fromList
                                                     (map
                                                      (\(TopicItem n _ _ a) -> (n, a))
                                                      topics_table)),
                  Actions.TopicSpace.defaultTopicAction = (const (return ())),
                  Actions.TopicSpace.defaultTopic = "*scratch*",
                  Actions.TopicSpace.maxTopicHistory = 128})

topics_list = (map (\(TopicItem n _ _ _) -> n) topics_table)
check_topics = Actions.TopicSpace.checkTopicConfig topics_list topics_config

-- Creates the workspace if needed.
workspace_goto :: Actions.TopicSpace.Topic -> X ()
workspace_goto t = (workspace_new t) >>
                   (Actions.TopicSpace.switchTopic topics_config t)

workspace_shift :: Actions.TopicSpace.Topic -> X ()
workspace_shift t = (workspace_new t) >> ((windows . StackSet.shift) t)

-- isWorkspace sc w = w `elem` map StackSet.tag (StackSet.current w : StackSet.visible w)

workspace_tag_sep = '/'
workspace_tag_groups = (Actions.CycleWS.WSTagGroup workspace_tag_sep)
workspace_tag_non_groups = 
  (Actions.CycleWS.WSIs
   (do cur <- (fmap
               (groupName . StackSet.workspace . StackSet.current)
               (gets windowset))
       return ((\ x -> (cur /= x)) . groupName)))
  where groupName = (takeWhile (/= workspace_tag_sep) . StackSet.tag)

workspace_new :: WorkspaceId -> X ()
workspace_new w = do exists <- workspace_id_exist w
                     if (not exists)
                       then (Actions.DynamicWorkspaces.addHiddenWorkspace w)
                       else return ()

workspace_id_exist :: WorkspaceId -> X Bool
workspace_id_exist wid = do
  xs <- get
  return (workspace_id_exists wid (windowset xs))

workspace_id_exists :: WorkspaceId -> StackSet.StackSet WorkspaceId l a s sd -> Bool
workspace_id_exists wid ws = (elem wid (map StackSet.tag  (StackSet.workspaces ws)))

change_dir c = (Prompt.Directory.directoryPrompt
                c
                "Directory: "
                (liftIO . System.Directory.setCurrentDirectory))

-- Prompt theme
prompt_config = (Prompt.defaultXPConfig
              {Prompt.font = font_regular,
	       Prompt.bgColor = "#000",
	       Prompt.fgColor = "#eee",
               -- bgHLight = colorBlue,
	       -- fgHLight = colorWhite,
	       Prompt.borderColor = "#111",
	       -- promptBorderWidth = 1,
	       Prompt.height = 24,
	       -- position = Top,
	       -- historySize = 100,
	       -- historyFilter = deleteConsecutive,
	       -- Prompt.autoComplete = Just 1000000, -- wait 0.1 second
               Prompt.promptKeymap = Map.union
                                     Prompt.defaultXPKeymap
                                     (Map.fromList
                                      [((controlMask, xK_b), Prompt.moveCursor Prompt.Prev),
                                       ((controlMask, xK_f), Prompt.moveCursor Prompt.Next),
                                       ((mod1Mask, xK_b), Prompt.moveWord Prompt.Prev),
                                       ((mod1Mask, xK_f), Prompt.moveWord Prompt.Next),
                                       ((mod1Mask, xK_d), Prompt.killWord Prompt.Next),
                                       ((mod1Mask, xK_BackSpace), Prompt.killWord Prompt.Prev),
                                       ((controlMask, xK_p), Prompt.moveHistory StackSet.focusUp'),
                                       ((controlMask, xK_n), Prompt.moveHistory StackSet.focusDown')]),
               Prompt.searchPredicate = is_multifix_of})

is_multifix_of :: String -> String -> Bool
is_multifix_of needles haystack =
  (and (map (flip List.isInfixOf haystack) 
       (List.words needles)))


-- Applications
term_cmd = "xterm"
-- term_cmd = "uxterm -class XTerm"
run_term :: X ()
run_term = (spawn term_cmd)
-- run_colour_term = (Actions.RandomBackground.randomBg
--                       (Actions.RandomBackground.HSV x y))
--                      where
--                         [x, y] = term_background color_theme

-- runInColourTerminal cmd = do
--     c <- (Actions.RandomBackground.randomBg'
--            (Actions.RandomBackground.HSV x y))
--     (spawn (term_cmd ++ " -bg " ++ c ++ " -e " ++ cmd))
--     where
--         [x, y] = term_background color_theme

run_colour_term = do
  [x, y] <- Util.ExtensibleState.gets term_background
  c <- (Actions.RandomBackground.randomBg'
        (Actions.RandomBackground.HSV x y))
  (spawn (term_cmd  ++ " -bg " ++ c))

run_in_colour_term cmd = do
    [x, y] <- Util.ExtensibleState.gets term_background
    c <- (Actions.RandomBackground.randomBg'
           (Actions.RandomBackground.HSV x y))
    (spawn (term_cmd ++ " -bg " ++ c ++ " -e " ++ cmd))

run_colour_screen_term = (run_in_colour_term "screen -xR")

-- inTerminal cmd = (term_cmd ++ " -e " ++ cmd)

-- saveSession cmd = "/bin/bash -c '" ++ cmd ++ "; /bin/bash'"
-- runInTerminal f = transformPromptSelection f (term_cmd ++ " -e ")
-- pasteTerminal = runInTerminal saveSession
-- manTerminal = runInTerminal manPage

run_chat = (run_in_colour_term "ssh -t personal-server emacsclient -t")

run_clove_clojure = (run_in_colour_term "clove -i clojure")
run_ghci = (run_in_colour_term "ghci")
run_python_interactive = (run_in_colour_term "python -i")

chrome_cmd = "chromium"
run_chrome = (spawn chrome_cmd)
 -- TODO: This is unreliable; properly escape or use execve.
run_in_chrome uri = (spawn (chrome_cmd ++ " --new-window '" ++ uri ++ "'"))
-- pasteChrome = safePromptSelection ChromeCmd

firefox_cmd = "firefox"
run_firefox :: X ()
run_firefox = (spawn firefox_cmd)
run_in_firefox uri = (spawn (firefox_cmd ++ " -new-window '" ++ uri ++ "'"))

-- Using Gmail for mail and calendar.
run_mail = (run_in_chrome "https://mail.google.com/mail")
run_calendar = (run_in_chrome "https://www.google.com/calendar")

run_cmd_line = (Prompt.Shell.shellPrompt prompt_config)

file_manager_cmd = "nautilus"
run_file_manager :: X ()
run_file_manager = (spawn file_manager_cmd)

-- musicPlayerCmd = inTerminal "ncmpc"
-- runMusicPlayer = spawn musicPlayerCmd
-- pasteMusicPlayer = promptSelection musicPlayerCmd

-- mixerCmd = inTerminal "alsamixer"
-- runMixer = spawn mixerCmd

restart_xmonad = (broadcastMessage ReleaseResources) >>
                 (restart "xmonad" True)

-- rememberCmd = "/path/to/emacsclient-starter org-protocol:/remember:/t/foo/" -- for adding quick reminders to your agenda
-- runRemember = spawn rememberCmd

editor_cmd :: String
editor_cmd = "emacsclient -nc"

run_editor :: X ()
run_editor = (spawn editor_cmd)

run_editor_here :: X ()
run_editor_here = (spawn (editor_cmd ++ " " ++ "."))


-- Topics and Premade Workspaces
topics_table :: [TopicItem]
topics_table = [
  TopicItem "*scratch*" "s" "~" (run_colour_term >> run_editor),
  TopicItem "terminals" "r" "~" (Monad.replicateM_ 2 run_colour_term),
  TopicItem "browse"    "y" "~" run_chrome,
  TopicItem "irc" "i" "~" run_chat,
  TopicItem "mail" "m" "~" run_mail,
  TopicItem "agenda" "a" "~" ((spawn (editor_cmd ++ " ~/agenda.org")) >>
                              (run_calendar))]

-- Work in progress ...
-- TODO: specify perworkspace layout in _topicsTable
layouts_config =
  Hooks.ManageDocks.avoidStruts
  (Layout.WindowNavigation.configurableNavigation
   Layout.WindowNavigation.noNavigateBorders
   (Layout.BoringWindows.boringWindows
    (Layout.Minimize.minimize
     -- Layout.PerWorkspace.onWorkspace "agenda" layout_tiled2 $
     -- Layout.PerWorkspace.onWorkspace "rtb/main" layout_grid $
     (Layout.Gaps.gaps [(Layout.Gaps.L, 0)]
      (layout_tiled_fixed_2 |||
       layout_tiled2 |||
       (Layout.Renamed.renamed [Layout.Renamed.Replace "Tall2-Limited"]
        (Layout.LimitWindows.limitWindows 5 layout_tiled2)) |||

       (Mirror layout_tiled2) |||

       (L.named "Wide2-Limited"
        (Layout.LimitWindows.limitWindows 5 (Mirror layout_tiled2))) |||
       layout_multicol |||
       layout_tiled3 |||
       layout_tiled3mid |||
       layout_grid |||
       layout_right_paned |||
       Layout.Circle.Circle |||
       (L.named "Circular" L.Circular) |||
       layout_dishes |||
       Layout.Accordion.Accordion |||
       (Layout.NoBorders.noBorders Full))))))

layout_tiled2 = Tall 1 (3/100) (1/2)
layout_tiled3 = Layout.ThreeColumns.ThreeCol 1 (3/100) (1/2)
layout_tiled3mid = Layout.ThreeColumns.ThreeColMid 1 (3/100) (1/2)
layout_grid = L.named "Grid" (Layout.Grid.GridRatio 1.1)
layout_right_paned = (L.named "Isolated Left"
                      (layout_grid ***||** layout_tiled3))
layout_dishes = (L.named "Dishes"
                 (Layout.LimitWindows.limitWindows 5
                  (Layout.Dishes.Dishes 1 (1/5))))
layout_tiled_fixed_2 = (L.named "Fixed"
                        (Layout.FixedColumn.FixedColumn 1 20 80 10))
layout_multicol = (L.named "Multicol"
                   (Layout.MultiColumns.multiCol [1, 1] 3 0.02 0.28))

-- _layoutTable = (("1", "Tall2", _tiled2),
--                 ("S-1", "Tall2-Limited", (Layout.LimitWindows.limitWindows 5 _tiled2)),
--                 ("2", "Wide2", (Mirror _tiled2)),
--                 ("S-2", "Wide2-Limited", (Layout.LimitWindows.limitWindows 5 (Mirror _tiled2))),
--                 ("3", "Tall3", _tiled3),
--                 ("S-3", "Tall3-Mid", _tiled3mid),
--                 ("g", "Grid", (Layout.Grid.GridRatio 1.1)),
--                 ("c", "Circle", Layout.Circle.Circle),
--                 ("a", "Accordion", Layout.Accordion.Accordion),
--                 ("f", "Full", (Layout.NoBorders.noBorders Full)))


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
  showXPrompt Goto = "Go to window: "  -- In any workspace
  showXPrompt Focus = "Focus to window: "  -- In the same workspace
  showXPrompt FocusNonIconified = "Focus to non-iconified window: "
  showXPrompt Bring = "Bring window(s): "
  commandToComplete _ c = c
  nextCompletion _ = Prompt.getNextCompletion

window_prompt :: WindowPrompt -> Prompt.XPConfig -> X ()
window_prompt t c = do
  action <- case t of
    Goto -> fmap gotoAction $ window_map pred
    Focus -> fmap gotoAction $ window_map pred
    FocusNonIconified -> fmap gotoAction $ window_map pred
    Bring -> fmap gotoAction $ window_map pred
  wm <- window_map pred
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
        Focus -> True  -- TODO: Should look at window state (Is in current WS).
        FocusNonIconified -> True -- TODO: Should look at state.
        Bring -> True

-- | A map from window names to Windows.
window_map :: (Window -> Bool) -> X (Map.Map String Window)
window_map pred = do
  ws <- gets windowset
  Map.fromList `fmap` concat `fmap` mapM keyValuePairs (StackSet.workspaces ws)
    where keyValuePairs ws = mapM (keyValuePair ws) $ StackSet.integrate' (StackSet.stack ws)
          keyValuePair ws w = flip (,) w `fmap` decorate_window_name ws w

-- | Returns the window name as will be listed in dmenu.
--   Lowercased, for your convenience (since dmenu is case-sensitive).
--   Tagged with the workspace ID, to guarantee uniqueness, and to let the user
--   know where he's going.
decorate_window_name :: WindowSpace -> Window -> X String
decorate_window_name ws w = do
  name <- fmap (map Char.toLower . show) $ Util.NamedWindows.getName w
  return ("#" ++ StackSet.tag ws ++ " " ++ name)


emacs_keys :: [(String, X())]
emacs_keys  =
  [ -- Applications
    ("M-C-S-r", run_colour_term),
    ("M-C-r", run_colour_screen_term),
    -- ("M-v M-t", pasteTerminal),
    -- ("M-v M-y", pasteChrome),
    -- ("M-h n", manTerminal),
    ("M-C-S-e", (spawn "gedit")),
    ("M-C-e", run_editor),
    ("M-C-S-e", run_editor_here),
    ("M-C-c", run_chrome),
    ("M-C-y", run_firefox),
    ("M-C-u", run_clove_clojure),
    ("M-C-S-u", run_python_interactive),
    ("M-C-o", run_ghci),
    ("M-S-1", run_cmd_line),
    ("M-S-s", (change_dir prompt_config)),
    ("M-C-t", run_file_manager)] ++

  (map (\(TopicItem n k _ _) -> ("M-d " ++ k, (workspace_goto n))) topics_table) ++
  (map (\(TopicItem n k _ _) -> ("M-S-d " ++ k, (workspace_shift n))) topics_table) ++

  [("M-m", (withFocused Layout.Minimize.minimizeWindow)),
   ("M-C-m", (withFocused (\ w -> (sendMessage 
                                   (Layout.Minimize.RestoreMinimizedWin w))))),
   ("M-S-m", (sendMessage Layout.Minimize.RestoreNextMinimizedWin)),

   -- Layouts
   ("M-C-s", refresh),

   -- ("M-e d", (setLayout (XMonad.layoutHook conf))),
   ("M-e n", (sendMessage NextLayout))] ++

  [("M-e f", (sendMessage (JumpToLayout "Full"))),
   ("M-e 1", (sendMessage (JumpToLayout "Tall"))),
   ("M-e S-1", (sendMessage (JumpToLayout "Tall2-Limited"))),
   ("M-e 2", (sendMessage (JumpToLayout "Mirror Tall"))),
   ("M-e S-2", (sendMessage (JumpToLayout "Wide2-Limited"))),
   ("M-e 3", (sendMessage (JumpToLayout "ThreeCol"))),
   ("M-e g", (sendMessage (JumpToLayout "Grid"))),
   ("M-e c", (sendMessage (JumpToLayout "Circle"))),
   ("M-e S-c", (sendMessage (JumpToLayout "Circular"))),
   ("M-e l", (sendMessage (JumpToLayout "Isolated Left"))),
   ("M-e d", (sendMessage (JumpToLayout "Dishes"))),
   ("M-e s", (sendMessage (JumpToLayout "Fixed"))),
   ("M-e m", (sendMessage (JumpToLayout "Multicol"))),

   ("M-e r", (sendMessage (Layout.Gaps.ToggleGaps))),
   ("M-e M-r M-e", (sendMessage (Layout.Gaps.DecGap 200 Layout.Gaps.L))),
   ("M-e M-r M-t", (sendMessage (Layout.Gaps.IncGap 200 Layout.Gaps.L)))
  ] ++

  (let key_dirs = ["<Up>", "<Right>", "<Down>", "<Left>"] ++ ["p", "f", "n", "b"]
       nav_dirs = [Layout.WindowNavigation.U, Layout.WindowNavigation.R,
                   Layout.WindowNavigation.D, Layout.WindowNavigation.L]
       floatsnap_dirs = [Actions.FloatSnap.U, Actions.FloatSnap.R,
                         Actions.FloatSnap.D, Actions.FloatSnap.L]
   in [("M-" ++ a, (sendMessage (Layout.WindowNavigation.Go b)))
      | (a, b) <- zip key_dirs (cycle nav_dirs)] ++
      [("M-S-" ++ a, (sendMessage (Layout.WindowNavigation.Swap b)))
      | (a, b) <- zip key_dirs (cycle nav_dirs)] ++
      [("M-C-" ++ a, (withFocused (Actions.FloatSnap.snapMove b Nothing)))
      | (a, b) <- zip key_dirs (cycle floatsnap_dirs)] ++
      [("M-M1-" ++ a, (withFocused (Actions.FloatSnap.snapGrow b Nothing)))
      | (a, b) <- zip key_dirs (cycle floatsnap_dirs)] ++
      [("M-M1-S-" ++ a, (withFocused (Actions.FloatSnap.snapShrink b Nothing)))
      | (a, b) <- zip key_dirs (cycle floatsnap_dirs)]) ++

  -- ("M-<Tab>", (Layout.BoringWindows.focusDown)),
  -- ("M-S-<Tab>", (Layout.BoringWindows.focusUp)),
  [(k, (Actions.CycleWindows.cycleRecentWindows [xK_Super_L] xK_Tab xK_grave) >>
       (Actions.UpdatePointer.updatePointer (Actions.UpdatePointer.Relative 1 1)))
   | k <- ["M-S-e", "M-<Tab>"]] ++

  [("M-<Return>", (Actions.DwmPromote.dwmpromote)),
   ("M-S-<Return>", (Layout.BoringWindows.focusMaster)),

   ("M-C-1", (screenWorkspace 0) >>= (flip whenJust (windows . StackSet.view))),
   ("M-C-2", (screenWorkspace 1) >>= (flip whenJust (windows . StackSet.view))),
   ("M-C-3", (screenWorkspace 2) >>= flip whenJust (windows . StackSet.view)),
   ("M-C-S-1", (screenWorkspace 0 >>= flip whenJust (windows . StackSet.shift))),
   ("M-C-S-2", (screenWorkspace 1 >>= flip whenJust (windows . StackSet.shift))),
   ("M-C-S-3", (screenWorkspace 2 >>= flip whenJust (windows . StackSet.shift))),

   ("M--", (sendMessage Shrink)),
   ("M-=", (sendMessage Expand)),
   ("M-<F10>", (withFocused (windows . StackSet.sink))),
   ("M-t", (withFocused (windows . StackSet.sink))),
   ("M-M1-t", (Actions.WithAll.withAll' StackSet.sink)),

   ("M-[", (sendMessage (IncMasterN 1))),
   ("M-]", (sendMessage (IncMasterN (-1)))),

   ("M-S-[", (Layout.LimitWindows.increaseLimit)),
   ("M-S-]", (Layout.LimitWindows.decreaseLimit)),


   -- Toggle full screen
   ("M-S-<F10>", (sendMessage Hooks.ManageDocks.ToggleStruts) >>
                 (refresh)),
   ("M-S-t", (sendMessage Hooks.ManageDocks.ToggleStruts) >>
             (refresh)),

   ("M-a", (Prompt.Workspace.workspacePrompt prompt_config workspace_goto)),
   ("M-S-a", (Prompt.Workspace.workspacePrompt prompt_config workspace_shift)),
   ("M-r", (window_prompt FocusNonIconified prompt_config)),
   ("M-S-r", (window_prompt Goto prompt_config)),
   ("M-M1-r", (window_prompt Focus prompt_config)),

   -- ("M-M1-r", (Prompt.Window.windowPromptGoto prompt_config)),
   ("M-M1-S-r", (Prompt.Window.windowPromptBring prompt_config)),

   ("M-q", (kill)),
   ("M-S-q", (Actions.WithAll.killAll))] ++

  [("M-j", (Actions.CycleWS.moveTo
            Actions.CycleWS.Next workspace_tag_groups)),
   ("M-k", (Actions.CycleWS.moveTo
            Actions.CycleWS.Prev workspace_tag_groups)),
   ("M-S-j", (Actions.CycleWS.shiftTo
              Actions.CycleWS.Next workspace_tag_groups)),
   ("M-S-k", (Actions.CycleWS.shiftTo
              Actions.CycleWS.Prev workspace_tag_groups)),

   ("M-.", (Actions.CycleWS.moveTo
            Actions.CycleWS.Next workspace_tag_non_groups)),
   ("M-,", (Actions.CycleWS.moveTo
            Actions.CycleWS.Prev workspace_tag_non_groups)),
   ("M-l", (Actions.CycleWS.moveTo
            Actions.CycleWS.Next Actions.CycleWS.NonEmptyWS)),
   ("M-h", (Actions.CycleWS.moveTo
            Actions.CycleWS.Prev Actions.CycleWS.NonEmptyWS)),
   ("M-S-l", (Actions.CycleWS.shiftTo
              Actions.CycleWS.Next Actions.CycleWS.NonEmptyWS)),
   ("M-S-h", (Actions.CycleWS.shiftTo
              Actions.CycleWS.Prev Actions.CycleWS.NonEmptyWS)),
   ("M-C-l", (Actions.CycleWS.moveTo
              Actions.CycleWS.Next Actions.CycleWS.EmptyWS)),
   ("M-C-h", (Actions.CycleWS.moveTo
              Actions.CycleWS.Prev Actions.CycleWS.EmptyWS)),
   ("M-C-S-l", (Actions.CycleWS.shiftTo
                Actions.CycleWS.Next Actions.CycleWS.EmptyWS)),
   ("M-C-S-h", (Actions.CycleWS.shiftTo
                Actions.CycleWS.Prev Actions.CycleWS.EmptyWS))]
  ++
  [("M-`", Actions.CycleRecentWS.cycleRecentWS [xK_Super_L] xK_grave xK_Tab),

   ("M-S-<Backspace>", (Actions.WithAll.killAll) >>
                       (Actions.DynamicWorkspaces.removeWorkspace)),
   -- Buggy, messes with focus and creates flicker, needs to be fixed.

   ("M-C-a", (Actions.DynamicWorkspaces.renameWorkspace prompt_config)),

   ("M-i 1", (Util.ExtensibleState.put color_theme_dark) >>
             (spawn "xrdb -merge .local/etc/Xresources-dark") >>
             (spawn "emacsclient -e \"(set-theme 'dark-forge)\"")),
   ("M-i 2", (Util.ExtensibleState.put color_theme_light) >>
             (spawn "xrdb -merge .local/etc/Xresources-light") >>
             (spawn "emacsclient -e \"(set-theme 'whitestone-serious)\"")),
   ("M-i 3", (Util.ExtensibleState.put color_theme_grey) >>
             (spawn "xrdb -merge .local/etc/Xresources-dark") >>
             (spawn "emacsclient -e \"(set-theme 'fruitsalad-dark)\"")),

   -- -- Commands
   -- , ("M-y", runCommand _commands)

   -- -- Remember
   -- , ("M-C-f", runRemember)
   -- xmonad
   ("M-C-S-q", (restart_xmonad))]

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
mouse_bindings :: XConfig Layout -> Map.Map (ButtonMask, Button) (Window -> X ())
mouse_bindings (XConfig {XMonad.modMask = modMask}) =
               (Map.fromList
                 ([((modMask, button1),
                    (\w -> (focus w) >> (mouseMoveWindow w))),
                   ((modMask, button3),
                    (\w -> (focus w) >>
                    (Actions.FlexibleResize.mouseResizeWindow w)))]))

-- Application specific window handling
manage_hook =
  (manageHook defaultConfig
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


fairy_config = (defaultConfig
        {borderWidth = 4,
         terminal = term_cmd,
         normalBorderColor = normal_border color_theme,  -- Well, this sucks!
         focusedBorderColor = focused_border color_theme,  -- Same as this.
         workspaces = topics_list,
         layoutHook = layouts_config,
         modMask = mod4Mask,
         logHook = Actions.GroupNavigation.historyHook,
         handleEventHook = Hooks.Minimize.minimizeEventHook,
         mouseBindings = mouse_bindings,
         manageHook = manage_hook})
