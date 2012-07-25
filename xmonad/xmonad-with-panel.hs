{-# LANGUAGE OverloadedStrings #-}

-- XMonad:
import XMonad hiding ( (|||) )
import XMonad.Layout.LayoutCombinators
-- import qualified XMonad.Operations as Op

import qualified XMonad.StackSet as StackSet

-- Haskell:
import qualified Control.Monad as Monad
-- import qualified Data.Either.Utils
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid
import qualified System.Environment
import qualified System.Exit
import qualified System.IO

import qualified DBus.Client.Simple
import qualified Codec.Binary.UTF8.String as UTF8

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
import qualified XMonad.Layout.LimitWindows as Layout.LimitWindows
import qualified XMonad.Layout.Minimize as Layout.Minimize
import qualified XMonad.Layout.NoBorders as Layout.NoBorders
import qualified XMonad.Layout.PerWorkspace as Layout.PerWorkspace
-- import qualified XMonad.Layout.SimpleFloat as Layout.SimpleFloat -- unused
-- import qualified XMonad.Layout.Tabbed as Layout.Tabbed -- unused
-- import qualified XMonad.Layout.Spacing as Layout.Spacing -- unused
import qualified XMonad.Layout.ThreeColumns as Layout.ThreeColumns
import qualified XMonad.Layout.WindowNavigation as Layout.WindowNavigation
import qualified XMonad.Layout.WorkspaceDir as Layout.WorkspaceDir
import qualified XMonad.Prompt as Prompt
-- import qualified XMonad.Prompt.Input as Prompt.Input -- unused
import qualified XMonad.Prompt.Shell as Prompt.Shell
-- import qualified XMonad.Prompt.Ssh as Prompt.Ssh -- unused
import qualified XMonad.Prompt.Window as Prompt.Window
import qualified XMonad.Prompt.Workspace as Prompt.Workspace
import qualified XMonad.Util.EZConfig as Util.EZConfig
-- import qualified XMonad.Util.Loggers as Util.Loggers -- unused
import qualified XMonad.Util.Run as Util.Run
import qualified XMonad.Util.Stack as Util.Stack
import qualified XMonad.Util.WindowProperties as Util.WindowProperties
import qualified XMonad.Util.WorkspaceCompare as Util.WorkspaceCompare
import qualified XMonad.Util.XSelection as Util.XSelection


main :: IO ()
main = do
  dbus <- DBus.Client.Simple.connectSession
  getWellKnownName dbus
  (xmonad
    (Hooks.EwmhDesktops.ewmh
      (defaultConfig
        {borderWidth = 4,
         terminal = terminalCmd,
         normalBorderColor = normal_border _colors,
         focusedBorderColor = focused_border _colors,
         workspaces = _workspaces,
         layoutHook = _layout,
         keys = _keys,
         modMask = mod4Mask,
         mouseBindings = _mouseBindings,
         logHook = Hooks.DynamicLog.dynamicLogWithPP (prettyPrinter dbus) >>
                   Actions.GroupNavigation.historyHook,
         handleEventHook = Hooks.EwmhDesktops.ewmhDesktopsEventHook >>
                           Hooks.Minimize.minimizeEventHook,
         manageHook = _manageHook,
         startupHook = gnomeRegister >> startupHook defaultConfig})))

-- Workspaces
_topicConfig = (Actions.TopicSpace.defaultTopicConfig
                 {Actions.TopicSpace.topicDirs = _topicDirs,
                  Actions.TopicSpace.topicActions = _topicActions,
                  Actions.TopicSpace.defaultTopicAction = (const (return ())),
                  Actions.TopicSpace.defaultTopic = "*scratch*",
                  Actions.TopicSpace.maxTopicHistory = 128})

_workspaces = ["*scratch*",
               "browse"]

_topicDirs = (Map.fromList $
              [("*scratch*", "~"),
               ("browse", "~"),
               ("irc", "~"),
               ("mail", "~/Mail"),
               ("agenda","~/Documents"),
               ("terminals", "~/src")])

_topicActions = (Map.fromList
                  ([("terminals", Monad.replicateM_ 2 runColourTerminal),
                    ("*scratch*", do
                       runColourTerminal
                       runEditor),
                    ("browse", runBrowser),
                    ("irc", runChat),
                    ("mail", runMail),
                    ("agenda", (spawn "emacsclient -nc ~/Documents/agenda.org") >>
                               (runCalendar))]))

-- Creates the workspace if needed.
goto :: Actions.TopicSpace.Topic -> X ()
goto t = (newWorkspace t) >> (Actions.TopicSpace.switchTopic _topicConfig t)

shift :: Actions.TopicSpace.Topic -> X ()
shift t = (newWorkspace t) >> ((windows . StackSet.shift) t)

-- Themes
_colors = _colors_light

data Colors = Colors { normal_border :: String,
                       term_background :: [Double],
                       focused_border :: String }

_colors_light = Colors { normal_border = "#000000",
                         focused_border = "#0066CC",
                         term_background = [0xEE, 0xFF] }

_colors_dark = Colors { normal_border = "#000000",
                        focused_border = "#0088FF",
                        term_background = [0x18, 0x00] } -- [0x18, 0x00] [0xEE, 0xFF] [0x44, 0x00]

-- _randomBackgroundColors = do
--     case System.Environment.getEnv "THEMETYPE" of
--          Nothing ->  [0x18, 0x00]
--          _ -> return [0xEE, 0xFF]

-- Applications
terminalCmd = "uxterm"
runTerminal :: X()
runTerminal = (spawn terminalCmd)
runColourTerminal = (Actions.RandomBackground.randomBg
                      (Actions.RandomBackground.HSV x y))
                     where
                        [x, y] = term_background _colors

runInColourTerminal cmd = do
    c <- (Actions.RandomBackground.randomBg'
           (Actions.RandomBackground.HSV x y))
    (spawn (terminalCmd ++ " -bg " ++ c ++ " -e " ++ cmd))
    where
        [x, y] = term_background _colors

runColourScreenTerminal = (runInColourTerminal "screen -xR")

-- inTerminal cmd = (terminalCmd ++ " -e " ++ cmd)

-- saveSession cmd = "/bin/bash -c '" ++ cmd ++ "; /bin/bash'"
-- runInTerminal f = transformPromptSelection f (terminalCmd ++ " -e ")
-- pasteTerminal = runInTerminal saveSession
-- manTerminal = runInTerminal manPage

runChat = (runInColourTerminal "ssh -t personal-server emacsclient -t")

runCloveClojure = (runInColourTerminal "clove -i clojure")

browserCmd = "chromium"
runBrowser = (spawn browserCmd)
 -- TODO: This is unreliable; properly escape or use execve.
runInBrowser uri = (spawn (browserCmd ++ " --new-window '" ++ uri ++ "'"))
-- pasteBrowser = safePromptSelection browserCmd

-- Using Gmail for mail and calendar.
runMail = (runInBrowser "https://mail.google.com/mail")
runCalendar = (runInBrowser "https://www.google.com/calendar")

runCmdLine = (Prompt.Shell.shellPrompt _XPConfig)

fileManagerCmd = "nautilus"
runFileManager = (spawn fileManagerCmd)

-- musicPlayerCmd = inTerminal "ncmpc"
-- runMusicPlayer = spawn musicPlayerCmd
-- pasteMusicPlayer = promptSelection musicPlayerCmd

-- mixerCmd = inTerminal "alsamixer"
-- runMixer = spawn mixerCmd

restartXMonad = (broadcastMessage ReleaseResources) >>
                (restart "xmonad" True)

-- rememberCmd = "/path/to/emacsclient-starter org-protocol:/remember:/t/foo/" -- for adding quick reminders to your agenda
-- runRemember = spawn rememberCmd

editorCmd :: String
editorCmd = "emacsclient -nc"

runEditor :: X ()
runEditor = (spawn editorCmd)

runEditorHere :: X ()
runEditorHere = (spawn (editorCmd ++ " " ++ "."))

-- Keys
_keys :: XConfig Layout -> Map.Map (KeyMask, KeySym) (X())
_keys = \conf -> (Util.EZConfig.mkKeymap conf (_emacsKeys conf))

_emacsKeys :: XConfig Layout -> [(String, X())]
_emacsKeys  = \conf ->
              [ -- Applications
               ("M-C-S-r", runColourTerminal),
               ("M-C-r", runColourScreenTerminal),
               -- ("M-v M-t", pasteTerminal),
               -- ("M-v M-y", pasteBrowser),
               -- ("M-h n", manTerminal),
               ("M-C-S-e", (spawn "gedit")),
               ("M-C-e", runEditor),
               ("M-e", runEditorHere),
               ("M-C-y", runBrowser),
               ("M-C-u", runCloveClojure),
               ("M-S-1", runCmdLine),
               ("M-C-d", (Layout.WorkspaceDir.changeDir _XPConfig)),
               ("M-C-t", runFileManager)] ++

              [("M-d a", (goto "agenda")),
               ("M-d i", (goto "irc")),
               ("M-d m", (goto "mail")),
               ("M-d r", (goto "terminals")),
               ("M-d s", (goto "*scratch*")),
               ("M-d y", (goto "browse"))] ++

              [("M-m", (withFocused Layout.Minimize.minimizeWindow)),
               ("M-S-m", (sendMessage Layout.Minimize.RestoreNextMinimizedWin)),

               -- Layouts
               ("M-r", refresh),

               ("M-S-<Space>", (setLayout (XMonad.layoutHook conf))),
               ("M-<Space>", (sendMessage NextLayout)),
               ("M-<F1>", (sendMessage (JumpToLayout "Full"))),
               ("M-<F2>", (sendMessage (JumpToLayout "Tall"))),
               ("M-<F3>", (sendMessage (JumpToLayout "Mirror Tall"))),
               ("M-<F4>", (sendMessage (JumpToLayout "ThreeCol")))] ++

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
              [
               -- ("M-<Tab>", (Layout.BoringWindows.focusDown)),
               -- ("M-S-<Tab>", (Layout.BoringWindows.focusUp)),
               ("M-<Tab>", (Actions.CycleWindows.cycleRecentWindows [xK_Super_L] xK_Tab xK_grave) >>
                           (Actions.UpdatePointer.updatePointer (Actions.UpdatePointer.Relative 1 1))),

               ("M-<Return>", (Actions.DwmPromote.dwmpromote)),
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

               ("M-s", (Prompt.Workspace.workspacePrompt _XPConfig goto)),
               ("M-S-s", (Prompt.Workspace.workspacePrompt _XPConfig shift)),
               ("M-a", (Prompt.Window.windowPromptGoto _XPConfig)),
               ("M-S-a", (Prompt.Window.windowPromptBring _XPConfig)),

               ("M-q", (kill)),
               ("M-S-q", (Actions.WithAll.killAll))] ++

              [("M-j", (Actions.CycleWS.moveTo
                         Actions.CycleWS.Next _WSTagGroups)),
               ("M-k", (Actions.CycleWS.moveTo
                         Actions.CycleWS.Prev _WSTagGroups)),
               ("M-S-j", (Actions.CycleWS.shiftTo
                           Actions.CycleWS.Next _WSTagGroups)),
               ("M-S-k", (Actions.CycleWS.shiftTo
                           Actions.CycleWS.Prev _WSTagGroups)),

               ("M-.", (Actions.CycleWS.moveTo
                         Actions.CycleWS.Next _WSTagNonGroups)),
               ("M-,", (Actions.CycleWS.moveTo
                         Actions.CycleWS.Prev _WSTagNonGroups)),
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

               ("M-S-r", (Actions.DynamicWorkspaces.renameWorkspace _XPConfig)),

              -- -- Commands
              -- , ("M-y", runCommand _commands)

              -- -- Remember
              -- , ("M-C-f", runRemember)
              -- xmonad
               ("M-C-S-q", (restartXMonad))]

_WSTagSep = '/'
_WSTagGroups = (Actions.CycleWS.WSTagGroup _WSTagSep)
_WSTagNonGroups = (Actions.CycleWS.WSIs
                    (do cur <- (fmap
                                 (groupName . StackSet.workspace . StackSet.current)
                                 (gets windowset))
                        return ((\ x -> (cur /= x)) . groupName)))
                  where groupName = (takeWhile (/= _WSTagSep) . StackSet.tag)

newWorkspace :: WorkspaceId -> X ()
newWorkspace w = do exists <- widExist w
                    if (not exists)
                    then (Actions.DynamicWorkspaces.addHiddenWorkspace w)
                    else return ()

newWorkspaceDir :: WorkspaceId -> X ()
newWorkspaceDir w = do exists <- widExist w
                       if (not exists)
                           then do (Actions.DynamicWorkspaces.addHiddenWorkspace w)
                                   (goto w)
                                   (Layout.WorkspaceDir.changeDir _XPConfig)
                           else return ()

widExist :: WorkspaceId -> X Bool
widExist wid = do xs <- get
                  return (widExists wid (windowset xs))

widExists :: WorkspaceId -> StackSet.StackSet WorkspaceId l a s sd -> Bool
widExists wid ws = (elem wid (map StackSet.tag  (StackSet.workspaces ws)))

-- isWorkspace sc w = w `elem` map StackSet.tag (StackSet.current w : StackSet.visible w)

-- Mouse bindings
_mouseBindings :: XConfig Layout -> Map.Map (ButtonMask, Button) (Window -> X ())
_mouseBindings (XConfig {XMonad.modMask = modMask}) =
               (Map.fromList
                 ([((modMask, button1),
                    (\w -> (focus w) >> (mouseMoveWindow w))),
                   ((modMask, button3),
                    (\w -> (focus w) >>
                    (Actions.FlexibleResize.mouseResizeWindow w)))]))

-- Work in progress ...
_layout = Hooks.ManageDocks.avoidStruts
          -- $ _onWorkspace "agenda" _full
          (Layout.WindowNavigation.configurableNavigation
            Layout.WindowNavigation.noNavigateBorders
            (Layout.WorkspaceDir.workspaceDir "~"
              (Layout.BoringWindows.boringWindows
                (Layout.Minimize.minimize
                  (_tiled2 |||
                   (Layout.LimitWindows.limitWindows 5 _tiled2) |||
                   (Mirror _tiled2) |||
                   (Layout.LimitWindows.limitWindows 5 (Mirror _tiled2)) |||
                   _tiled3 |||
                   _tiled3mid |||
                   Layout.Circle.Circle |||
                   Layout.Accordion.Accordion |||
                   (Layout.NoBorders.noBorders Full))))))

_tiled2 = Tall 1 (3/100) (1/2)
_tiled3 = Layout.ThreeColumns.ThreeCol 1 (3/100) (1/2)
_tiled3mid = Layout.ThreeColumns.ThreeColMid 1 (3/100) (1/2)

-- _onWorkspace t l = (Layout.PerWorkspace.onWorkspace
--                       t
--                       (Layout.WorkspaceDir.workspaceDir (_folderOf t) l))

-- _folderOf = (Maybe.fromMaybe "~" . flip Map.lookup _topicDirs)

-- Theme
myFont               = "xft:Ubuntu Mono:pixelsize=16"
myFontScalable       = "xft:Ubuntu:pixelsize=14"

-- Prompt theme
_XPConfig = (Prompt.defaultXPConfig
              {Prompt.font = myFont,
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
               Prompt.searchPredicate = List.isInfixOf})

_manageHook = manageHook defaultConfig
            <+> composeAll
            [Hooks.ManageHelpers.isFullscreen --> Hooks.ManageHelpers.doFullFloat,
             appName =? "Dialog" --> Hooks.ManageHelpers.doCenterFloat,
             className =? "Emacs" --> unfloat,
             className =? "Thunderbird" --> unfloat,
             className =? "Firefox" --> unfloat,
             className =? "Chromium" --> unfloat,
            -- checkDock --> doIgnore,
             className =? "Inkscape" --> unfloat]
            -- <+> composeOne [
            --       transience,
            --       className =? "Firefox" -?> doF (StackSet.shift "browse")]
            -- <+> Hooks.Place.placeHook Hooks.Place.simpleSmart
	    <+> Hooks.ManageHelpers.doCenterFloat -- Float by default, at center.
	    -- <+> doFloat -- Float by default.
            <+> Hooks.ManageDocks.manageDocks
    where unfloat = ask >>= (doF . StackSet.sink)

gnomeRegister :: MonadIO m => m ()
gnomeRegister =
  (io
    (do
       x <- (fmap (lookup "DESKTOP_AUTOSTART_ID")
                  System.Environment.getEnvironment)
       whenJust x (\ sessionId ->
                     (Util.Run.safeSpawn "dbus-send"
                       ["--session",
                        "--print-reply=string",
                        "--dest=org.gnome.SessionManager",
                        "/org/gnome/SessionManager",
                        "org.gnome.SessionManager.RegisterClient",
                        "string:xmonad",
                        "string:" ++ sessionId]))))

prettyPrinter :: DBus.Client.Simple.Client -> Hooks.DynamicLog.PP
prettyPrinter dbus = Hooks.DynamicLog.defaultPP
    { Hooks.DynamicLog.ppOutput   = dbusOutput dbus
    , Hooks.DynamicLog.ppTitle    = pangoSanitize
    , Hooks.DynamicLog.ppCurrent  = pangoColor "green" . Hooks.DynamicLog.wrap "[" "]" . pangoSanitize
    , Hooks.DynamicLog.ppVisible  = pangoColor "yellow" . Hooks.DynamicLog.wrap "(" ")" . pangoSanitize
    , Hooks.DynamicLog.ppHidden   = const ""
    , Hooks.DynamicLog.ppUrgent   = pangoColor "red"
    , Hooks.DynamicLog.ppLayout   = const ""
    , Hooks.DynamicLog.ppSep      = " ┆ "
    }

getWellKnownName :: DBus.Client.Simple.Client -> IO ()
getWellKnownName dbus = do
  DBus.Client.Simple.requestName dbus (DBus.Client.Simple.busName_ "org.xmonad.Log")
                [DBus.Client.Simple.AllowReplacement,
                 DBus.Client.Simple.ReplaceExisting,
                 DBus.Client.Simple.DoNotQueue]
  return ()

dbusOutput :: DBus.Client.Simple.Client -> String -> IO ()
dbusOutput dbus str = DBus.Client.Simple.emit dbus
                             "/org/xmonad/Log"
                             "org.xmonad.Log"
                             "Update"
                             [DBus.Client.Simple.toVariant ("<b>" ++ (UTF8.decodeString str) ++ "</b>")]

pangoColor :: String -> String -> String
pangoColor fg = Hooks.DynamicLog.wrap left right
  where
    left  = "<span foreground=\"" ++ fg ++ "\">"
    right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs
