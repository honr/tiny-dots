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
import qualified System.Directory

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
-- import qualified XMonad.Layout.GridVariants as Layout.GridVariants
import qualified XMonad.Layout.Grid as Layout.Grid
import qualified XMonad.Layout.LimitWindows as Layout.LimitWindows
import qualified XMonad.Layout.Minimize as Layout.Minimize
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
import qualified XMonad.Util.EZConfig as Util.EZConfig
-- import qualified XMonad.Util.Loggers as Util.Loggers -- unused
import qualified XMonad.Util.Run as Util.Run
import qualified XMonad.Util.Stack as Util.Stack
import qualified XMonad.Util.WindowProperties as Util.WindowProperties
import qualified XMonad.Util.WorkspaceCompare as Util.WorkspaceCompare
import qualified XMonad.Util.XSelection as Util.XSelection


-- Utils:
data TopicItem = TI {topicName :: Actions.TopicSpace.Topic,
                     topicAccel :: String,
                     topicDir :: String,
                     topicAction :: X ()}

_topicConfig = (Actions.TopicSpace.TopicConfig
                 {Actions.TopicSpace.topicDirs = (Map.fromList
                                                  (map
                                                   (\(TI n _ d _) -> (n, d))
                                                   _topicTable)),
                  Actions.TopicSpace.topicActions = (Map.fromList
                                                     (map
                                                      (\(TI n _ _ a) -> (n, a))
                                                      _topicTable)),
                  Actions.TopicSpace.defaultTopicAction = (const (return ())),
                  Actions.TopicSpace.defaultTopic = "*scratch*",
                  Actions.TopicSpace.maxTopicHistory = 128})

_workspaces = (map (\(TI n _ _ _) -> n) _topicTable)

changeDir c = (Prompt.Directory.directoryPrompt
               c
               "Directory: "
               (liftIO . System.Directory.setCurrentDirectory))

-- Creates the workspace if needed.
goto :: Actions.TopicSpace.Topic -> X ()
goto t = (newWorkspace t) >> (Actions.TopicSpace.switchTopic _topicConfig t)

shift :: Actions.TopicSpace.Topic -> X ()
shift t = (newWorkspace t) >> ((windows . StackSet.shift) t)


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

widExist :: WorkspaceId -> X Bool
widExist wid = do xs <- get
                  return (widExists wid (windowset xs))

widExists :: WorkspaceId -> StackSet.StackSet WorkspaceId l a s sd -> Bool
widExists wid ws = (elem wid (map StackSet.tag  (StackSet.workspaces ws)))

-- isWorkspace sc w = w `elem` map StackSet.tag (StackSet.current w : StackSet.visible w)

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
    , Hooks.DynamicLog.ppCurrent  = (pangoColor (dynamiclog_current _colors)) . Hooks.DynamicLog.wrap "[" "]" . pangoSanitize
    , Hooks.DynamicLog.ppVisible  = (pangoColor (dynamiclog_visible _colors)) . Hooks.DynamicLog.wrap "(" ")" . pangoSanitize
    , Hooks.DynamicLog.ppHidden   = const ""
    , Hooks.DynamicLog.ppUrgent   = (pangoColor (dynamiclog_urgent _colors))
    , Hooks.DynamicLog.ppLayout   = (Hooks.DynamicLog.wrap "<span weight='normal'>" "</span>") . pangoSanitize -- const ""
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
    left  = "<span foreground='" ++ fg ++ "'>"
    right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs


-- Themes
_colors = _colors_dark

data Colors = Colors { normal_border :: String,
                       term_background :: [Double],
                       focused_border :: String,
                       dynamiclog_current :: String,
                       dynamiclog_visible :: String,
                       dynamiclog_urgent :: String }

_colors_light = Colors { normal_border = "#AAAAAA",
                         focused_border = "#0066CC",
                         term_background = [0xEE, 0xFF],
                         dynamiclog_current = "#008800",
                         dynamiclog_visible = "#444400",
                         dynamiclog_urgent = "#880000" }

_colors_dark = Colors { normal_border = "#000000",
                        focused_border = "#0088FF",
                        term_background = [0x18, 0x00],  -- [0x18, 0x00] [0xEE, 0xFF] [0x44, 0x00]
                        dynamiclog_current = "green",
                        dynamiclog_visible = "yellow",
                        dynamiclog_urgent = "red" }

-- _randomBackgroundColors = do
--     case System.Environment.getEnv "THEMETYPE" of
--          Nothing ->  [0x18, 0x00]
--          _ -> return [0xEE, 0xFF]

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
               Prompt.searchPredicate = List.isInfixOf})


-- Applications
terminalCmd = "xterm"
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
runGhci = (runInColourTerminal "ghci")

chromeCmd = "chromium"
runChrome = (spawn chromeCmd)
 -- TODO: This is unreliable; properly escape or use execve.
runInChrome uri = (spawn (chromeCmd ++ " --new-window '" ++ uri ++ "'"))
-- pasteChrome = safePromptSelection ChromeCmd

firefoxCmd = "firefox"
runFirefox = (spawn firefoxCmd)
runInFirefox uri = (spawn (firefoxCmd ++ " -new-window '" ++ uri ++ "'"))

-- Using Gmail for mail and calendar.
runMail = (runInChrome "https://mail.google.com/mail")
runCalendar = (runInChrome "https://www.google.com/calendar")

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
               -- ("M-v M-y", pasteChrome),
               -- ("M-h n", manTerminal),
               ("M-C-S-e", (spawn "gedit")),
               ("M-C-e", runEditor),
               ("M-C-S-e", runEditorHere),
               ("M-C-c", runChrome),
               ("M-C-y", runFirefox),
               ("M-C-u", runCloveClojure),
               ("M-C-o", runGhci),
               ("M-S-1", runCmdLine),
               ("M-S-s",   (changeDir _XPConfig)),
               ("M-C-t", runFileManager)] ++

              (map (\(TI n k _ _) -> ("M-d " ++ k, (goto n))) _topicTable) ++
              (map (\(TI n k _ _) -> ("M-S-d " ++ k, (shift n))) _topicTable) ++

              [("M-m", (withFocused Layout.Minimize.minimizeWindow)),
               ("M-S-m", (sendMessage Layout.Minimize.RestoreNextMinimizedWin)),

               -- Layouts
               ("M-C-s", refresh),

               ("M-e d", (setLayout (XMonad.layoutHook conf))),
               ("M-e n", (sendMessage NextLayout))] ++

              [("M-e f", (sendMessage (JumpToLayout "Full"))),
               ("M-e 1", (sendMessage (JumpToLayout "Tall"))),
               ("M-e S-1", (sendMessage (JumpToLayout "Tall2-Limited"))),
               ("M-e 2", (sendMessage (JumpToLayout "Mirror Tall"))),
               ("M-e S-2", (sendMessage (JumpToLayout "Wide2-Limited"))),
               ("M-e 3", (sendMessage (JumpToLayout "ThreeCol"))),
               ("M-e g", (sendMessage (JumpToLayout "Grid"))),
               ("M-e c", (sendMessage (JumpToLayout "Circle")))] ++

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

               ("M-a", (Prompt.Workspace.workspacePrompt _XPConfig goto)),
               ("M-S-a", (Prompt.Workspace.workspacePrompt _XPConfig shift)),
               ("M-r", (Prompt.Window.windowPromptGoto _XPConfig)),
               ("M-S-r", (Prompt.Window.windowPromptBring _XPConfig)),

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

               ("M-C-a", (Actions.DynamicWorkspaces.renameWorkspace _XPConfig)),

              -- -- Commands
              -- , ("M-y", runCommand _commands)

              -- -- Remember
              -- , ("M-C-f", runRemember)
              -- xmonad
               ("M-C-S-q", (restartXMonad))]

-- Mouse bindings
_mouseBindings :: XConfig Layout -> Map.Map (ButtonMask, Button) (Window -> X ())
_mouseBindings (XConfig {XMonad.modMask = modMask}) =
               (Map.fromList
                 ([((modMask, button1),
                    (\w -> (focus w) >> (mouseMoveWindow w))),
                   ((modMask, button3),
                    (\w -> (focus w) >>
                    (Actions.FlexibleResize.mouseResizeWindow w)))]))


-- Application specific window handling
_manageHook = manageHook defaultConfig
            <+> composeAll
            [Hooks.ManageHelpers.isFullscreen --> Hooks.ManageHelpers.doFullFloat,
             appName =? "Dialog" --> Hooks.ManageHelpers.doCenterFloat,
             ((className =? "Emacs24") <||> (className =? "Emacs")) --> unfloat,
             className =? "Thunderbird" --> unfloat,
             (((className =? "Firefox") <||> (className =? "Nightly")) <&&> 
              (roleName =? "browser")) --> unfloat,
             ((className =? "Chromium") <||>
              (className =? "Chromium-browser")) --> unfloat,
            -- checkDock --> doIgnore,
             ((className =? "Inkscape") <&&>
              (fmap ("- Inkscape" `List.isSuffixOf`) title)) --> unfloat]
            -- <+> composeOne [
            --       transience,
            --       className =? "Firefox" -?> doF (StackSet.shift "browse")]
            -- <+> Hooks.Place.placeHook Hooks.Place.simpleSmart
	    <+> Hooks.ManageHelpers.doCenterFloat -- Float by default, at center.
	    -- <+> doFloat -- Float by default.
            <+> Hooks.ManageDocks.manageDocks
    where unfloat = ask >>= (doF . StackSet.sink)
          roleName = stringProperty "WM_WINDOW_ROLE"


-- Topics and Premade Workspaces
_topicTable :: [TopicItem]
_topicTable = [
  TI "*scratch*" "s" "~" (runColourTerminal >> runEditor),
  TI "terminals" "r" "~" (Monad.replicateM_ 2 runColourTerminal),
  TI "browse"    "y" "~" runChrome,
  TI "irc" "i" "~" runChat,
  TI "mail" "m" "~" runMail,
  TI "agenda" "a" "~/Documents" ((spawn (editorCmd ++ " ~/Documents/agenda.org")) >>
                                            (runCalendar))
  ]


-- Work in progress ...
-- TODO: specify perworkspace layout in _topicsTable
_layout = Hooks.ManageDocks.avoidStruts
          (Layout.WindowNavigation.configurableNavigation
           Layout.WindowNavigation.noNavigateBorders
           (Layout.BoringWindows.boringWindows
            (Layout.Minimize.minimize
             -- Layout.PerWorkspace.onWorkspace "agenda" _layout_tiled2 $
             -- Layout.PerWorkspace.onWorkspace "rtb/main" _layout_grid $
             (_layout_tiled2 |||
              (Layout.Renamed.renamed [Layout.Renamed.Replace "Tall2-Limited"]
               (Layout.LimitWindows.limitWindows 5 _layout_tiled2)) |||

              (Mirror _layout_tiled2) |||

              (Layout.Renamed.renamed [Layout.Renamed.Replace "Wide2-Limited"]
               (Layout.LimitWindows.limitWindows 5 (Mirror _layout_tiled2))) |||
              _layout_tiled3 |||
              _layout_tiled3mid |||
              _layout_grid |||
              _layout_right_paned |||
              Layout.Circle.Circle |||
              Layout.Accordion.Accordion |||
              (Layout.NoBorders.noBorders Full)))))

_layout_tiled2 = Tall 1 (3/100) (1/2)
_layout_tiled3 = Layout.ThreeColumns.ThreeCol 1 (3/100) (1/2)
_layout_tiled3mid = Layout.ThreeColumns.ThreeColMid 1 (3/100) (1/2)
_layout_grid = (Layout.Renamed.renamed [Layout.Renamed.Replace "Grid"]
                (Layout.Grid.GridRatio 1.1))
_layout_right_paned = (Layout.Renamed.renamed [Layout.Renamed.Replace "Right Paned"]
                 (_layout_grid ***||** _layout_tiled3))


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


-- Main.
main :: IO ()
main = do
  home <- System.Directory.getHomeDirectory
  -- xmproc <- (Util.Run.spawnPipe
  --            (home ++ "/bin/xmobar" ++ -- path to xmobar
  --             " " ++
  --             home ++ "/.xmonad/xmobar.hs"))
  dbus <- DBus.Client.Simple.connectSession
  getWellKnownName dbus
  Actions.TopicSpace.checkTopicConfig _workspaces _topicConfig
  liftIO (System.Directory.setCurrentDirectory home)
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
