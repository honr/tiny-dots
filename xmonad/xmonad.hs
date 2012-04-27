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

-- Contrib:
-- import qualified XMonad.Actions.Commands as Actions.Commands -- unused
import qualified XMonad.Actions.CycleWS as Actions.CycleWS
-- import qualified XMonad.Actions.CycleWindows as Actions.CycleWindows -- unused
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
-- import qualified XMonad.Actions.UpdatePointer as Actions.UpdatePointer -- unused
import qualified XMonad.Actions.WithAll as Actions.WithAll
-- import qualified XMonad.Hooks.DynamicLog as Hooks.DynamicLog -- unused
import qualified XMonad.Hooks.EwmhDesktops as Hooks.EwmhDesktops
import qualified XMonad.Hooks.ManageDocks as Hooks.ManageDocks
import qualified XMonad.Hooks.ManageHelpers as Hooks.ManageHelpers
import qualified XMonad.Hooks.Minimize as Hooks.Minimize
import qualified XMonad.Hooks.Place as Hooks.Place
import qualified XMonad.Layout.Accordion as Layout.Accordion
-- import qualified XMonad.Layout.BoringWindows as Layout.BoringWindows -- unused
import qualified XMonad.Layout.Circle as Layout.Circle
import qualified XMonad.Layout.Minimize as Layout.Minimize
import qualified XMonad.Layout.NoBorders as Layout.NoBorders
import qualified XMonad.Layout.PerWorkspace as Layout.PerWorkspace
-- import qualified XMonad.Layout.SimpleFloat as Layout.SimpleFloat -- unused
-- import qualified XMonad.Layout.Tabbed as Layout.Tabbed -- unused
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
  (xmonad
    (Hooks.EwmhDesktops.ewmh
      (defaultConfig
        {borderWidth = 2,
         terminal = terminalCmd,
         normalBorderColor = _normalBorderColor,
         focusedBorderColor = _focusedBorderColor,
         workspaces = _workspaces,
         layoutHook = _layout,
         keys = _keys,
         modMask = mod4Mask,
         mouseBindings = _mouseBindings,
         logHook = Actions.GroupNavigation.historyHook,
         handleEventHook = Hooks.EwmhDesktops.ewmhDesktopsEventHook >>
                           Hooks.Minimize.minimizeEventHook,
         manageHook = _manageHook,
         startupHook = gnomeRegister >> startupHook defaultConfig})))

-- Workspaces
_spaces = (Map.fromList
            ([("*scratch*", "~"),
              ("browse", "~"),
              ("irc", "~"),
              ("mail", "~/Mail"),
              ("agenda","~/Documents"),
              ("terminals", "~")]))

_workspaces = ["*scratch*",
               "browse"]

_topicConfig = (Actions.TopicSpace.TopicConfig
                 {Actions.TopicSpace.topicDirs = _spaces,
                  Actions.TopicSpace.topicActions = _topicActions,
                  Actions.TopicSpace.defaultTopicAction = (const (return ())),
                  Actions.TopicSpace.defaultTopic = "*scratch*",
                  Actions.TopicSpace.maxTopicHistory = 10})

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
_normalBorderColor :: String
_normalBorderColor = "#000000"
_focusedBorderColor :: String
_focusedBorderColor = "#0066CC"

-- Applications
terminalCmd = "xterm"
runTerminal :: X()
runTerminal = (spawn terminalCmd)
runColourTerminal = (Actions.RandomBackground.randomBg
                      (Actions.RandomBackground.HSV 0x18 0x00))
                      --                            0xCC 0xFF
                      --                            0x44 0x00
runColourScreenTerminal = do
    c <- (Actions.RandomBackground.randomBg'
           (Actions.RandomBackground.HSV 0x18 0x00))
    (spawn (terminalCmd ++ " -bg " ++ c ++ " -e screen -xR"))

inTerminal cmd = (terminalCmd ++ " -e " ++ cmd)

-- saveSession cmd = "/bin/bash -c '" ++ cmd ++ "; /bin/bash'"
-- runInTerminal f = transformPromptSelection f (terminalCmd ++ " -e ")
-- pasteTerminal = runInTerminal saveSession
-- manTerminal = runInTerminal manPage

chatCmd = (inTerminal ircCmd)
ircCmd = "ssh -t personal-server emacsclient -t"
runChat = (spawn chatCmd)

runClrl = (spawn (inTerminal "clove -i clojure"))


browserCmd = "browser1"
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
               ("M-C-y", runBrowser),
               ("M-C-u", runClrl),
               ("M-S-1", runCmdLine),
               ("M-C-d", (Layout.WorkspaceDir.changeDir _XPConfig)),
               ("M-C-t", runFileManager),

               ("M-d a", (goto "agenda")),
               ("M-d i", (goto "irc")),
               ("M-d m", (goto "mail")),
               ("M-d r", (goto "terminals")),
               ("M-d s", (goto "*scratch*")),
               ("M-d y", (goto "browse")),

               ("M-m", (withFocused Layout.Minimize.minimizeWindow)),
               ("M-S-m", (sendMessage Layout.Minimize.RestoreNextMinimizedWin)),

               -- Layouts
               ("M-r", refresh),

               ("M-S-<Space>", (setLayout (XMonad.layoutHook conf))),
               ("M-<Space>", (sendMessage NextLayout)),
               ("M-<F1>", (sendMessage (JumpToLayout "Full"))),
               ("M-<F2>", (sendMessage (JumpToLayout "Minimize Tall"))),
               ("M-<F3>", (sendMessage (JumpToLayout "Mirror Minimize Tall"))),

               ("M-<Up>", (sendMessage (Layout.WindowNavigation.Go
                                         Layout.WindowNavigation.U))),
               ("M-<Right>", (sendMessage (Layout.WindowNavigation.Go
                                            Layout.WindowNavigation.R))),
               ("M-<Down>", (sendMessage (Layout.WindowNavigation.Go
                                           Layout.WindowNavigation.D))),
               ("M-<Left>", (sendMessage (Layout.WindowNavigation.Go
                                           Layout.WindowNavigation.L))),

               ("M-p", (sendMessage (Layout.WindowNavigation.Go
                                      Layout.WindowNavigation.U))),
               ("M-f", (sendMessage (Layout.WindowNavigation.Go
                                      Layout.WindowNavigation.R))),
               ("M-n", (sendMessage (Layout.WindowNavigation.Go
                                      Layout.WindowNavigation.D))),
               ("M-b", (sendMessage (Layout.WindowNavigation.Go
                                      Layout.WindowNavigation.L))),

               ("M-C-<Up>", (withFocused (Actions.FloatSnap.snapMove
                                           Actions.FloatSnap.U Nothing))),
               ("M-C-<Right>", (withFocused (Actions.FloatSnap.snapMove
                                              Actions.FloatSnap.R Nothing))),
               ("M-C-<Down>", (withFocused (Actions.FloatSnap.snapMove
                                             Actions.FloatSnap.D Nothing))),
               ("M-C-<Left>", (withFocused (Actions.FloatSnap.snapMove
                                             Actions.FloatSnap.L Nothing))),

               ("M-M1-<Up>", (withFocused (Actions.FloatSnap.snapGrow
                                            Actions.FloatSnap.U Nothing))),
               ("M-M1-<Right>", (withFocused (Actions.FloatSnap.snapGrow
                                               Actions.FloatSnap.R Nothing))),
               ("M-M1-<Down>", (withFocused (Actions.FloatSnap.snapGrow
                                              Actions.FloatSnap.D Nothing))),
               ("M-M1-<Left>", (withFocused (Actions.FloatSnap.snapGrow
                                              Actions.FloatSnap.L Nothing))),

               ("M-M1-S-<Up>", (withFocused (Actions.FloatSnap.snapShrink
                                             Actions.FloatSnap.U Nothing))),
               ("M-M1-S-<Right>", (withFocused (Actions.FloatSnap.snapShrink
                                                Actions.FloatSnap.R Nothing))),
               ("M-M1-S-<Down>", (withFocused (Actions.FloatSnap.snapShrink
                                               Actions.FloatSnap.D Nothing))),
               ("M-M1-S-<Left>", (withFocused (Actions.FloatSnap.snapShrink
                                               Actions.FloatSnap.L Nothing))),

               ("M-S-<Up>", (sendMessage (Layout.WindowNavigation.Swap
                                           Layout.WindowNavigation.U))),
               ("M-S-<Right>", (sendMessage (Layout.WindowNavigation.Swap
                                              Layout.WindowNavigation.R))),
               ("M-S-<Down>", (sendMessage (Layout.WindowNavigation.Swap
                                             Layout.WindowNavigation.D))),
               ("M-S-<Left>", (sendMessage (Layout.WindowNavigation.Swap
                                             Layout.WindowNavigation.L))),

               ("M-S-p", (sendMessage (Layout.WindowNavigation.Swap
                                        Layout.WindowNavigation.U))),
               ("M-S-f", (sendMessage (Layout.WindowNavigation.Swap
                                        Layout.WindowNavigation.R))),
               ("M-S-n", (sendMessage (Layout.WindowNavigation.Swap
                                        Layout.WindowNavigation.D))),
               ("M-S-b", (sendMessage (Layout.WindowNavigation.Swap
                                        Layout.WindowNavigation.L))),

               ("M-<Tab>", (windows StackSet.focusDown)),
               ("M-S-<Tab>", (windows StackSet.focusUp)),
               ("M-<Return>", (Actions.DwmPromote.dwmpromote)),
               ("M-S-<Return>", (windows StackSet.focusMaster)),

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
               ("M-S-q", (Actions.WithAll.killAll)),

               ("M-j", (Actions.CycleWS.moveTo
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
                             Actions.CycleWS.Prev Actions.CycleWS.EmptyWS)),

               -- Toggle between current and previous
               ("M-`", (Actions.CycleWS.toggleWS)),

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
          (Layout.WindowNavigation.windowNavigation
            (Layout.WorkspaceDir.workspaceDir "~" 
              (_tiled |||
              (Mirror _tiled) |||
              Layout.Circle.Circle |||
              Layout.Accordion.Accordion |||
              _full)))

_tiled = (Layout.Minimize.minimize (Tall nmaster delta ratio))
         where
           nmaster = 1
           delta   = 3/100
           ratio   = 1/2

_full = (Layout.NoBorders.noBorders Full)

_onWorkspace t l = (Layout.PerWorkspace.onWorkspace
                      t
                      (Layout.WorkspaceDir.workspaceDir (_folderOf t) l))

_folderOf = (Maybe.fromMaybe "~" . flip Map.lookup _spaces)

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
             className =? "Browser1" --> unfloat,
             className =? "Browser2" --> unfloat,
             className =? "Browser3" --> unfloat,
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
       whenJust x (\sessionId -> 
                    (Util.Run.safeSpawn "dbus-send"
                      ["--session",
                       "--print-reply=string",
                       "--dest=org.gnome.SessionManager",
                       "/org/gnome/SessionManager",
                       "org.gnome.SessionManager.RegisterClient",
                       "string:xmonad",
                       "string:" ++ sessionId]))))
