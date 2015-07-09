import XMonad
import qualified Data.Map as Map
import qualified System.Directory

import qualified DBus.Client
import qualified XMonad.Hooks.DynamicLog as Hooks.DynamicLog
import qualified XMonad.Hooks.EwmhDesktops as Hooks.EwmhDesktops
import qualified XMonad.Util.EZConfig as Util.EZConfig

import XMonad.Fairy
import XMonad.FairyGnome
import XMonad.FairyTheme


import qualified XMonad.Util.XSelection as Util.XSelection

keyBindings :: XConfig Layout -> Map.Map (KeyMask, KeySym) (X())
keyBindings = \conf ->
  (Util.EZConfig.mkKeymap conf
   (emacsKeys ++
    [("M-e d", (setLayout (XMonad.layoutHook conf))),
     ("M-C-S-c", (spawn "chrome-tmp")),
     ("M-y w", Util.XSelection.transformSafePromptSelection
               (\s -> ("'http://en.wikipedia.org/wiki/Special:Search?search=" ++ s ++ "'"))
               "firefox")]))

main :: IO ()
main = do
  home <- System.Directory.getHomeDirectory
  -- xmproc <- (Util.Run.spawnPipe
  --            (home ++ "/bin/xmobar" ++ -- path to xmobar
  --             " " ++
  --             home ++ "/.xmonad/xmobar.hs"))
  dbus <- DBus.Client.connectSession
  getWellKnownName dbus
  checkTopics
  liftIO (System.Directory.setCurrentDirectory home)
  (xmonad
    (Hooks.EwmhDesktops.ewmh
      (fairyConfig
        {keys = keyBindings,
         logHook = Hooks.DynamicLog.dynamicLogWithPP (prettyPrinter dbus) >>
                   logHook fairyConfig,
         handleEventHook = Hooks.EwmhDesktops.ewmhDesktopsEventHook >>
                           handleEventHook fairyConfig,
         startupHook = gnomeRegister >> startupHook fairyConfig})))
