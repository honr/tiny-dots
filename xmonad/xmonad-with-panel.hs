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

key_bindings :: XConfig Layout -> Map.Map (KeyMask, KeySym) (X())
key_bindings = \conf ->
  (Util.EZConfig.mkKeymap conf
   (emacs_keys ++
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
  get_well_known_name dbus
  check_topics
  liftIO (System.Directory.setCurrentDirectory home)
  (xmonad
    (Hooks.EwmhDesktops.ewmh
      (fairy_config
        {keys = key_bindings,
         logHook = Hooks.DynamicLog.dynamicLogWithPP (pretty_printer dbus) >>
                   logHook fairy_config,
         handleEventHook = Hooks.EwmhDesktops.ewmhDesktopsEventHook >>
                           handleEventHook fairy_config,
         startupHook = gnome_register >> startupHook fairy_config})))
