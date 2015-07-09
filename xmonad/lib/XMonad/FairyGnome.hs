{-# LANGUAGE OverloadedStrings #-}

module XMonad.FairyGnome
       (gnomeRegister,
        prettyPrinter,
        getWellKnownName) where

-- XMonad:
import qualified XMonad
import qualified System.Environment
import qualified DBus
import qualified DBus.Client
import qualified Codec.Binary.UTF8.String as UTF8
import qualified XMonad.Hooks.DynamicLog as Hooks.DynamicLog
import qualified XMonad.Util.Run as Util.Run

import XMonad.FairyTheme

gnomeRegister :: XMonad.MonadIO m => m ()
gnomeRegister =
  (XMonad.io
    (do
       x <- (fmap (lookup "DESKTOP_AUTOSTART_ID")
                  System.Environment.getEnvironment)
       XMonad.whenJust x (\ sessionId ->
                           (Util.Run.safeSpawn "dbus-send"
                            ["--session",
                             "--print-reply=string",
                             "--dest=org.gnome.SessionManager",
                             "/org/gnome/SessionManager",
                             "org.gnome.SessionManager.RegisterClient",
                             "string:xmonad",
                             "string:" ++ sessionId]))))


prettyPrinter :: DBus.Client.Client -> Hooks.DynamicLog.PP
prettyPrinter dbus = Hooks.DynamicLog.defaultPP {
  Hooks.DynamicLog.ppOutput   = dbusOutput dbus,
  Hooks.DynamicLog.ppTitle    = pangoSanitize,
  Hooks.DynamicLog.ppCurrent  = (pangoColor (dynamiclogCurrent colorTheme)) .
                                (Hooks.DynamicLog.wrap "[" "]") .
                                pangoSanitize,
  Hooks.DynamicLog.ppVisible  = (pangoColor (dynamiclogVisible colorTheme)) .
                                (Hooks.DynamicLog.wrap "(" ")") .
                                pangoSanitize,
  Hooks.DynamicLog.ppHidden   = const "",
  Hooks.DynamicLog.ppUrgent   = pangoColor (dynamiclogUrgent colorTheme),
  Hooks.DynamicLog.ppLayout   = (Hooks.DynamicLog.wrap "<span weight='normal'>" "</span>") .
                                pangoSanitize, -- const ""
  Hooks.DynamicLog.ppSep      = " ┆ "
  }

getWellKnownName :: DBus.Client.Client -> IO ()
getWellKnownName dbus = do
  DBus.Client.requestName dbus (DBus.busName_ "org.xmonad.Log")
                [DBus.Client.nameAllowReplacement,
                 DBus.Client.nameReplaceExisting,
                 DBus.Client.nameDoNotQueue]
  return ()

dbusOutput :: DBus.Client.Client -> String -> IO ()
dbusOutput dbus str =
  DBus.Client.emit dbus
  (DBus.signal
   "/org/xmonad/Log"
   "org.xmonad.Log"
   "Update") {DBus.signalBody =
                 [DBus.toVariant ("<b>" ++ (UTF8.decodeString str) ++ "</b>")]}

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
