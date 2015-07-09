{-# LANGUAGE OverloadedStrings #-}

module XMonad.FairyGnome
       (gnome_register,
        pretty_printer,
        get_well_known_name) where

-- XMonad:
import qualified XMonad
import qualified System.Environment
import qualified DBus
import qualified DBus.Client
import qualified Codec.Binary.UTF8.String as UTF8
import qualified XMonad.Hooks.DynamicLog as Hooks.DynamicLog
import qualified XMonad.Util.Run as Util.Run

import XMonad.FairyTheme

gnome_register :: XMonad.MonadIO m => m ()
gnome_register =
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


pretty_printer :: DBus.Client.Client -> Hooks.DynamicLog.PP
pretty_printer dbus = Hooks.DynamicLog.defaultPP
    { Hooks.DynamicLog.ppOutput   = dbus_output dbus
    , Hooks.DynamicLog.ppTitle    = pango_sanitize
    , Hooks.DynamicLog.ppCurrent  = ((pango_color (dynamiclog_current color_theme)) .
                                     Hooks.DynamicLog.wrap "[" "]" .
                                     pango_sanitize)
    , Hooks.DynamicLog.ppVisible  = ((pango_color (dynamiclog_visible color_theme)) .
                                     Hooks.DynamicLog.wrap "(" ")" .
                                     pango_sanitize)
    , Hooks.DynamicLog.ppHidden   = const ""
    , Hooks.DynamicLog.ppUrgent   = (pango_color (dynamiclog_urgent color_theme))
    , Hooks.DynamicLog.ppLayout   = ((Hooks.DynamicLog.wrap "<span weight='normal'>" "</span>") . 
                                     pango_sanitize) -- const ""
    , Hooks.DynamicLog.ppSep      = " ┆ "}

get_well_known_name :: DBus.Client.Client -> IO ()
get_well_known_name dbus = do
  DBus.Client.requestName dbus (DBus.busName_ "org.xmonad.Log")
                [DBus.Client.nameAllowReplacement,
                 DBus.Client.nameReplaceExisting,
                 DBus.Client.nameDoNotQueue]
  return ()

dbus_output :: DBus.Client.Client -> String -> IO ()
dbus_output dbus str =
  DBus.Client.emit dbus
  (DBus.signal
   "/org/xmonad/Log"
   "org.xmonad.Log"
   "Update") {DBus.signalBody =
                 [DBus.toVariant ("<b>" ++ (UTF8.decodeString str) ++ "</b>")]}

pango_color :: String -> String -> String
pango_color fg = Hooks.DynamicLog.wrap left right
  where
    left  = "<span foreground='" ++ fg ++ "'>"
    right = "</span>"

pango_sanitize :: String -> String
pango_sanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs
