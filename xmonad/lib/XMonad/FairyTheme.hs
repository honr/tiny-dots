{-# LANGUAGE DeriveDataTypeable #-}

module XMonad.FairyTheme
       (color_theme,
        Colors (..),
        color_theme_light,
        color_theme_light_wood,
        color_theme_dark,
        color_theme_grey,
        font_regular,
        font_scalable
       ) where


import qualified XMonad

color_theme = color_theme_light_wood

data Colors = Colors { normal_border :: String,
                       term_background :: [Double],
                       focused_border :: String,
                       dynamiclog_current :: String,
                       dynamiclog_visible :: String,
                       dynamiclog_urgent :: String }
              deriving (XMonad.Typeable, Read, Show)

instance XMonad.ExtensionClass Colors where
  initialValue = color_theme
  extensionType = XMonad.PersistentExtension

color_theme_light = Colors { normal_border = "#AAAAAA",
                             focused_border = "#66BBFF",
                             term_background = [0xEE, 0xFF],
                             dynamiclog_current = "#008800",
                             dynamiclog_visible = "#444400",
                             dynamiclog_urgent = "#880000" }

color_theme_light_wood = Colors { normal_border = "#b9a99f",
                                  focused_border = "#683718",
                                  term_background = [0xEE, 0xE0],
                                  dynamiclog_current = "#683718",
                                  dynamiclog_visible = "#444400",
                                  dynamiclog_urgent = "#880000" }

color_theme_dark = Colors { normal_border = "#000000",
                            focused_border = "#66BBFF", -- "#0088FF",
                            term_background = [0x18, 0x00],
                            dynamiclog_current = "green",
                            dynamiclog_visible = "yellow",
                            dynamiclog_urgent = "red" }

color_theme_grey = Colors { normal_border = "#000000",
                            focused_border = "#66BBFF",
                            term_background = [0x30, 0x20],
                            dynamiclog_current = "green",
                            dynamiclog_visible = "yellow",
                            dynamiclog_urgent = "red" }

-- _randomBackgroundColors = do
--     case System.Environment.getEnv "THEMETYPE" of
--          Nothing ->  [0x18, 0x00]
--          _ -> return [0xEE, 0xFF]

font_regular = "xft:UbuntuMono:pixelsize=16"
font_scalable = "xft:Ubuntu:pixelsize=14"

-- font_regular = "-b&h-lucidatypewriter-medium-r-*-*-12-*-*-*-*-*-*-*"
-- font_scalable = "-b&h-lucidatypewriter-medium-r-*-*-12-*-*-*-*-*-*-*"
