{-# LANGUAGE DeriveDataTypeable #-}

module XMonad.FairyTheme (
  colorTheme,
  Colors (..),
  colorThemeLight,
  colorThemeLightWood,
  colorThemeDark,
  colorThemeGrey,
  font,
) where

import qualified XMonad

colorTheme = colorThemeLight

data Colors = Colors {
  normalBorder :: String,
  termBackground :: [Double],
  focusedBorder :: String,
  promptBg :: String,
  promptFg :: String,
  promptBorder :: String,
  dynamiclogCurrent :: String,
  dynamiclogVisible :: String,
  dynamiclogUrgent :: String
} deriving (XMonad.Typeable, Read, Show)

instance XMonad.ExtensionClass Colors where
  initialValue = colorTheme
  extensionType = XMonad.PersistentExtension

colorThemeLight = Colors {
  normalBorder = "#AAAAAA",
  focusedBorder = "#66BBFF",
  termBackground = [0xEE, 0xFF],
  promptBorder = "#ccc",
  promptBg = "#f8f8f8",
  promptFg = "#333",
  dynamiclogCurrent = "#008800",
  dynamiclogVisible = "#444400",
  dynamiclogUrgent = "#880000" }

colorThemeLightWood = Colors {
  normalBorder = "#b9a99f",
  focusedBorder = "#683718",
  termBackground = [0xEE, 0xE0],
  promptBorder = "#b9a99f",
  promptBg = "#d9c9bf",
  promptFg = "#333",
  dynamiclogCurrent = "#683718",
  dynamiclogVisible = "#444400",
  dynamiclogUrgent = "#880000"
}

colorThemeDark = Colors {
  normalBorder = "#000",
  focusedBorder = "#060", -- "#0088FF"
  termBackground = [0x18, 0x00],
  promptBorder = "#111",
  promptBg = "#000",
  promptFg = "#EEE",
  dynamiclogCurrent = "green",
  dynamiclogVisible = "yellow",
  dynamiclogUrgent = "red"
}

colorThemeGrey = Colors {
  normalBorder = "#000000",
  focusedBorder = "#66BBFF",
  termBackground = [0x30, 0x20],
  promptBorder = "#111",
  promptBg = "#000",
  promptFg = "#EEE",
  dynamiclogCurrent = "green",
  dynamiclogVisible = "yellow",
  dynamiclogUrgent = "red"
}

-- randomBackgroundColors = do
--     case System.Environment.getEnv "THEMETYPE" of
--          Nothing ->  [0x18, 0x00]
--          _ -> return [0xEE, 0xFF]

font =
  "xft:UbuntuMono:pixelsize=16"
  -- "-b&h-lucidatypewriter-medium-r-*-*-12-*-*-*-*-*-*-*"
