import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.EZConfig
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Magnifier
import XMonad.Util.Loggers (logTitles)
import XMonad.Layout.Spacing
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad.Layout.Dwindle
import System.Exit (exitSuccess)

main :: IO ()
main = xmonad 
    . ewmhFullscreen 
    . ewmh
    . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) toggleStrutsKey
    $ myConf

toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
toggleStrutsKey XConfig{ modMask = m } = (m, xK_b)

myConf = def
    { terminal     = myTerm
    , modMask      = myMod
    , layoutHook   = spacingWithEdge 10 $ myLayout
    , borderWidth  = myBorderWidth
    , focusFollowsMouse = False
    }
  `additionalKeysP`
    [ ("M-<Return>", spawn "alacritty")
    , ("M-d", spawn "rofi -show drun -run-shell-command 'alacritty -e bash -ic \"{cmd} && read\"'")
    , ("M-s", spawn "rofi -show ssh")
    , ("M-w", spawn "rofi -show window")
    , ("M-S-q", kill)
    , ("M-c", spawn "xmonad --recompile && xmonad --restart")
    , ("M-S-l", sendMessage NextLayout)
    , ("M-S-f", withFocused toggleFloat)
    , ("M-S-e", io exitSuccess)
    ]
  `removeKeysP`
    [ "M-q" ]

myTerm :: String
myTerm = "alacritty"

myMod :: KeyMask
myMod = mod4Mask

myBorderWidth :: Dimension 
myBorderWidth = 0

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol ||| magnified ||| Spiral R CCW 1.618 1.1
  where
    magnified = magnifiercz' 1.5 (Tall nmaster delta (1/2))
    threeCol = ThreeColMid nmaster delta (1/3)
    tiled = Tall nmaster delta (1/2)
    nmaster = 1
    delta = 3/100

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarColor "#ff79c6" "#333333" 
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

toggleFloat :: Window -> X ()
toggleFloat w =
  windows 
    (\s ->
        if M.member w (W.floating s)
          then W.sink w s
          else W.float w (W.RationalRect (1 / 3) (1 / 4) (1 / 2) (1 / 2)) s
          )
    
    

