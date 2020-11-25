import XMonad
import Data.Monoid
import System.Exit
import Graphics.X11.ExtraTypes.XF86
import XMonad.Hooks.DynamicLog
import XMonad.Layout.MultiColumns
import XMonad.Util.Run
import XMonad.Layout.Grid
import XMonad.Util.Cursor
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Util.NamedScratchpad
import XMonad.Layout.IndependentScreens
import XMonad.Actions.CycleWS
import XMonad.ManageHook
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Actions.UpdatePointer

myTerminal      = "terminator"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth   = 2
myModMask       = mod4Mask

myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

myNormalBorderColor  = "#232323"
myFocusedBorderColor = "#EF9A51"

scratchpads =  [
  NS "term"  (myTerminal ++ " --title term") (title =? "term") (customFloating $ W.RationalRect (1/20) (1/20) (9/10) (9/10)),
  NS "pavucontrol" "pavucontrol" (className =? "Pavucontrol")(customFloating $ W.RationalRect (1/20) (1/20) (9/10) (9/10)),
  NS "nautilus" "nautilus" (className =? "Org.gnome.Nautilus")(customFloating $ W.RationalRect (1/20) (1/20) (9/10) (9/10))
  ]

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "rofi -font 'Source Code Pro 9' -show run")
    , ((modm,               xK_Tab     ), spawn "rofi -font 'Source Code Pro 9' -modi 'window' -show window")
    , ((modm .|. shiftMask,   xK_p     ), spawn "bgl collection")
    , ((modm .|. shiftMask,   xK_s     ), spawn "bgl saved")
    , ((modm .|. controlMask,   xK_p     ), spawn "bagel -password")
    , ((modm,               xK_s     ), spawn "bagel -foo")
    , ((modm,               xK_grave     ), spawn "bagel -sumon")
    , ((modm,               xK_f     ), spawn "flameshot gui")
    , ((modm,               xK_e     ), spawn "mtoggle -t")
    , ((modm .|. controlMask, xK_Return), namedScratchpadAction scratchpads "term")
    , ((modm, xK_m), namedScratchpadAction scratchpads "pavucontrol")
    , ((modm, xK_q), namedScratchpadAction scratchpads "nautilus")

    -- close focused window
    , ((modm, xK_w     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm,               xK_period ), nextScreen)
    , ((modm .|. shiftMask,   xK_period ), sequence_ [shiftNextScreen, nextScreen])

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), spawn "mtoggle -n")
    , ((modm .|. shiftMask, xK_n     ), spawn "mtoggle -p")

    -- Move focus to the next window

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_semicolon     ), windows W.focusUp  )

    -- Move focus to the master window
    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    -- Swap the focused window with the previous window
    , ((modm, xK_r     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm .|. shiftMask,          xK_j     ), sendMessage Shrink)
    , ((modm .|. shiftMask,          xK_semicolon     ), sendMessage Expand)
    -- Expand the master area

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_i ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_d), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm .|. shiftMask, xK_r     ), spawn "xmonad --recompile; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    -- foo meh
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    -- [((m .|. modm, k), windows $ onCurrentScreen f i)
    --     | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_Z, xK_x, xK_y] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts(tiled ||| Full ||| Grid ||| Mirror tiled ||| multiCol [1] 1 0.01 (-0.5))
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
  spawn "/usr/local/bin/stupy"
  setDefaultCursor xC_left_ptr


------------------------------------------------------------------------
xmobarTitleColor = "#EF9A51"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#EF9A51"
main = do
  xmproc0 <- spawnPipe "xmobar -x 0 ~/.xmo"
  xmproc1 <- spawnPipe "xmobar -x 1 ~/.xmo"
  xmonad $ docks
         $ ewmh
         $ defaults {
         manageHook = namedScratchpadManageHook scratchpads
         , logHook = dynamicLogWithPP xmobarPP {
                  ppCurrent = xmobarColor xmobarCurrentWorkspaceColor "" . wrap "[" "]"
                , ppTitle = xmobarColor xmobarTitleColor "" . shorten 50
                , ppSep = "   "
                , ppOutput = \x -> hPutStrLn xmproc0 x  >> hPutStrLn xmproc1 x
         } >> updatePointer (0.5, 0.5) (0, 0)
      }

defaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
