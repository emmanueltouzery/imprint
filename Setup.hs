{-# LANGUAGE CPP #-}

import System.Process (rawSystem)

#if CABAL_OS_WINDOWS

import Distribution.Simple
main = defaultMain

#else

import Distribution.Simple.I18N.GetText
main = do
    -- manage translations
    gettextDefaultMain
    -- install desktop file and icon
    rawSystem "xdg-icon-resource" ["install", "--size", "128", "imprint-128.png", "graphics-imprint"]
    rawSystem "xdg-desktop-menu" ["install", "graphics-imprint.desktop"]
    return ()


#endif
