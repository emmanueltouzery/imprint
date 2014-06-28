{-# LANGUAGE CPP #-}

#if CABAL_OS_WINDOWS

import Distribution.Simple
main = defaultMain

#else

import Distribution.Simple.I18N.GetText
main = gettextDefaultMain

#endif
