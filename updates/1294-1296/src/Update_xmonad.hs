{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Update_xmonad () where

import Control.Applicative
import qualified Data.Map as Map
import Lowarn
import Lowarn.TH
import Lowarn.Transformer
import qualified NextVersion.EntryPoint as NextVersion
import qualified NextVersion.LowarnState as NextVersion
import qualified NextVersion.XMonad as NextVersion
import qualified PreviousVersion.LowarnState as PreviousVersion
import qualified PreviousVersion.XMonad as PreviousVersion

instance
  Transformable
    (PreviousVersion.Layout PreviousVersion.Window)
    (NextVersion.Layout NextVersion.Window)
  where
  transform ::
    PreviousVersion.Layout PreviousVersion.Window ->
    IO (Maybe (NextVersion.Layout NextVersion.Window))
  transform (PreviousVersion.Layout layout) =
    case NextVersion.readsLayout
      (NextVersion.Layout $ NextVersion.layoutHook NextVersion.def)
      (show layout) of
      [(l, "")] -> return $ Just l
      _ -> return Nothing

instance Transformable PreviousVersion.XState NextVersion.XState where
  transform :: PreviousVersion.XState -> IO (Maybe NextVersion.XState)
  transform PreviousVersion.XState {..} = do
    windowset' <- transform windowset
    mapped' <- transform mapped
    waitingUnmap' <- transform waitingUnmap
    let dragging' = Nothing
    numberlockMask' <- transform numberlockMask
    return $
      NextVersion.XState
        <$> windowset'
        <*> mapped'
        <*> waitingUnmap'
        <*> Just dragging'
        <*> numberlockMask'
        <*> return Map.empty

instance
  Transformable
    (PreviousVersion.XConfig PreviousVersion.Layout)
    (NextVersion.XConfig NextVersion.Layout)
  where
  transform ::
    PreviousVersion.XConfig PreviousVersion.Layout ->
    IO (Maybe (NextVersion.XConfig NextVersion.Layout))
  transform PreviousVersion.XConfig {..} = do
    layoutHook' <- transform layoutHook
    let manageHook' = NextVersion.manageHook NextVersion.def
        handleEventHook' = NextVersion.handleEventHook NextVersion.def
    workspaces' <- transform workspaces
    modMask' <- transform modMask
    let keys' = NextVersion.keys NextVersion.def
        mouseBindings' = NextVersion.mouseBindings NextVersion.def
    borderWidth' <- transform borderWidth
    let logHook' = NextVersion.logHook NextVersion.def
        startupHook' = NextVersion.startupHook NextVersion.def
    clientMask' <- transform clientMask
    rootMask' <- transform rootMask
    let handleExtraArgs' = NextVersion.handleExtraArgs NextVersion.def

    return $
      NextVersion.XConfig
        normalBorderColor
        focusedBorderColor
        terminal
        <$> layoutHook'
        <*> Just manageHook'
        <*> Just handleEventHook'
        <*> workspaces'
        <*> modMask'
        <*> Just keys'
        <*> Just mouseBindings'
        <*> borderWidth'
        <*> Just logHook'
        <*> Just startupHook'
        <*> Just focusFollowsMouse
        <*> Just clickJustFocuses
        <*> clientMask'
        <*> rootMask'
        <*> Just handleExtraArgs'

instance Transformable PreviousVersion.XConf NextVersion.XConf where
  transform :: PreviousVersion.XConf -> IO (Maybe NextVersion.XConf)
  transform PreviousVersion.XConf {..} = do
    config' <- transform config
    theRoot' <- transform theRoot
    normalBorder' <- transform normalBorder
    focusedBorder' <- transform focusedBorder
    let keyActions' = liftA2 NextVersion.keys config' config'
        buttonActions' = liftA2 NextVersion.mouseBindings config' config'
    mouseFocused' <- transform mouseFocused
    mousePosition' <- transform mousePosition
    let currentEvent' = Nothing
    dirs' <- NextVersion.getDirs

    return $
      NextVersion.XConf display
        <$> config'
        <*> theRoot'
        <*> normalBorder'
        <*> focusedBorder'
        <*> keyActions'
        <*> buttonActions'
        <*> mouseFocused'
        <*> mousePosition'
        <*> Just currentEvent'
        <*> Just dirs'

update :: Update PreviousVersion.LowarnState NextVersion.LowarnState
update = Update transformer NextVersion.entryPoint

updateExportDeclarations 'update
