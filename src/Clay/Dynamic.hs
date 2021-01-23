{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , OverloadedStrings
  , NoImplicitPrelude
  #-}

-- | Dynamic user interface element control. This CSS3 functionality is still
-- in draft, though it is implemented in several browsers. See
-- <http://www.w3.org/TR/2000/WD-css3-userint-20000216#dynamic> and your target
-- browsers' vendor documentation for more information.

module Clay.Dynamic
(
  -- * User input
  UserInput
, userInput
, inputEnabled, inputDisabled

  -- * User modifiability
, UserModify
, userModify
, readOnly, readWrite, writeOnly

  -- * User selection
, UserSelect
, userSelect
, selectText, selectToggle, selectElement, selectElements

  -- * User focus
, UserFocus
, userFocus
, selectAll, selectBefore, selectAfter, selectSame, selectMenu
)
where

import Clay.Common
import Clay.Property
import Clay.Stylesheet
import Data.Monoid hiding (All)

--------------------------------------------------------------------------------
-- Enabling user interface elements: the 'user-input' property

-- | Enabling user interface elements.

userInput :: Style m => UserInput -> m ()
userInput = prefixed (browsers <> "user-input")

-- | Selection mode.

newtype UserInput = UserInput Value
  deriving (Val, GlobalValues, None)

-- | Selection mode.

inputEnabled, inputDisabled :: UserInput

inputEnabled  = UserInput "enabled"
inputDisabled = UserInput "disabled"

--------------------------------------------------------------------------------
-- Modifiability of an element: the 'user-modify' property

-- | Modifiability of an element.

userModify :: Style m => UserModify -> m ()
userModify = prefixed (browsers <> "user-modify")

-- | Selection mode.

newtype UserModify = UserModify Value
  deriving (Val, GlobalValues)

-- | Selection mode.

readOnly, readWrite, writeOnly :: UserModify

readOnly  = UserModify "readonly"
readWrite = UserModify "read-write"
writeOnly = UserModify "write-only"

--------------------------------------------------------------------------------
-- Content selection granularity: the 'user-select' property

-- | Content selection granularity.

userSelect :: Style m => UserSelect -> m ()
userSelect = prefixed (browsers <> "user-select")

-- | Selection mode.

newtype UserSelect = UserSelect Value
  deriving (Val, GlobalValues, None, All)

-- | Selection mode.

selectText, selectToggle, selectElement, selectElements :: UserSelect

selectText     = UserSelect "text"
selectToggle   = UserSelect "toggle"
selectElement  = UserSelect "element"
selectElements = UserSelect "elements"

--------------------------------------------------------------------------------
-- Focus selection behavior of the contents of an element: the 'user-focus' property

-- | Content focusing granularity.

userFocus :: Style m => UserFocus -> m ()
userFocus = prefixed (browsers <> "user-focus")

-- | Focus behaviour.

newtype UserFocus = UserFocus Value
  deriving (Val, GlobalValues, None, Normal, Auto)

-- | Focus mode.

selectAll, selectBefore, selectAfter, selectSame, selectMenu :: UserFocus

selectAll    = UserFocus "select-all"
selectBefore = UserFocus "select-before"
selectAfter  = UserFocus "select-after"
selectSame   = UserFocus "select-same"
selectMenu   = UserFocus "select-menu"
