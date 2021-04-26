{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.LSP.Types.CallHierarchy where

import Data.Aeson
import Data.Aeson.TH
import Language.LSP.Types.Common
import Language.LSP.Types.DocumentSymbol
import Language.LSP.Types.Location
import Language.LSP.Types.Progress
import Language.LSP.Types.StaticRegistrationOptions
import Language.LSP.Types.TextDocument
import Language.LSP.Types.Uri
import Language.LSP.Types.Utils

data CallHierarchyClientCapabilities = CallHierarchyClientCapabilities
  { -- | Whether implementation supports dynamic registration. If this is set to
    -- `true` the client supports the new `(TextDocumentRegistrationOptions &
    -- StaticRegistrationOptions)` return value for the corresponding server
    -- capability as well.
    --
    -- @since 3.16.0
    _dynamicRegistration :: Maybe Bool
  }
  deriving (Show, Read, Eq)

deriveJSON lspOptions ''CallHierarchyClientCapabilities

-- -------------------------------------

makeExtendingDatatype "CallHierarchyOptions" [''WorkDoneProgressOptions] []
deriveJSON lspOptions ''CallHierarchyOptions

makeExtendingDatatype
  "CallHierarchyRegistrationOptions"
  [ ''TextDocumentRegistrationOptions,
    ''CallHierarchyOptions,
    ''StaticRegistrationOptions
  ]
  []
deriveJSON lspOptions ''CallHierarchyRegistrationOptions

-- -------------------------------------

makeExtendingDatatype
  "CallHierarchyPrepareParams"
  [''TextDocumentPositionParams, ''WorkDoneProgressParams]
  []
deriveJSON lspOptions ''CallHierarchyPrepareParams

-- -------------------------------------

data CallHierarchyItem = CallHierarchyItem
  { -- | The name of this item.
    _name :: String,
    -- | The kind of this item.
    _kind :: SymbolKind,
    -- | Tags for this item.
    _tags :: Maybe (List SymbolTag),
    -- | More detail for this item, e.g. the signature of a function.
    _detail :: Maybe String,
    -- | The resource identifier of this item.
    _uri :: Uri,
    -- | The range enclosing this symbol not including leading/trailing whitespace
    -- but everything else, e.g. comments and code.
    _range :: Range,
    -- | The range that should be selected and revealed when this symbol is being
    -- picked, e.g. the name of a function. Must be contained by the
    -- `CallHierarchyItem._range`
    _selectionRange :: Range,
    -- | A data entry field that is preserved between a call hierarchy prepare and
    -- incoming calls or outgoing calls requests.
    _xdata :: Maybe Value
  }
  deriving (Read, Show, Eq)

deriveJSON lspOptions ''CallHierarchyItem

-- -------------------------------------

makeExtendingDatatype
  "CallHierarchyIncomingCallsParams"
  [''WorkDoneProgressParams, ''PartialResultParams]
  [("_item", [t|CallHierarchyItem|])]
deriveJSON lspOptions ''CallHierarchyIncomingCallsParams

-- -------------------------------------

data CallHierarchyIncomingCall = CallHierarchyIncomingCall
  { -- | The item that makes the call.
    _from :: CallHierarchyItem,
    -- | The ranges at which the calls appear. This is relative to the caller
    -- denoted by the `from` field.
    _fromRanges :: List Range
  }
  deriving (Read, Show, Eq)

deriveJSON lspOptions ''CallHierarchyIncomingCall
