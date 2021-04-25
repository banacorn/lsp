{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.LSP.Types.CallHierarchy where

import Data.Aeson.TH
import Language.LSP.Types.Progress
import Language.LSP.Types.TextDocument
import Language.LSP.Types.Utils
import Language.LSP.Types.StaticRegistrationOptions

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
