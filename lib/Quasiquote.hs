module Quasiquote (config) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

config :: QuasiQuoter
config =
  QuasiQuoter
    { quoteExp = stringE,
      quotePat = error "Cannot use config quoter in a pattern context",
      quoteType = error "Cannot use config quoter in a type context",
      quoteDec = error "Cannot use config quoter in a declaration context"
    }
