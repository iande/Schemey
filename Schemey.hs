module Schemey
  (
  ) where

import Schemey.Env
import Schemey.Repl

{--
  Initially, this was pretty much a verbatim copy of
    http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours
  
  I wrote the code, but it's spelled out on that site (with the exception of
  dropping one of the old `apply` patterns.)
  
  Now, we're going to do some things that aren't officially Scheme, just to
  see what happens.
  
    - Partial application of functions.  Let's pretend Scheme is more like
      Haskell.
--}
