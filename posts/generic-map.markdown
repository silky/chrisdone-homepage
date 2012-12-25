---
date: 2011-12-05
title: A map generic upon the value thanks to typeable
description: A map generic upon the value thanks to typeable
author: Chris Done
tags: haskell
---

Not sure why I never tried this before.

    {-# LANGUAGE ExistentialQuantification #-}
    {-# LANGUAGE DeriveDataTypeable #-}

    import Data.Typeable
    import qualified Data.Map as M
    import Data.Map (Map)

    data Person = Person Integer String
      deriving (Typeable,Show)

    data Value = forall v. Typeable v => Value v

    demo = do
      print (glookup "foo" map :: Maybe ())
      print (glookup "bar" map :: Maybe Char)
      print (glookup "chris" map :: Maybe Person)

        where map = M.insert "bar" (Value 'a') $
                    M.insert "foo" (Value ())  $
                    M.insert "chris" (Value (Person 123 "Chris Done"))  $
                    M.empty

    glookup :: (Typeable a,Ord key) => key -> Map key Value -> Maybe a
    glookup key map =
      case M.lookup key map of
        Nothing -> Nothing
        Just (Value x) -> (cast x)

    -- λ> demo
    -- Just ()
    -- Just 'a'
    -- Just (Person 123 "Chris Done")
