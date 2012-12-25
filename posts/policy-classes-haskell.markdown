---
date: 2011-09-05
title: Policy classes in Haskell
author: Chris Done
tags: haskell, cpp
---

Policy classes parametrize a data type or class over polymorphic types
(“policies”), allowing behaviour of the resulting composed class or
function to be chosen entirely by the type.

[Here is a canonical example](/code/cpp/policy-classes-languages.cpp)
of policy classes in C++.

[Here it is in Haskell](/code/hs/policy-classes-languages.hs) and [here
is another way.](/code/hs/policy-classes-languages2.hs) But these are
more direct translations. A [more idiomatic approach, similar to the first is here.](/code/hs/policy-classes-languages-idiomatic.hs)
