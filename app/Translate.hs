-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Translate where

import Common
import qualified Data.Map as Map

translate :: String -> Int -> Map.Map String Int -> Syntax -> Term
translate mod_name index renames t =
  let tr = translate mod_name in
  case t of
    LambdaSyntax p param body ->
      let body2 = tr (index + 1) (Map.insert param index renames) body in
      Lambda p param body2
    IdentSyntax p ('#':name) -> Ident p 0 $ '#':name
    IdentSyntax p name ->
      case Map.lookup name renames of
        Just i -> Ident p (index - i - 1) name
        Nothing ->
          let t2 = AccessSyntax p (IdentSyntax p mod_name) name in
          tr index renames t2
    AppSyntax p foo bar ->
      let foo2 = tr index renames foo in
      let bar2 = tr index renames bar in
      App p foo2 bar2
    IntSyntax p i -> Int p i
    LetForceSyntax p ident val scope ->
      let val2 = tr index renames val in
      let scope2 = tr (index + 1) (Map.insert ident index renames) scope in
      LetForce p ident val2 scope2
    ObjectSyntax p mb_name methods ->
      let methods2 = foldr (\(self, method, def) so_far->
              let def2 = tr (index + 1) (Map.insert self index renames) def in
              Map.insert method (self, def2) so_far
            ) Map.empty methods in
      Object p mb_name methods2
    ModuleSyntax p name methods ->
      let methods2 =
            foldr (\(self, method, def) so_far->
              let def2 = translate self (index + 1)
                    (Map.insert self index renames) def in
              Map.insert method (self, def2) so_far
            ) Map.empty methods in
      Object p (Just $ '$':name) methods2
    AccessSyntax p object method ->
      let object2 = tr index renames object in
      Access p object2 method
    UpdateSyntax p object self method def ->
      let object2 = tr index renames object in
      let def2 = tr (index + 1)
            (Map.insert self index renames) def in
      Update p object2 self method def2
    OperatorSyntax p lhs op rhs ->
      let lhs2 = tr index renames lhs in
      let rhs2 = tr index renames rhs in
      Operator p lhs2 op rhs2
    StringSyntax p s -> String p s
    FloatSyntax p f -> Float p f
