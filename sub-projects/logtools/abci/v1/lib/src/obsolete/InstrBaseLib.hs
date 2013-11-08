-- Add more information to this module depending on external library usage

{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances #-}
module InstrBaseLib where

import Instr
import Data.Word


--
-- Built-in types
--

type T'any = Any
t'any :: Type Any
t'any = TypeAny

type T'int = Int
t'int :: Type Int
t'int = TypeInt

t'int'obj :: Type Int
t'int'obj = TypeObject "int"

type T'uint = Word32
t'uint :: Type Word32
t'uint = TypeUInt

t'uint'obj :: Type Word32
t'uint'obj = TypeObject "uint"

type T'double = Double
t'double :: Type Double
t'double = TypeDouble

t'double'obj :: Type Double
t'double'obj = TypeObject "double"

type T'float = Double
t'float :: Type Double
t'float = TypeDouble

t'float'obj :: Type Double
t'float'obj = TypeObject "float"

type T'Boolean = Bool
t'Boolean :: Type Bool
t'Boolean = TypeBool
instance IsSuper Bool Bool
instance IsSuper Base Bool

t'Boolean'obj :: Type Bool
t'Boolean'obj = TypeObject "Boolean"

type T'String = String
t'String :: Type String
t'String = TypeString
instance IsSuper String String
instance IsSuper Base String

t'String'obj :: Type String
t'String'obj = TypeObject "String"

type T'Base = Base
t'Base :: Type Base
t'Base = TypeBase
instance IsSuper Base Base

t'Base'obj :: Type Base
t'Base'obj = TypeBase


--
-- Built-in classes
--

data T'flash'display'Sprite
t'flash'display'Sprite = TypeObject "flash.display:Sprite"
instance IsSuper T'flash'display'Sprite T'flash'display'Sprite
instance IsSuper Base T'flash'display'Sprite

k'flash'display'Sprite'buttonMode :: Key T'flash'display'Sprite T'Boolean
k'flash'display'Sprite'buttonMode = Key "buttonMode" TypeBool

class K'buttonMode a b | a -> b where
  k'buttonMode :: Key a b

instance K'buttonMode T'flash'display'Sprite T'Boolean where
  k'buttonMode = k'flash'display'Sprite'buttonMode


data T'flash'events'MouseEvent
t'flash'events'MouseEvent = TypeObject "flash.events:MouseEvent"
instance IsSuper T'flash'events'MouseEvent T'flash'events'MouseEvent
instance IsSuper Base T'flash'events'MouseEvent

k'flash'events'MouseEvent'localX :: Key T'flash'events'MouseEvent T'int
k'flash'events'MouseEvent'localX = Key "localX" TypeInt

k'flash'events'MouseEvent'localY :: Key T'flash'events'MouseEvent T'int
k'flash'events'MouseEvent'localY = Key "localY" TypeInt
