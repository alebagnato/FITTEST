SWF   compressed: yes, version: 10, length: 1034, rate: 6144
TAG FileAttributes   use blit: no, use gpu: no, hasAS3: yes, use network: yes
TAG TagKind_Metadata   length: 457
TAG TagKind_ScriptLimits   length: 4
TAG TagKind_SetBackgroundColor   length: 3
TAG TagKind_Other 41   length: 26
TAG TagKind_FrameLabel   length: 6
TAG Abc   lazy init: yes, name: frame1
ABC   major version: 46, minor version: 16, classes: 1, methods: 4
TABLE integers
TABLE uintegers
TABLE doubles
TABLE strings
  1 -> 
  2 -> int
  3 -> Boolean
  4 -> Point
  5 -> Object
  6 -> x
  7 -> y
  8 -> isEqual
  9 -> INFO: enter .
  10 -> INFO: enter Point.
  11 -> INFO: enter Point.Point
  12 -> INFO: enter Point.isEqual
  13 -> INFO: exit .
  14 -> INFO: exit Point.
  15 -> INFO: exit Point.Point
  16 -> INFO: exit Point.isEqual
  17 -> trace
TABLE namespaces
  1 ->  {str-id: 1} {ns-id: 1}
  2 -> Point {str-id: 4} {ns-id: 2}
  3 ->  {str-id: 1} {ns-id: 3}
  4 ->  {str-id: 1} {ns-id: 4}
TABLE namesets
TABLE multinames
  1 -> namespace:  {str-id: 1} {ns-id: 1}, name: int {str-id: 2}
  2 -> namespace:  {str-id: 1} {ns-id: 1}, name: Boolean {str-id: 3}
  3 -> namespace:  {str-id: 1} {ns-id: 1}, name: Point {str-id: 4}
  4 -> namespace:  {str-id: 1} {ns-id: 1}, name: Object {str-id: 5}
  5 -> namespace:  {str-id: 1} {ns-id: 3}, name: x {str-id: 6}
  6 -> namespace:  {str-id: 1} {ns-id: 3}, name: y {str-id: 7}
  7 -> namespace:  {str-id: 1} {ns-id: 1}, name: isEqual {str-id: 8}
  8 -> namespace:  {str-id: 1} {ns-id: 4}, name: trace {str-id: 17}
METHODS_INFO
  METHOD 0 {}
  METHOD 0 {1 1}
  METHOD 2 {3}
  METHOD 0 {}
INSTANCES_INFO
  INSTANCE Point
    SUPER Object
    FLAG sealed
    FLAG has protected namespace
    CON (int, int):*
    NS Point {str-id: 4} {ns-id: 2}
    TRAIT x (5)  
      SLOT slotid: 0, type: int, vindex: 0, vkind:
    TRAIT y (6)  
      SLOT slotid: 0, type: int, vindex: 0, vkind:
    TRAIT isEqual (7)  
      METHOD disp-id: 0, method: isEqual(Point):Boolean
CLASSES_INFO
  CLASS Point
    CON ():*
SCRIPTS_INFO
  SCRIPT ():*   (3)
    TRAIT Point (3)  
      CLASS Point
METHOD_BODIES_INFO
  METHOD ():*   (0) max stack: 2, locals: 0, init scopes: 3, max scopes: 4
    135   : INSTR < 1 > begin body
    134   : INSTR < 2 > begin block
            INSTR Find property strict trace
            INSTR Push string INFO: enter Point.
            INSTR Call prop (void) trace args: 1
            INSTR Get local0
            INSTR Push scope
            INSTR Find property strict trace
            INSTR Push string INFO: exit Point.
            INSTR Call prop (void) trace args: 1
    168   : INSTR < 2 > end block
    169   : INSTR < 0 > end body
            INSTR Return (void)
  METHOD (int, int):*   (1) max stack: 2, locals: 2, init scopes: 4, max scopes: 5
    157   : INSTR < 4 > begin body
    156   : INSTR < 5 > begin block
            INSTR Find property strict trace
            INSTR Push string INFO: enter Point.Point
            INSTR Call prop (void) trace args: 1
            INSTR Get local0
            INSTR Push scope
            INSTR Get local0
            INSTR Construct super args: 0
            INSTR Get local0
            INSTR Get local1
            INSTR Init property x
            INSTR Get local0
            INSTR Get local2
            INSTR Init property y
            INSTR Find property strict trace
            INSTR Push string INFO: exit Point.Point
            INSTR Call prop (void) trace args: 1
    222   : INSTR < 5 > end block
    223   : INSTR < 0 > end body
            INSTR Return (void)
  METHOD isEqual(Point):Boolean   (2) max stack: 3, locals: 1, init scopes: 4, max scopes: 5
    213   : INSTR < 7 > begin body
    212   : INSTR < 8 > begin block
            INSTR Find property strict trace
            INSTR Push string INFO: enter Point.isEqual
            INSTR Call prop (void) trace args: 1
            INSTR Get local0
            INSTR Push scope
            INSTR Get local0
            INSTR Get property x
            INSTR Get local1
            INSTR Get property x
    250   : INSTR < 8 > end block
            INSTR If equal 28 (9)
    19    : INSTR < 9 > begin block
            INSTR Push false
            INSTR Find property strict trace
            INSTR Push string INFO: exit Point.isEqual
            INSTR Call prop (void) trace args: 1
    270   : INSTR < 9 > end block
    271   : INSTR < 0 > end body
            INSTR Return (value)
    28    : INSTR < 10 > begin block
            INSTR Get local0
            INSTR Get property y
            INSTR Get local1
            INSTR Get property y
    290   : INSTR < 10 > end block
            INSTR If equal 47 (9)
    38    : INSTR < 11 > begin block
            INSTR Push false
            INSTR Find property strict trace
            INSTR Push string INFO: exit Point.isEqual
            INSTR Call prop (void) trace args: 1
    310   : INSTR < 11 > end block
    311   : INSTR < 0 > end body
            INSTR Return (value)
    47    : INSTR < 12 > begin block
            INSTR Push true
            INSTR Find property strict trace
            INSTR Push string INFO: exit Point.isEqual
            INSTR Call prop (void) trace args: 1
    330   : INSTR < 12 > end block
    331   : INSTR < 0 > end body
            INSTR Return (value)
  METHOD ():*   (3) max stack: 2, locals: 0, init scopes: 1, max scopes: 3
    159   : INSTR < 14 > begin body
    158   : INSTR < 15 > begin block
            INSTR Find property strict trace
            INSTR Push string INFO: enter .
            INSTR Call prop (void) trace args: 1
            INSTR Get local0
            INSTR Push scope
            INSTR Get scope object index: 0
            INSTR Get lex (find+get prop) Object
            INSTR Push scope
            INSTR Get lex (find+get prop) Object
            INSTR New class Point
            INSTR Pop scope
            INSTR Init property Point
            INSTR Find property strict trace
            INSTR Push string INFO: exit .
            INSTR Call prop (void) trace args: 1
    220   : INSTR < 15 > end block
    221   : INSTR < 0 > end body
            INSTR Return (void)
TAG TagKind_SymbolClass   length: 10
TAG TagKind_ShowFrame   length: 0
