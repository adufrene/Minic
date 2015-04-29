{-# OPTIONS_GHC -w #-}
module Mini.Parser where

import Control.Applicative ((<$>))
import Data.Char
import Data.List (find)
import Data.Maybe

import Mini.Types
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (Program)
	| HappyAbsSyn5 ([TypeDef])
	| HappyAbsSyn6 (TypeDef)
	| HappyAbsSyn7 ([Field])
	| HappyAbsSyn8 (Field)
	| HappyAbsSyn9 (Type)
	| HappyAbsSyn10 ([Declaration])
	| HappyAbsSyn12 ([Id])
	| HappyAbsSyn13 ([Function])
	| HappyAbsSyn14 (Function)
	| HappyAbsSyn18 (Statement)
	| HappyAbsSyn20 ([Statement])
	| HappyAbsSyn23 (Bool)
	| HappyAbsSyn26 (Maybe Statement)
	| HappyAbsSyn30 (Maybe Expression)
	| HappyAbsSyn32 (LValue)
	| HappyAbsSyn33 (Expression)
	| HappyAbsSyn40 ([Expression])
	| HappyAbsSyn41 (LineNumber)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157 :: () => Int -> ({-HappyReduction (P) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (P) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79 :: () => ({-HappyReduction (P) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (P) HappyAbsSyn)

action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_2
action_0 _ = happyReduce_2

action_1 (5) = happyGoto action_2
action_1 _ = happyFail

action_2 (42) = happyReduce_79
action_2 (6) = happyGoto action_4
action_2 (10) = happyGoto action_5
action_2 (41) = happyGoto action_6
action_2 _ = happyReduce_11

action_3 (77) = happyAccept
action_3 _ = happyFail

action_4 _ = happyReduce_3

action_5 (45) = happyReduce_17
action_5 (59) = happyReduce_17
action_5 (11) = happyGoto action_8
action_5 (13) = happyGoto action_9
action_5 (41) = happyGoto action_10
action_5 _ = happyReduce_79

action_6 (42) = happyShift action_7
action_6 _ = happyFail

action_7 (60) = happyShift action_18
action_7 _ = happyFail

action_8 _ = happyReduce_12

action_9 (59) = happyShift action_17
action_9 (14) = happyGoto action_15
action_9 (41) = happyGoto action_16
action_9 _ = happyReduce_79

action_10 (42) = happyShift action_12
action_10 (43) = happyShift action_13
action_10 (44) = happyShift action_14
action_10 (9) = happyGoto action_11
action_10 _ = happyFail

action_11 (60) = happyShift action_23
action_11 (12) = happyGoto action_22
action_11 _ = happyReduce_14

action_12 (60) = happyShift action_21
action_12 _ = happyFail

action_13 _ = happyReduce_8

action_14 _ = happyReduce_9

action_15 _ = happyReduce_18

action_16 (45) = happyShift action_20
action_16 _ = happyFail

action_17 _ = happyReduce_1

action_18 (71) = happyShift action_19
action_18 _ = happyFail

action_19 (7) = happyGoto action_27
action_19 _ = happyReduce_5

action_20 (60) = happyShift action_26
action_20 _ = happyFail

action_21 _ = happyReduce_10

action_22 (73) = happyShift action_24
action_22 (74) = happyShift action_25
action_22 _ = happyFail

action_23 _ = happyReduce_15

action_24 _ = happyReduce_13

action_25 (60) = happyShift action_33
action_25 _ = happyFail

action_26 (69) = happyShift action_32
action_26 (15) = happyGoto action_31
action_26 _ = happyFail

action_27 (72) = happyShift action_30
action_27 (8) = happyGoto action_28
action_27 (41) = happyGoto action_29
action_27 _ = happyReduce_79

action_28 (73) = happyShift action_42
action_28 _ = happyFail

action_29 (42) = happyShift action_12
action_29 (43) = happyShift action_13
action_29 (44) = happyShift action_14
action_29 (9) = happyGoto action_41
action_29 _ = happyFail

action_30 (73) = happyShift action_40
action_30 _ = happyFail

action_31 (42) = happyShift action_12
action_31 (43) = happyShift action_13
action_31 (44) = happyShift action_14
action_31 (46) = happyShift action_39
action_31 (9) = happyGoto action_37
action_31 (17) = happyGoto action_38
action_31 _ = happyFail

action_32 (70) = happyShift action_36
action_32 (8) = happyGoto action_34
action_32 (16) = happyGoto action_35
action_32 (41) = happyGoto action_29
action_32 _ = happyReduce_79

action_33 _ = happyReduce_16

action_34 _ = happyReduce_22

action_35 (70) = happyShift action_45
action_35 (74) = happyShift action_46
action_35 _ = happyFail

action_36 _ = happyReduce_20

action_37 _ = happyReduce_24

action_38 (71) = happyShift action_44
action_38 _ = happyFail

action_39 _ = happyReduce_25

action_40 _ = happyReduce_4

action_41 (60) = happyShift action_43
action_41 _ = happyFail

action_42 _ = happyReduce_6

action_43 _ = happyReduce_7

action_44 (10) = happyGoto action_48
action_44 _ = happyReduce_11

action_45 _ = happyReduce_21

action_46 (8) = happyGoto action_47
action_46 (41) = happyGoto action_29
action_46 _ = happyReduce_79

action_47 _ = happyReduce_23

action_48 (47) = happyReduce_36
action_48 (49) = happyReduce_36
action_48 (50) = happyReduce_36
action_48 (52) = happyReduce_36
action_48 (53) = happyReduce_36
action_48 (54) = happyReduce_36
action_48 (60) = happyReduce_36
action_48 (71) = happyReduce_36
action_48 (72) = happyReduce_36
action_48 (11) = happyGoto action_8
action_48 (20) = happyGoto action_49
action_48 (41) = happyGoto action_10
action_48 _ = happyReduce_79

action_49 (71) = happyShift action_61
action_49 (72) = happyShift action_62
action_49 (18) = happyGoto action_50
action_49 (19) = happyGoto action_51
action_49 (21) = happyGoto action_52
action_49 (22) = happyGoto action_53
action_49 (24) = happyGoto action_54
action_49 (25) = happyGoto action_55
action_49 (27) = happyGoto action_56
action_49 (28) = happyGoto action_57
action_49 (29) = happyGoto action_58
action_49 (31) = happyGoto action_59
action_49 (41) = happyGoto action_60
action_49 _ = happyReduce_79

action_50 _ = happyReduce_37

action_51 _ = happyReduce_26

action_52 _ = happyReduce_27

action_53 _ = happyReduce_28

action_54 _ = happyReduce_29

action_55 _ = happyReduce_30

action_56 _ = happyReduce_31

action_57 _ = happyReduce_32

action_58 _ = happyReduce_33

action_59 _ = happyReduce_34

action_60 (47) = happyShift action_66
action_60 (49) = happyShift action_67
action_60 (50) = happyShift action_68
action_60 (52) = happyShift action_69
action_60 (53) = happyShift action_70
action_60 (54) = happyShift action_71
action_60 (60) = happyShift action_72
action_60 (32) = happyGoto action_64
action_60 (41) = happyGoto action_65
action_60 _ = happyFail

action_61 (20) = happyGoto action_63
action_61 _ = happyReduce_36

action_62 _ = happyReduce_19

action_63 (71) = happyShift action_61
action_63 (72) = happyShift action_93
action_63 (18) = happyGoto action_50
action_63 (19) = happyGoto action_51
action_63 (21) = happyGoto action_52
action_63 (22) = happyGoto action_53
action_63 (24) = happyGoto action_54
action_63 (25) = happyGoto action_55
action_63 (27) = happyGoto action_56
action_63 (28) = happyGoto action_57
action_63 (29) = happyGoto action_58
action_63 (31) = happyGoto action_59
action_63 (41) = happyGoto action_60
action_63 _ = happyReduce_79

action_64 (64) = happyShift action_92
action_64 _ = happyFail

action_65 (60) = happyShift action_91
action_65 (32) = happyGoto action_90
action_65 (41) = happyGoto action_65
action_65 _ = happyFail

action_66 (69) = happyShift action_82
action_66 (33) = happyGoto action_89
action_66 (34) = happyGoto action_74
action_66 (35) = happyGoto action_75
action_66 (36) = happyGoto action_76
action_66 (37) = happyGoto action_77
action_66 (38) = happyGoto action_78
action_66 (39) = happyGoto action_79
action_66 (41) = happyGoto action_81
action_66 _ = happyReduce_79

action_67 (32) = happyGoto action_88
action_67 (41) = happyGoto action_65
action_67 _ = happyReduce_79

action_68 (69) = happyShift action_87
action_68 _ = happyFail

action_69 (69) = happyShift action_86
action_69 _ = happyFail

action_70 (69) = happyShift action_82
action_70 (33) = happyGoto action_85
action_70 (34) = happyGoto action_74
action_70 (35) = happyGoto action_75
action_70 (36) = happyGoto action_76
action_70 (37) = happyGoto action_77
action_70 (38) = happyGoto action_78
action_70 (39) = happyGoto action_79
action_70 (41) = happyGoto action_81
action_70 _ = happyReduce_79

action_71 (69) = happyShift action_82
action_71 (73) = happyReduce_49
action_71 (30) = happyGoto action_83
action_71 (33) = happyGoto action_84
action_71 (34) = happyGoto action_74
action_71 (35) = happyGoto action_75
action_71 (36) = happyGoto action_76
action_71 (37) = happyGoto action_77
action_71 (38) = happyGoto action_78
action_71 (39) = happyGoto action_79
action_71 (41) = happyGoto action_81
action_71 _ = happyReduce_79

action_72 (69) = happyShift action_82
action_72 (33) = happyGoto action_73
action_72 (34) = happyGoto action_74
action_72 (35) = happyGoto action_75
action_72 (36) = happyGoto action_76
action_72 (37) = happyGoto action_77
action_72 (38) = happyGoto action_78
action_72 (39) = happyGoto action_79
action_72 (40) = happyGoto action_80
action_72 (41) = happyGoto action_81
action_72 _ = happyReduce_79

action_73 _ = happyReduce_77

action_74 _ = happyReduce_54

action_75 _ = happyReduce_56

action_76 _ = happyReduce_58

action_77 _ = happyReduce_61

action_78 _ = happyReduce_64

action_79 _ = happyReduce_67

action_80 (73) = happyShift action_118
action_80 (74) = happyShift action_119
action_80 _ = happyFail

action_81 (55) = happyShift action_110
action_81 (56) = happyShift action_111
action_81 (57) = happyShift action_112
action_81 (58) = happyShift action_113
action_81 (60) = happyShift action_114
action_81 (63) = happyShift action_115
action_81 (66) = happyShift action_116
action_81 (69) = happyShift action_82
action_81 (76) = happyShift action_117
action_81 (34) = happyGoto action_104
action_81 (35) = happyGoto action_105
action_81 (36) = happyGoto action_106
action_81 (37) = happyGoto action_107
action_81 (38) = happyGoto action_108
action_81 (39) = happyGoto action_79
action_81 (41) = happyGoto action_109
action_81 _ = happyFail

action_82 (69) = happyShift action_82
action_82 (33) = happyGoto action_103
action_82 (34) = happyGoto action_74
action_82 (35) = happyGoto action_75
action_82 (36) = happyGoto action_76
action_82 (37) = happyGoto action_77
action_82 (38) = happyGoto action_78
action_82 (39) = happyGoto action_79
action_82 (41) = happyGoto action_81
action_82 _ = happyReduce_79

action_83 (73) = happyShift action_102
action_83 _ = happyFail

action_84 _ = happyReduce_50

action_85 (73) = happyShift action_101
action_85 _ = happyFail

action_86 (69) = happyShift action_82
action_86 (33) = happyGoto action_100
action_86 (34) = happyGoto action_74
action_86 (35) = happyGoto action_75
action_86 (36) = happyGoto action_76
action_86 (37) = happyGoto action_77
action_86 (38) = happyGoto action_78
action_86 (39) = happyGoto action_79
action_86 (41) = happyGoto action_81
action_86 _ = happyReduce_79

action_87 (69) = happyShift action_82
action_87 (33) = happyGoto action_99
action_87 (34) = happyGoto action_74
action_87 (35) = happyGoto action_75
action_87 (36) = happyGoto action_76
action_87 (37) = happyGoto action_77
action_87 (38) = happyGoto action_78
action_87 (39) = happyGoto action_79
action_87 (41) = happyGoto action_81
action_87 _ = happyReduce_79

action_88 (73) = happyShift action_98
action_88 _ = happyFail

action_89 (48) = happyShift action_97
action_89 (23) = happyGoto action_96
action_89 _ = happyReduce_40

action_90 (75) = happyShift action_95
action_90 _ = happyFail

action_91 _ = happyReduce_52

action_92 (69) = happyShift action_82
action_92 (33) = happyGoto action_94
action_92 (34) = happyGoto action_74
action_92 (35) = happyGoto action_75
action_92 (36) = happyGoto action_76
action_92 (37) = happyGoto action_77
action_92 (38) = happyGoto action_78
action_92 (39) = happyGoto action_79
action_92 (41) = happyGoto action_81
action_92 _ = happyReduce_79

action_93 _ = happyReduce_35

action_94 (73) = happyShift action_140
action_94 _ = happyFail

action_95 (60) = happyShift action_139
action_95 _ = happyFail

action_96 (73) = happyShift action_138
action_96 _ = happyFail

action_97 _ = happyReduce_41

action_98 _ = happyReduce_42

action_99 (70) = happyShift action_137
action_99 _ = happyFail

action_100 (70) = happyShift action_136
action_100 _ = happyFail

action_101 _ = happyReduce_47

action_102 _ = happyReduce_48

action_103 (70) = happyShift action_135
action_103 _ = happyFail

action_104 (61) = happyShift action_134
action_104 _ = happyFail

action_105 (62) = happyShift action_133
action_105 _ = happyReduce_56

action_106 (65) = happyShift action_131
action_106 (66) = happyShift action_132
action_106 _ = happyReduce_58

action_107 (67) = happyShift action_129
action_107 (68) = happyShift action_130
action_107 _ = happyReduce_61

action_108 (75) = happyShift action_128
action_108 _ = happyReduce_64

action_109 (55) = happyShift action_110
action_109 (56) = happyShift action_111
action_109 (57) = happyShift action_112
action_109 (58) = happyShift action_113
action_109 (60) = happyShift action_114
action_109 (63) = happyShift action_115
action_109 (66) = happyShift action_116
action_109 (69) = happyShift action_82
action_109 (76) = happyShift action_117
action_109 (35) = happyGoto action_126
action_109 (36) = happyGoto action_106
action_109 (37) = happyGoto action_107
action_109 (38) = happyGoto action_108
action_109 (39) = happyGoto action_79
action_109 (41) = happyGoto action_127
action_109 _ = happyFail

action_110 _ = happyReduce_73

action_111 _ = happyReduce_74

action_112 (60) = happyShift action_125
action_112 _ = happyFail

action_113 _ = happyReduce_76

action_114 (55) = happyReduce_79
action_114 (56) = happyReduce_79
action_114 (57) = happyReduce_79
action_114 (58) = happyReduce_79
action_114 (60) = happyReduce_79
action_114 (63) = happyReduce_79
action_114 (66) = happyReduce_79
action_114 (69) = happyShift action_82
action_114 (76) = happyReduce_79
action_114 (33) = happyGoto action_73
action_114 (34) = happyGoto action_74
action_114 (35) = happyGoto action_75
action_114 (36) = happyGoto action_76
action_114 (37) = happyGoto action_77
action_114 (38) = happyGoto action_78
action_114 (39) = happyGoto action_79
action_114 (40) = happyGoto action_124
action_114 (41) = happyGoto action_81
action_114 _ = happyReduce_70

action_115 _ = happyReduce_72

action_116 (69) = happyShift action_82
action_116 (38) = happyGoto action_123
action_116 (39) = happyGoto action_79
action_116 (41) = happyGoto action_122
action_116 _ = happyReduce_79

action_117 (69) = happyShift action_82
action_117 (38) = happyGoto action_121
action_117 (39) = happyGoto action_79
action_117 (41) = happyGoto action_122
action_117 _ = happyReduce_79

action_118 _ = happyReduce_51

action_119 (69) = happyShift action_82
action_119 (33) = happyGoto action_120
action_119 (34) = happyGoto action_74
action_119 (35) = happyGoto action_75
action_119 (36) = happyGoto action_76
action_119 (37) = happyGoto action_77
action_119 (38) = happyGoto action_78
action_119 (39) = happyGoto action_79
action_119 (41) = happyGoto action_81
action_119 _ = happyReduce_79

action_120 _ = happyReduce_78

action_121 _ = happyReduce_65

action_122 (55) = happyShift action_110
action_122 (56) = happyShift action_111
action_122 (57) = happyShift action_112
action_122 (58) = happyShift action_113
action_122 (60) = happyShift action_114
action_122 (63) = happyShift action_115
action_122 (69) = happyShift action_82
action_122 (38) = happyGoto action_153
action_122 (39) = happyGoto action_79
action_122 (41) = happyGoto action_122
action_122 _ = happyFail

action_123 _ = happyReduce_66

action_124 (74) = happyShift action_119
action_124 _ = happyReduce_71

action_125 _ = happyReduce_75

action_126 (62) = happyShift action_133
action_126 _ = happyFail

action_127 (55) = happyShift action_110
action_127 (56) = happyShift action_111
action_127 (57) = happyShift action_112
action_127 (58) = happyShift action_113
action_127 (60) = happyShift action_114
action_127 (63) = happyShift action_115
action_127 (66) = happyShift action_116
action_127 (69) = happyShift action_82
action_127 (76) = happyShift action_117
action_127 (36) = happyGoto action_152
action_127 (37) = happyGoto action_107
action_127 (38) = happyGoto action_108
action_127 (39) = happyGoto action_79
action_127 (41) = happyGoto action_146
action_127 _ = happyFail

action_128 (60) = happyShift action_151
action_128 _ = happyFail

action_129 (69) = happyShift action_82
action_129 (37) = happyGoto action_150
action_129 (38) = happyGoto action_78
action_129 (39) = happyGoto action_79
action_129 (41) = happyGoto action_149
action_129 _ = happyReduce_79

action_130 (69) = happyShift action_82
action_130 (37) = happyGoto action_148
action_130 (38) = happyGoto action_78
action_130 (39) = happyGoto action_79
action_130 (41) = happyGoto action_149
action_130 _ = happyReduce_79

action_131 (69) = happyShift action_82
action_131 (36) = happyGoto action_147
action_131 (37) = happyGoto action_77
action_131 (38) = happyGoto action_78
action_131 (39) = happyGoto action_79
action_131 (41) = happyGoto action_146
action_131 _ = happyReduce_79

action_132 (69) = happyShift action_82
action_132 (36) = happyGoto action_145
action_132 (37) = happyGoto action_77
action_132 (38) = happyGoto action_78
action_132 (39) = happyGoto action_79
action_132 (41) = happyGoto action_146
action_132 _ = happyReduce_79

action_133 (69) = happyShift action_82
action_133 (35) = happyGoto action_144
action_133 (36) = happyGoto action_76
action_133 (37) = happyGoto action_77
action_133 (38) = happyGoto action_78
action_133 (39) = happyGoto action_79
action_133 (41) = happyGoto action_127
action_133 _ = happyReduce_79

action_134 (69) = happyShift action_82
action_134 (34) = happyGoto action_143
action_134 (35) = happyGoto action_75
action_134 (36) = happyGoto action_76
action_134 (37) = happyGoto action_77
action_134 (38) = happyGoto action_78
action_134 (39) = happyGoto action_79
action_134 (41) = happyGoto action_109
action_134 _ = happyReduce_79

action_135 _ = happyReduce_69

action_136 (71) = happyShift action_61
action_136 (19) = happyGoto action_142
action_136 _ = happyFail

action_137 (71) = happyShift action_61
action_137 (19) = happyGoto action_141
action_137 _ = happyFail

action_138 _ = happyReduce_39

action_139 _ = happyReduce_53

action_140 _ = happyReduce_38

action_141 (51) = happyShift action_156
action_141 (26) = happyGoto action_155
action_141 _ = happyReduce_44

action_142 _ = happyReduce_46

action_143 _ = happyReduce_55

action_144 _ = happyReduce_57

action_145 _ = happyReduce_60

action_146 (55) = happyShift action_110
action_146 (56) = happyShift action_111
action_146 (57) = happyShift action_112
action_146 (58) = happyShift action_113
action_146 (60) = happyShift action_114
action_146 (63) = happyShift action_115
action_146 (66) = happyShift action_116
action_146 (69) = happyShift action_82
action_146 (76) = happyShift action_117
action_146 (37) = happyGoto action_154
action_146 (38) = happyGoto action_108
action_146 (39) = happyGoto action_79
action_146 (41) = happyGoto action_149
action_146 _ = happyFail

action_147 _ = happyReduce_59

action_148 _ = happyReduce_63

action_149 (55) = happyShift action_110
action_149 (56) = happyShift action_111
action_149 (57) = happyShift action_112
action_149 (58) = happyShift action_113
action_149 (60) = happyShift action_114
action_149 (63) = happyShift action_115
action_149 (66) = happyShift action_116
action_149 (69) = happyShift action_82
action_149 (76) = happyShift action_117
action_149 (38) = happyGoto action_153
action_149 (39) = happyGoto action_79
action_149 (41) = happyGoto action_122
action_149 _ = happyFail

action_150 _ = happyReduce_62

action_151 _ = happyReduce_68

action_152 (65) = happyShift action_131
action_152 (66) = happyShift action_132
action_152 _ = happyFail

action_153 (75) = happyShift action_128
action_153 _ = happyFail

action_154 (67) = happyShift action_129
action_154 (68) = happyShift action_130
action_154 _ = happyFail

action_155 _ = happyReduce_43

action_156 (71) = happyShift action_61
action_156 (19) = happyGoto action_157
action_156 _ = happyFail

action_157 _ = happyReduce_45

happyReduce_1 = happyReduce 4 4 happyReduction_1
happyReduction_1 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	(HappyAbsSyn5  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Program happy_var_1 happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 ([]
	)

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_2 : happy_var_1
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 7 6 happyReduction_4
happyReduction_4 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenId happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn41  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (TypeDef happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_0  7 happyReduction_5
happyReduction_5  =  HappyAbsSyn7
		 ([]
	)

happyReduce_6 = happySpecReduce_3  7 happyReduction_6
happyReduction_6 _
	(HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_2 : happy_var_1
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  8 happyReduction_7
happyReduction_7 (HappyTerminal (TokenId happy_var_3))
	(HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn8
		 (Field happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  9 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn9
		 (intType
	)

happyReduce_9 = happySpecReduce_1  9 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn9
		 (boolType
	)

happyReduce_10 = happySpecReduce_2  9 happyReduction_10
happyReduction_10 (HappyTerminal (TokenId happy_var_2))
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_0  10 happyReduction_11
happyReduction_11  =  HappyAbsSyn10
		 ([]
	)

happyReduce_12 = happySpecReduce_2  10 happyReduction_12
happyReduction_12 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_2 ++ happy_var_1
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happyReduce 4 11 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	(HappyAbsSyn41  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (map (Declaration happy_var_1 happy_var_2) happy_var_3
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_0  12 happyReduction_14
happyReduction_14  =  HappyAbsSyn12
		 ([]
	)

happyReduce_15 = happySpecReduce_1  12 happyReduction_15
happyReduction_15 (HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  12 happyReduction_16
happyReduction_16 (HappyTerminal (TokenId happy_var_3))
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_3 : happy_var_1
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_0  13 happyReduction_17
happyReduction_17  =  HappyAbsSyn13
		 ([]
	)

happyReduce_18 = happySpecReduce_2  13 happyReduction_18
happyReduction_18 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_2 : happy_var_1
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happyReduce 9 14 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_8) `HappyStk`
	(HappyAbsSyn10  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_5) `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	(HappyTerminal (TokenId happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn41  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Function happy_var_1 happy_var_3 happy_var_4 happy_var_7 happy_var_8 happy_var_5
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_2  15 happyReduction_20
happyReduction_20 _
	_
	 =  HappyAbsSyn7
		 ([]
	)

happyReduce_21 = happySpecReduce_3  15 happyReduction_21
happyReduction_21 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  16 happyReduction_22
happyReduction_22 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  16 happyReduction_23
happyReduction_23 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_3 : happy_var_1
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  17 happyReduction_24
happyReduction_24 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  17 happyReduction_25
happyReduction_25 _
	 =  HappyAbsSyn9
		 (voidType
	)

happyReduce_26 = happySpecReduce_1  18 happyReduction_26
happyReduction_26 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  18 happyReduction_27
happyReduction_27 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  18 happyReduction_28
happyReduction_28 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  18 happyReduction_29
happyReduction_29 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  18 happyReduction_30
happyReduction_30 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  18 happyReduction_31
happyReduction_31 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  18 happyReduction_32
happyReduction_32 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  18 happyReduction_33
happyReduction_33 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  18 happyReduction_34
happyReduction_34 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  19 happyReduction_35
happyReduction_35 _
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (Block happy_var_2
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_0  20 happyReduction_36
happyReduction_36  =  HappyAbsSyn20
		 ([]
	)

happyReduce_37 = happySpecReduce_2  20 happyReduction_37
happyReduction_37 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_2 : happy_var_1
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happyReduce 5 21 happyReduction_38
happyReduction_38 (_ `HappyStk`
	(HappyAbsSyn33  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn32  happy_var_2) `HappyStk`
	(HappyAbsSyn41  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (Asgn happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_39 = happyReduce 5 22 happyReduction_39
happyReduction_39 (_ `HappyStk`
	(HappyAbsSyn23  happy_var_4) `HappyStk`
	(HappyAbsSyn33  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn41  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (Print happy_var_1 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_40 = happySpecReduce_0  23 happyReduction_40
happyReduction_40  =  HappyAbsSyn23
		 (False
	)

happyReduce_41 = happySpecReduce_1  23 happyReduction_41
happyReduction_41 _
	 =  HappyAbsSyn23
		 (True
	)

happyReduce_42 = happyReduce 4 24 happyReduction_42
happyReduction_42 (_ `HappyStk`
	(HappyAbsSyn32  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn41  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (Read happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_43 = happyReduce 7 25 happyReduction_43
happyReduction_43 ((HappyAbsSyn26  happy_var_7) `HappyStk`
	(HappyAbsSyn18  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn33  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn41  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (Cond happy_var_1 happy_var_4 happy_var_6 happy_var_7
	) `HappyStk` happyRest

happyReduce_44 = happySpecReduce_0  26 happyReduction_44
happyReduction_44  =  HappyAbsSyn26
		 (Nothing
	)

happyReduce_45 = happySpecReduce_2  26 happyReduction_45
happyReduction_45 (HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (Just happy_var_2
	)
happyReduction_45 _ _  = notHappyAtAll 

happyReduce_46 = happyReduce 6 27 happyReduction_46
happyReduction_46 ((HappyAbsSyn18  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn33  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn41  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (Loop happy_var_1 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_47 = happyReduce 4 28 happyReduction_47
happyReduction_47 (_ `HappyStk`
	(HappyAbsSyn33  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn41  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (Delete happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_48 = happyReduce 4 29 happyReduction_48
happyReduction_48 (_ `HappyStk`
	(HappyAbsSyn30  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn41  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (Ret happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_49 = happySpecReduce_0  30 happyReduction_49
happyReduction_49  =  HappyAbsSyn30
		 (Nothing
	)

happyReduce_50 = happySpecReduce_1  30 happyReduction_50
happyReduction_50 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn30
		 (Just happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happyReduce 4 31 happyReduction_51
happyReduction_51 (_ `HappyStk`
	(HappyAbsSyn40  happy_var_3) `HappyStk`
	(HappyTerminal (TokenId happy_var_2)) `HappyStk`
	(HappyAbsSyn41  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (InvocSt happy_var_1 happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_52 = happySpecReduce_2  32 happyReduction_52
happyReduction_52 (HappyTerminal (TokenId happy_var_2))
	(HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn32
		 (LValue happy_var_1 happy_var_2 Nothing
	)
happyReduction_52 _ _  = notHappyAtAll 

happyReduce_53 = happyReduce 4 32 happyReduction_53
happyReduction_53 ((HappyTerminal (TokenId happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn32  happy_var_2) `HappyStk`
	(HappyAbsSyn41  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn32
		 (LValue happy_var_1 happy_var_4 (Just happy_var_2)
	) `HappyStk` happyRest

happyReduce_54 = happySpecReduce_1  33 happyReduction_54
happyReduction_54 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happyReduce 4 33 happyReduction_55
happyReduction_55 ((HappyAbsSyn33  happy_var_4) `HappyStk`
	(HappyTerminal (TokenBoolOp happy_var_3)) `HappyStk`
	(HappyAbsSyn33  happy_var_2) `HappyStk`
	(HappyAbsSyn41  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn33
		 (BinExp happy_var_1 happy_var_3 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_56 = happySpecReduce_1  34 happyReduction_56
happyReduction_56 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happyReduce 4 34 happyReduction_57
happyReduction_57 ((HappyAbsSyn33  happy_var_4) `HappyStk`
	(HappyTerminal (TokenCmpOp happy_var_3)) `HappyStk`
	(HappyAbsSyn33  happy_var_2) `HappyStk`
	(HappyAbsSyn41  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn33
		 (BinExp happy_var_1 happy_var_3 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_58 = happySpecReduce_1  35 happyReduction_58
happyReduction_58 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happyReduce 4 35 happyReduction_59
happyReduction_59 ((HappyAbsSyn33  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn33  happy_var_2) `HappyStk`
	(HappyAbsSyn41  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn33
		 (BinExp happy_var_1 "+" happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_60 = happyReduce 4 35 happyReduction_60
happyReduction_60 ((HappyAbsSyn33  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn33  happy_var_2) `HappyStk`
	(HappyAbsSyn41  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn33
		 (BinExp happy_var_1 "-" happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_61 = happySpecReduce_1  36 happyReduction_61
happyReduction_61 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happyReduce 4 36 happyReduction_62
happyReduction_62 ((HappyAbsSyn33  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn33  happy_var_2) `HappyStk`
	(HappyAbsSyn41  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn33
		 (BinExp happy_var_1 "*" happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_63 = happyReduce 4 36 happyReduction_63
happyReduction_63 ((HappyAbsSyn33  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn33  happy_var_2) `HappyStk`
	(HappyAbsSyn41  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn33
		 (BinExp happy_var_1 "/" happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_64 = happySpecReduce_1  37 happyReduction_64
happyReduction_64 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_3  37 happyReduction_65
happyReduction_65 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn33
		 (UExp happy_var_1 "!" happy_var_3
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_3  37 happyReduction_66
happyReduction_66 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn33
		 (UExp happy_var_1 "-" happy_var_3
	)
happyReduction_66 _ _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  38 happyReduction_67
happyReduction_67 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happyReduce 4 38 happyReduction_68
happyReduction_68 ((HappyTerminal (TokenId happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn33  happy_var_2) `HappyStk`
	(HappyAbsSyn41  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn33
		 (DotExp happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_69 = happySpecReduce_3  39 happyReduction_69
happyReduction_69 _
	(HappyAbsSyn33  happy_var_2)
	_
	 =  HappyAbsSyn33
		 (happy_var_2
	)
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_2  39 happyReduction_70
happyReduction_70 (HappyTerminal (TokenId happy_var_2))
	(HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn33
		 (IdExp happy_var_1 happy_var_2
	)
happyReduction_70 _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_3  39 happyReduction_71
happyReduction_71 (HappyAbsSyn40  happy_var_3)
	(HappyTerminal (TokenId happy_var_2))
	(HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn33
		 (InvocExp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_71 _ _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_2  39 happyReduction_72
happyReduction_72 (HappyTerminal (TokenNum happy_var_2))
	(HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn33
		 (IntExp happy_var_1 happy_var_2
	)
happyReduction_72 _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_2  39 happyReduction_73
happyReduction_73 _
	(HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn33
		 (TrueExp happy_var_1
	)
happyReduction_73 _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_2  39 happyReduction_74
happyReduction_74 _
	(HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn33
		 (FalseExp happy_var_1
	)
happyReduction_74 _ _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_3  39 happyReduction_75
happyReduction_75 (HappyTerminal (TokenId happy_var_3))
	_
	(HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn33
		 (NewExp happy_var_1 happy_var_3
	)
happyReduction_75 _ _ _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_2  39 happyReduction_76
happyReduction_76 _
	(HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn33
		 (NullExp happy_var_1
	)
happyReduction_76 _ _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_1  40 happyReduction_77
happyReduction_77 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn40
		 ([happy_var_1]
	)
happyReduction_77 _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_3  40 happyReduction_78
happyReduction_78 _
	(HappyTerminal happy_var_2)
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (happy_var_2 : happy_var_1
	)
happyReduction_78 _ _ _  = notHappyAtAll 

happyReduce_79 = happyMonadReduce 0 41 happyReduction_79
happyReduction_79 (happyRest) tk
	 = happyThen (( getLineNo)
	) (\r -> happyReturn (HappyAbsSyn41 r))

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TokenEOF -> action 77 77 tk (HappyState action) sts stk;
	TokenStruct -> cont 42;
	TokenInt -> cont 43;
	TokenBool -> cont 44;
	TokenFun -> cont 45;
	TokenVoid -> cont 46;
	TokenPrint -> cont 47;
	TokenEndl -> cont 48;
	TokenRead -> cont 49;
	TokenIf -> cont 50;
	TokenElse -> cont 51;
	TokenWhile -> cont 52;
	TokenDelete -> cont 53;
	TokenReturn -> cont 54;
	TokenTrue -> cont 55;
	TokenFalse -> cont 56;
	TokenNew -> cont 57;
	TokenNull -> cont 58;
	TokenEOF -> cont 59;
	TokenId happy_dollar_dollar -> cont 60;
	TokenBoolOp happy_dollar_dollar -> cont 61;
	TokenCmpOp happy_dollar_dollar -> cont 62;
	TokenNum happy_dollar_dollar -> cont 63;
	TokenEq -> cont 64;
	TokenPlus -> cont 65;
	TokenMinus -> cont 66;
	TokenTimes -> cont 67;
	TokenDiv -> cont 68;
	TokenOB -> cont 69;
	TokenCB -> cont 70;
	TokenLBrace -> cont 71;
	TokenRBrace -> cont 72;
	TokenSemi -> cont 73;
	TokenComma -> cont 74;
	TokenDot -> cont 75;
	TokenBang -> cont 76;
	_ -> happyError' tk
	})

happyError_ 77 tk = happyError' tk
happyError_ _ tk = happyError' tk

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = (thenP)
happyReturn :: () => a -> P a
happyReturn = (returnP)
happyThen1 = happyThen
happyReturn1 :: () => a -> P a
happyReturn1 = happyReturn
happyError' :: () => (Token) -> P a
happyError' tk = parseError tk

parse = happySomeParser where
  happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data ParseResult a = Ok a | Failed String
type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

data Token = TokenStruct
           | TokenInt
           | TokenBool
           | TokenFun
           | TokenVoid
           | TokenPrint
           | TokenEndl
           | TokenRead
           | TokenIf
           | TokenElse
           | TokenWhile
           | TokenDelete
           | TokenReturn
           | TokenTrue
           | TokenFalse
           | TokenNew
           | TokenNull
           | TokenEOF
           | TokenId Id
           | TokenBoolOp String 
           | TokenCmpOp String
           | TokenNum Int
           | TokenEq
           | TokenPlus
           | TokenMinus
           | TokenTimes
           | TokenDiv
           | TokenOB
           | TokenCB
           | TokenLBrace
           | TokenRBrace
           | TokenSemi
           | TokenComma
           | TokenDot
           | TokenBang
           deriving (Show)

keywords :: [(String, Token)]
keywords = [
    ("struct", TokenStruct),
    ("int", TokenInt),
    ("bool", TokenBool),
    ("fun", TokenFun),
    ("void", TokenVoid),
    ("print", TokenPrint),
    ("read", TokenRead),
    ("if", TokenIf),
    ("else", TokenElse),
    ("while", TokenWhile),
    ("delete", TokenDelete),
    ("return", TokenReturn),
    ("true", TokenTrue),
    ("false", TokenFalse),
    ("new", TokenNew),
    ("null", TokenNull)
    ]

charTks :: [(Char, Token)]
charTks = [
    ('=', TokenEq),
    ('+', TokenPlus),
    ('-', TokenMinus),
    ('*', TokenTimes),
    ('/', TokenDiv),
    ('(', TokenOB),
    (')', TokenCB),
    ('{', TokenLBrace),
    ('}', TokenRBrace),
    (';', TokenSemi),
    (',', TokenComma),
    ('.', TokenDot),
    ('<', TokenCmpOp "<"),
    ('>', TokenCmpOp ">"),
    ('!', TokenBang)
    ]

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
thenP m k = \s l ->
    case m s l of
        Ok a -> k a s l
        Failed e -> Failed e

returnP :: a -> P a
returnP a = \s l -> Ok a

failP :: String -> P a
failP a = \s l -> Failed a

parseError :: Token -> P a
parseError tk = getLineNo `thenP` \line ->
                        failP (show line ++ ": parse error at token"
                                ++ (show tk))

lexer :: (Token -> P a) -> P a
lexer cont s = case s of
        [] -> cont TokenEOF []
        ('\n':cs) -> \line -> lexer cont cs (line + 1)
        ('#':cs) -> lexer cont $ dropWhile ((/=) '\n') cs
        (c:cs)
            | isSpace c -> lexer cont cs
            | isAlpha c -> lexText cont (c:cs)
            | isDigit c -> lexNum cont (c:cs)
        ('<':'=':cs) -> cont (TokenCmpOp "<=") cs       -- <=
        ('>':'=':cs) -> cont (TokenCmpOp ">=") cs       -- >=
        ('=':'=':cs) -> cont (TokenCmpOp "==") cs       -- ==
        ('!':'=':cs) -> cont (TokenCmpOp "!=") cs       -- !=
        ('&':'&':cs) -> cont (TokenBoolOp "&&") cs      -- &&
        ('|':'|':cs) -> cont (TokenBoolOp "||") cs      -- ||
        (c:cs)
            | isJust (charTk c)-> cont (fromJust $ charTk c) cs
            | otherwise -> failP ("Unexpected token " ++ [c]) s
    where charTk c = snd <$> find ((==) c . fst) charTks

lexNum :: (Token -> P a) -> P a
lexNum cont s = cont (TokenNum (read num)) rest
    where (num, rest) = span isDigit s

lexText :: (Token -> P a) -> P a
lexText cont s = cont
        (fromMaybe (TokenId s) (snd <$> find ((==) word . fst) keywords)) rest
    where (word, rest) = span isAlpha s
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}







# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4










































{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
