{-# OPTIONS_GHC -w #-}
module Mini.Parser (parse) where

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
 action_138 :: () => Int -> ({-HappyReduction (P) = -}
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

action_2 (42) = happyShift action_6
action_2 (6) = happyGoto action_4
action_2 (10) = happyGoto action_5
action_2 _ = happyReduce_11

action_3 (76) = happyAccept
action_3 _ = happyFail

action_4 _ = happyReduce_3

action_5 (42) = happyShift action_11
action_5 (43) = happyShift action_12
action_5 (44) = happyShift action_13
action_5 (9) = happyGoto action_8
action_5 (11) = happyGoto action_9
action_5 (13) = happyGoto action_10
action_5 _ = happyReduce_16

action_6 (59) = happyShift action_7
action_6 _ = happyFail

action_7 (70) = happyShift action_19
action_7 _ = happyFail

action_8 (59) = happyShift action_18
action_8 (12) = happyGoto action_17
action_8 _ = happyFail

action_9 _ = happyReduce_12

action_10 (45) = happyShift action_16
action_10 (14) = happyGoto action_15
action_10 _ = happyReduce_1

action_11 (59) = happyShift action_14
action_11 _ = happyFail

action_12 _ = happyReduce_8

action_13 _ = happyReduce_9

action_14 _ = happyReduce_10

action_15 _ = happyReduce_17

action_16 (59) = happyShift action_23
action_16 _ = happyFail

action_17 (72) = happyShift action_21
action_17 (73) = happyShift action_22
action_17 _ = happyFail

action_18 _ = happyReduce_14

action_19 (7) = happyGoto action_20
action_19 _ = happyReduce_5

action_20 (42) = happyShift action_11
action_20 (43) = happyShift action_12
action_20 (44) = happyShift action_13
action_20 (71) = happyShift action_29
action_20 (8) = happyGoto action_27
action_20 (9) = happyGoto action_28
action_20 _ = happyFail

action_21 _ = happyReduce_13

action_22 (59) = happyShift action_26
action_22 _ = happyFail

action_23 (68) = happyShift action_25
action_23 (15) = happyGoto action_24
action_23 _ = happyFail

action_24 (42) = happyShift action_11
action_24 (43) = happyShift action_12
action_24 (44) = happyShift action_13
action_24 (46) = happyShift action_38
action_24 (9) = happyGoto action_36
action_24 (17) = happyGoto action_37
action_24 _ = happyFail

action_25 (42) = happyShift action_11
action_25 (43) = happyShift action_12
action_25 (44) = happyShift action_13
action_25 (69) = happyShift action_35
action_25 (8) = happyGoto action_33
action_25 (9) = happyGoto action_28
action_25 (16) = happyGoto action_34
action_25 _ = happyFail

action_26 _ = happyReduce_15

action_27 (72) = happyShift action_32
action_27 _ = happyFail

action_28 (59) = happyShift action_31
action_28 _ = happyFail

action_29 (72) = happyShift action_30
action_29 _ = happyFail

action_30 _ = happyReduce_4

action_31 _ = happyReduce_7

action_32 _ = happyReduce_6

action_33 _ = happyReduce_21

action_34 (69) = happyShift action_40
action_34 (73) = happyShift action_41
action_34 _ = happyFail

action_35 _ = happyReduce_19

action_36 _ = happyReduce_23

action_37 (70) = happyShift action_39
action_37 _ = happyFail

action_38 _ = happyReduce_24

action_39 (10) = happyGoto action_43
action_39 _ = happyReduce_11

action_40 _ = happyReduce_20

action_41 (42) = happyShift action_11
action_41 (43) = happyShift action_12
action_41 (44) = happyShift action_13
action_41 (8) = happyGoto action_42
action_41 (9) = happyGoto action_28
action_41 _ = happyFail

action_42 _ = happyReduce_22

action_43 (42) = happyShift action_11
action_43 (43) = happyShift action_12
action_43 (44) = happyShift action_13
action_43 (9) = happyGoto action_8
action_43 (11) = happyGoto action_9
action_43 (20) = happyGoto action_44
action_43 _ = happyReduce_35

action_44 (47) = happyShift action_56
action_44 (49) = happyShift action_57
action_44 (50) = happyShift action_58
action_44 (52) = happyShift action_59
action_44 (53) = happyShift action_60
action_44 (54) = happyShift action_61
action_44 (59) = happyShift action_62
action_44 (70) = happyShift action_63
action_44 (71) = happyShift action_64
action_44 (18) = happyGoto action_45
action_44 (19) = happyGoto action_46
action_44 (21) = happyGoto action_47
action_44 (22) = happyGoto action_48
action_44 (24) = happyGoto action_49
action_44 (25) = happyGoto action_50
action_44 (27) = happyGoto action_51
action_44 (28) = happyGoto action_52
action_44 (29) = happyGoto action_53
action_44 (31) = happyGoto action_54
action_44 (32) = happyGoto action_55
action_44 _ = happyFail

action_45 _ = happyReduce_36

action_46 _ = happyReduce_25

action_47 _ = happyReduce_26

action_48 _ = happyReduce_27

action_49 _ = happyReduce_28

action_50 _ = happyReduce_29

action_51 _ = happyReduce_30

action_52 _ = happyReduce_31

action_53 _ = happyReduce_32

action_54 _ = happyReduce_33

action_55 (63) = happyShift action_91
action_55 (74) = happyShift action_92
action_55 _ = happyFail

action_56 (55) = happyShift action_76
action_56 (56) = happyShift action_77
action_56 (57) = happyShift action_78
action_56 (58) = happyShift action_79
action_56 (59) = happyShift action_80
action_56 (62) = happyShift action_81
action_56 (65) = happyShift action_82
action_56 (68) = happyShift action_83
action_56 (75) = happyShift action_84
action_56 (33) = happyGoto action_90
action_56 (34) = happyGoto action_70
action_56 (35) = happyGoto action_71
action_56 (36) = happyGoto action_72
action_56 (37) = happyGoto action_73
action_56 (38) = happyGoto action_74
action_56 (39) = happyGoto action_75
action_56 _ = happyFail

action_57 (59) = happyShift action_89
action_57 (32) = happyGoto action_88
action_57 _ = happyFail

action_58 (68) = happyShift action_87
action_58 _ = happyFail

action_59 (68) = happyShift action_86
action_59 _ = happyFail

action_60 (55) = happyShift action_76
action_60 (56) = happyShift action_77
action_60 (57) = happyShift action_78
action_60 (58) = happyShift action_79
action_60 (59) = happyShift action_80
action_60 (62) = happyShift action_81
action_60 (65) = happyShift action_82
action_60 (68) = happyShift action_83
action_60 (75) = happyShift action_84
action_60 (33) = happyGoto action_85
action_60 (34) = happyGoto action_70
action_60 (35) = happyGoto action_71
action_60 (36) = happyGoto action_72
action_60 (37) = happyGoto action_73
action_60 (38) = happyGoto action_74
action_60 (39) = happyGoto action_75
action_60 _ = happyFail

action_61 (55) = happyShift action_76
action_61 (56) = happyShift action_77
action_61 (57) = happyShift action_78
action_61 (58) = happyShift action_79
action_61 (59) = happyShift action_80
action_61 (62) = happyShift action_81
action_61 (65) = happyShift action_82
action_61 (68) = happyShift action_83
action_61 (75) = happyShift action_84
action_61 (30) = happyGoto action_68
action_61 (33) = happyGoto action_69
action_61 (34) = happyGoto action_70
action_61 (35) = happyGoto action_71
action_61 (36) = happyGoto action_72
action_61 (37) = happyGoto action_73
action_61 (38) = happyGoto action_74
action_61 (39) = happyGoto action_75
action_61 _ = happyReduce_48

action_62 (68) = happyShift action_67
action_62 (40) = happyGoto action_66
action_62 _ = happyReduce_51

action_63 (20) = happyGoto action_65
action_63 _ = happyReduce_35

action_64 _ = happyReduce_18

action_65 (47) = happyShift action_56
action_65 (49) = happyShift action_57
action_65 (50) = happyShift action_58
action_65 (52) = happyShift action_59
action_65 (53) = happyShift action_60
action_65 (54) = happyShift action_61
action_65 (59) = happyShift action_62
action_65 (70) = happyShift action_63
action_65 (71) = happyShift action_118
action_65 (18) = happyGoto action_45
action_65 (19) = happyGoto action_46
action_65 (21) = happyGoto action_47
action_65 (22) = happyGoto action_48
action_65 (24) = happyGoto action_49
action_65 (25) = happyGoto action_50
action_65 (27) = happyGoto action_51
action_65 (28) = happyGoto action_52
action_65 (29) = happyGoto action_53
action_65 (31) = happyGoto action_54
action_65 (32) = happyGoto action_55
action_65 _ = happyFail

action_66 (72) = happyShift action_117
action_66 _ = happyFail

action_67 (55) = happyShift action_76
action_67 (56) = happyShift action_77
action_67 (57) = happyShift action_78
action_67 (58) = happyShift action_79
action_67 (59) = happyShift action_80
action_67 (62) = happyShift action_81
action_67 (65) = happyShift action_82
action_67 (68) = happyShift action_83
action_67 (69) = happyShift action_116
action_67 (75) = happyShift action_84
action_67 (33) = happyGoto action_114
action_67 (34) = happyGoto action_70
action_67 (35) = happyGoto action_71
action_67 (36) = happyGoto action_72
action_67 (37) = happyGoto action_73
action_67 (38) = happyGoto action_74
action_67 (39) = happyGoto action_75
action_67 (41) = happyGoto action_115
action_67 _ = happyFail

action_68 (72) = happyShift action_113
action_68 _ = happyFail

action_69 (60) = happyShift action_97
action_69 _ = happyReduce_49

action_70 (61) = happyShift action_112
action_70 _ = happyReduce_53

action_71 (64) = happyShift action_110
action_71 (65) = happyShift action_111
action_71 _ = happyReduce_55

action_72 (66) = happyShift action_108
action_72 (67) = happyShift action_109
action_72 _ = happyReduce_57

action_73 _ = happyReduce_60

action_74 (74) = happyShift action_107
action_74 _ = happyReduce_63

action_75 _ = happyReduce_66

action_76 _ = happyReduce_72

action_77 _ = happyReduce_73

action_78 (59) = happyShift action_106
action_78 _ = happyFail

action_79 _ = happyReduce_75

action_80 (68) = happyShift action_67
action_80 (40) = happyGoto action_105
action_80 _ = happyReduce_69

action_81 _ = happyReduce_71

action_82 (55) = happyShift action_76
action_82 (56) = happyShift action_77
action_82 (57) = happyShift action_78
action_82 (58) = happyShift action_79
action_82 (59) = happyShift action_80
action_82 (62) = happyShift action_81
action_82 (65) = happyShift action_82
action_82 (68) = happyShift action_83
action_82 (75) = happyShift action_84
action_82 (37) = happyGoto action_104
action_82 (38) = happyGoto action_74
action_82 (39) = happyGoto action_75
action_82 _ = happyFail

action_83 (55) = happyShift action_76
action_83 (56) = happyShift action_77
action_83 (57) = happyShift action_78
action_83 (58) = happyShift action_79
action_83 (59) = happyShift action_80
action_83 (62) = happyShift action_81
action_83 (65) = happyShift action_82
action_83 (68) = happyShift action_83
action_83 (75) = happyShift action_84
action_83 (33) = happyGoto action_103
action_83 (34) = happyGoto action_70
action_83 (35) = happyGoto action_71
action_83 (36) = happyGoto action_72
action_83 (37) = happyGoto action_73
action_83 (38) = happyGoto action_74
action_83 (39) = happyGoto action_75
action_83 _ = happyFail

action_84 (55) = happyShift action_76
action_84 (56) = happyShift action_77
action_84 (57) = happyShift action_78
action_84 (58) = happyShift action_79
action_84 (59) = happyShift action_80
action_84 (62) = happyShift action_81
action_84 (65) = happyShift action_82
action_84 (68) = happyShift action_83
action_84 (75) = happyShift action_84
action_84 (37) = happyGoto action_102
action_84 (38) = happyGoto action_74
action_84 (39) = happyGoto action_75
action_84 _ = happyFail

action_85 (60) = happyShift action_97
action_85 (72) = happyShift action_101
action_85 _ = happyFail

action_86 (55) = happyShift action_76
action_86 (56) = happyShift action_77
action_86 (57) = happyShift action_78
action_86 (58) = happyShift action_79
action_86 (59) = happyShift action_80
action_86 (62) = happyShift action_81
action_86 (65) = happyShift action_82
action_86 (68) = happyShift action_83
action_86 (75) = happyShift action_84
action_86 (33) = happyGoto action_100
action_86 (34) = happyGoto action_70
action_86 (35) = happyGoto action_71
action_86 (36) = happyGoto action_72
action_86 (37) = happyGoto action_73
action_86 (38) = happyGoto action_74
action_86 (39) = happyGoto action_75
action_86 _ = happyFail

action_87 (55) = happyShift action_76
action_87 (56) = happyShift action_77
action_87 (57) = happyShift action_78
action_87 (58) = happyShift action_79
action_87 (59) = happyShift action_80
action_87 (62) = happyShift action_81
action_87 (65) = happyShift action_82
action_87 (68) = happyShift action_83
action_87 (75) = happyShift action_84
action_87 (33) = happyGoto action_99
action_87 (34) = happyGoto action_70
action_87 (35) = happyGoto action_71
action_87 (36) = happyGoto action_72
action_87 (37) = happyGoto action_73
action_87 (38) = happyGoto action_74
action_87 (39) = happyGoto action_75
action_87 _ = happyFail

action_88 (72) = happyShift action_98
action_88 (74) = happyShift action_92
action_88 _ = happyFail

action_89 _ = happyReduce_51

action_90 (48) = happyShift action_96
action_90 (60) = happyShift action_97
action_90 (23) = happyGoto action_95
action_90 _ = happyReduce_39

action_91 (55) = happyShift action_76
action_91 (56) = happyShift action_77
action_91 (57) = happyShift action_78
action_91 (58) = happyShift action_79
action_91 (59) = happyShift action_80
action_91 (62) = happyShift action_81
action_91 (65) = happyShift action_82
action_91 (68) = happyShift action_83
action_91 (75) = happyShift action_84
action_91 (33) = happyGoto action_94
action_91 (34) = happyGoto action_70
action_91 (35) = happyGoto action_71
action_91 (36) = happyGoto action_72
action_91 (37) = happyGoto action_73
action_91 (38) = happyGoto action_74
action_91 (39) = happyGoto action_75
action_91 _ = happyFail

action_92 (59) = happyShift action_93
action_92 _ = happyFail

action_93 _ = happyReduce_52

action_94 (60) = happyShift action_97
action_94 (72) = happyShift action_132
action_94 _ = happyFail

action_95 (72) = happyShift action_131
action_95 _ = happyFail

action_96 _ = happyReduce_40

action_97 (55) = happyShift action_76
action_97 (56) = happyShift action_77
action_97 (57) = happyShift action_78
action_97 (58) = happyShift action_79
action_97 (59) = happyShift action_80
action_97 (62) = happyShift action_81
action_97 (65) = happyShift action_82
action_97 (68) = happyShift action_83
action_97 (75) = happyShift action_84
action_97 (34) = happyGoto action_130
action_97 (35) = happyGoto action_71
action_97 (36) = happyGoto action_72
action_97 (37) = happyGoto action_73
action_97 (38) = happyGoto action_74
action_97 (39) = happyGoto action_75
action_97 _ = happyFail

action_98 _ = happyReduce_41

action_99 (60) = happyShift action_97
action_99 (69) = happyShift action_129
action_99 _ = happyFail

action_100 (60) = happyShift action_97
action_100 (69) = happyShift action_128
action_100 _ = happyFail

action_101 _ = happyReduce_46

action_102 _ = happyReduce_64

action_103 (60) = happyShift action_97
action_103 (69) = happyShift action_127
action_103 _ = happyFail

action_104 _ = happyReduce_65

action_105 _ = happyReduce_70

action_106 _ = happyReduce_74

action_107 (59) = happyShift action_126
action_107 _ = happyFail

action_108 (55) = happyShift action_76
action_108 (56) = happyShift action_77
action_108 (57) = happyShift action_78
action_108 (58) = happyShift action_79
action_108 (59) = happyShift action_80
action_108 (62) = happyShift action_81
action_108 (65) = happyShift action_82
action_108 (68) = happyShift action_83
action_108 (75) = happyShift action_84
action_108 (37) = happyGoto action_125
action_108 (38) = happyGoto action_74
action_108 (39) = happyGoto action_75
action_108 _ = happyFail

action_109 (55) = happyShift action_76
action_109 (56) = happyShift action_77
action_109 (57) = happyShift action_78
action_109 (58) = happyShift action_79
action_109 (59) = happyShift action_80
action_109 (62) = happyShift action_81
action_109 (65) = happyShift action_82
action_109 (68) = happyShift action_83
action_109 (75) = happyShift action_84
action_109 (37) = happyGoto action_124
action_109 (38) = happyGoto action_74
action_109 (39) = happyGoto action_75
action_109 _ = happyFail

action_110 (55) = happyShift action_76
action_110 (56) = happyShift action_77
action_110 (57) = happyShift action_78
action_110 (58) = happyShift action_79
action_110 (59) = happyShift action_80
action_110 (62) = happyShift action_81
action_110 (65) = happyShift action_82
action_110 (68) = happyShift action_83
action_110 (75) = happyShift action_84
action_110 (36) = happyGoto action_123
action_110 (37) = happyGoto action_73
action_110 (38) = happyGoto action_74
action_110 (39) = happyGoto action_75
action_110 _ = happyFail

action_111 (55) = happyShift action_76
action_111 (56) = happyShift action_77
action_111 (57) = happyShift action_78
action_111 (58) = happyShift action_79
action_111 (59) = happyShift action_80
action_111 (62) = happyShift action_81
action_111 (65) = happyShift action_82
action_111 (68) = happyShift action_83
action_111 (75) = happyShift action_84
action_111 (36) = happyGoto action_122
action_111 (37) = happyGoto action_73
action_111 (38) = happyGoto action_74
action_111 (39) = happyGoto action_75
action_111 _ = happyFail

action_112 (55) = happyShift action_76
action_112 (56) = happyShift action_77
action_112 (57) = happyShift action_78
action_112 (58) = happyShift action_79
action_112 (59) = happyShift action_80
action_112 (62) = happyShift action_81
action_112 (65) = happyShift action_82
action_112 (68) = happyShift action_83
action_112 (75) = happyShift action_84
action_112 (35) = happyGoto action_121
action_112 (36) = happyGoto action_72
action_112 (37) = happyGoto action_73
action_112 (38) = happyGoto action_74
action_112 (39) = happyGoto action_75
action_112 _ = happyFail

action_113 _ = happyReduce_47

action_114 (60) = happyShift action_97
action_114 _ = happyReduce_78

action_115 (69) = happyShift action_119
action_115 (73) = happyShift action_120
action_115 _ = happyFail

action_116 _ = happyReduce_76

action_117 _ = happyReduce_50

action_118 _ = happyReduce_34

action_119 _ = happyReduce_77

action_120 (55) = happyShift action_76
action_120 (56) = happyShift action_77
action_120 (57) = happyShift action_78
action_120 (58) = happyShift action_79
action_120 (59) = happyShift action_80
action_120 (62) = happyShift action_81
action_120 (65) = happyShift action_82
action_120 (68) = happyShift action_83
action_120 (75) = happyShift action_84
action_120 (33) = happyGoto action_135
action_120 (34) = happyGoto action_70
action_120 (35) = happyGoto action_71
action_120 (36) = happyGoto action_72
action_120 (37) = happyGoto action_73
action_120 (38) = happyGoto action_74
action_120 (39) = happyGoto action_75
action_120 _ = happyFail

action_121 (64) = happyShift action_110
action_121 (65) = happyShift action_111
action_121 _ = happyReduce_56

action_122 (66) = happyShift action_108
action_122 (67) = happyShift action_109
action_122 _ = happyReduce_59

action_123 (66) = happyShift action_108
action_123 (67) = happyShift action_109
action_123 _ = happyReduce_58

action_124 _ = happyReduce_62

action_125 _ = happyReduce_61

action_126 _ = happyReduce_67

action_127 _ = happyReduce_68

action_128 (70) = happyShift action_63
action_128 (19) = happyGoto action_134
action_128 _ = happyFail

action_129 (70) = happyShift action_63
action_129 (19) = happyGoto action_133
action_129 _ = happyFail

action_130 (61) = happyShift action_112
action_130 _ = happyReduce_54

action_131 _ = happyReduce_38

action_132 _ = happyReduce_37

action_133 (51) = happyShift action_137
action_133 (26) = happyGoto action_136
action_133 _ = happyReduce_43

action_134 _ = happyReduce_45

action_135 (60) = happyShift action_97
action_135 _ = happyReduce_79

action_136 _ = happyReduce_42

action_137 (70) = happyShift action_63
action_137 (19) = happyGoto action_138
action_137 _ = happyFail

action_138 _ = happyReduce_44

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn13  happy_var_3)
	(HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (Program (reverse happy_var_1) (reverse happy_var_2) (reverse happy_var_3)
	)
happyReduction_1 _ _ _  = notHappyAtAll 

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

happyReduce_4 = happyReduce 6 6 happyReduction_4
happyReduction_4 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (TypeDef 0 happy_var_2 $ reverse happy_var_4
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

happyReduce_7 = happySpecReduce_2  8 happyReduction_7
happyReduction_7 (HappyTerminal (TokenId happy_var_2))
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (Field 0 happy_var_1 happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

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

happyReduce_13 = happySpecReduce_3  11 happyReduction_13
happyReduction_13 _
	(HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn10
		 (map (Declaration 0 happy_var_1) happy_var_2
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  12 happyReduction_14
happyReduction_14 (HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  12 happyReduction_15
happyReduction_15 (HappyTerminal (TokenId happy_var_3))
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_3 : happy_var_1
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_0  13 happyReduction_16
happyReduction_16  =  HappyAbsSyn13
		 ([]
	)

happyReduce_17 = happySpecReduce_2  13 happyReduction_17
happyReduction_17 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_2 : happy_var_1
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 8 14 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_7) `HappyStk`
	(HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	(HappyTerminal (TokenId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Function 0 happy_var_2 (reverse happy_var_3) (reverse happy_var_6) (reverse happy_var_7) happy_var_4
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_2  15 happyReduction_19
happyReduction_19 _
	_
	 =  HappyAbsSyn7
		 ([]
	)

happyReduce_20 = happySpecReduce_3  15 happyReduction_20
happyReduction_20 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  16 happyReduction_21
happyReduction_21 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  16 happyReduction_22
happyReduction_22 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_3 : happy_var_1
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  17 happyReduction_23
happyReduction_23 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  17 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn9
		 (voidType
	)

happyReduce_25 = happySpecReduce_1  18 happyReduction_25
happyReduction_25 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

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

happyReduce_34 = happySpecReduce_3  19 happyReduction_34
happyReduction_34 _
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (Block (reverse happy_var_2)
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_0  20 happyReduction_35
happyReduction_35  =  HappyAbsSyn20
		 ([]
	)

happyReduce_36 = happySpecReduce_2  20 happyReduction_36
happyReduction_36 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_2 : happy_var_1
	)
happyReduction_36 _ _  = notHappyAtAll 

happyReduce_37 = happyReduce 4 21 happyReduction_37
happyReduction_37 (_ `HappyStk`
	(HappyAbsSyn33  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn32  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (Asgn 0 happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_38 = happyReduce 4 22 happyReduction_38
happyReduction_38 (_ `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	(HappyAbsSyn33  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (Print 0 happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_39 = happySpecReduce_0  23 happyReduction_39
happyReduction_39  =  HappyAbsSyn23
		 (False
	)

happyReduce_40 = happySpecReduce_1  23 happyReduction_40
happyReduction_40 _
	 =  HappyAbsSyn23
		 (True
	)

happyReduce_41 = happySpecReduce_3  24 happyReduction_41
happyReduction_41 _
	(HappyAbsSyn32  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (Read 0 happy_var_2
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happyReduce 6 25 happyReduction_42
happyReduction_42 ((HappyAbsSyn26  happy_var_6) `HappyStk`
	(HappyAbsSyn18  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn33  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (Cond 0 happy_var_3 happy_var_5 happy_var_6
	) `HappyStk` happyRest

happyReduce_43 = happySpecReduce_0  26 happyReduction_43
happyReduction_43  =  HappyAbsSyn26
		 (Nothing
	)

happyReduce_44 = happySpecReduce_2  26 happyReduction_44
happyReduction_44 (HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (Just happy_var_2
	)
happyReduction_44 _ _  = notHappyAtAll 

happyReduce_45 = happyReduce 5 27 happyReduction_45
happyReduction_45 ((HappyAbsSyn18  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn33  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (Loop 0 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_46 = happySpecReduce_3  28 happyReduction_46
happyReduction_46 _
	(HappyAbsSyn33  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (Delete 0 happy_var_2
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  29 happyReduction_47
happyReduction_47 _
	(HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (Ret 0 happy_var_2
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_0  30 happyReduction_48
happyReduction_48  =  HappyAbsSyn30
		 (Nothing
	)

happyReduce_49 = happySpecReduce_1  30 happyReduction_49
happyReduction_49 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn30
		 (Just happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  31 happyReduction_50
happyReduction_50 _
	(HappyAbsSyn40  happy_var_2)
	(HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn18
		 (InvocSt 0 happy_var_1 $ reverse happy_var_2
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  32 happyReduction_51
happyReduction_51 (HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn32
		 (LValue Nothing happy_var_1 Nothing
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  32 happyReduction_52
happyReduction_52 (HappyTerminal (TokenId happy_var_3))
	_
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn32
		 (LValue (Just 0) happy_var_3 (Just happy_var_1)
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  33 happyReduction_53
happyReduction_53 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_3  33 happyReduction_54
happyReduction_54 (HappyAbsSyn33  happy_var_3)
	(HappyTerminal (TokenBoolOp happy_var_2))
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (BinExp 0 happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  34 happyReduction_55
happyReduction_55 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_3  34 happyReduction_56
happyReduction_56 (HappyAbsSyn33  happy_var_3)
	(HappyTerminal (TokenCmpOp happy_var_2))
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (BinExp 0 happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  35 happyReduction_57
happyReduction_57 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_3  35 happyReduction_58
happyReduction_58 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (BinExp 0 "+" happy_var_1 happy_var_3
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_3  35 happyReduction_59
happyReduction_59 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (BinExp 0 "-" happy_var_1 happy_var_3
	)
happyReduction_59 _ _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_1  36 happyReduction_60
happyReduction_60 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_3  36 happyReduction_61
happyReduction_61 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (BinExp 0 "*" happy_var_1 happy_var_3
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_3  36 happyReduction_62
happyReduction_62 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (BinExp 0 "/" happy_var_1 happy_var_3
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_1  37 happyReduction_63
happyReduction_63 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_2  37 happyReduction_64
happyReduction_64 (HappyAbsSyn33  happy_var_2)
	_
	 =  HappyAbsSyn33
		 (UExp 0 "!" happy_var_2
	)
happyReduction_64 _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_2  37 happyReduction_65
happyReduction_65 (HappyAbsSyn33  happy_var_2)
	_
	 =  HappyAbsSyn33
		 (UExp 0 "-" happy_var_2
	)
happyReduction_65 _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  38 happyReduction_66
happyReduction_66 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  38 happyReduction_67
happyReduction_67 (HappyTerminal (TokenId happy_var_3))
	_
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (DotExp 0 happy_var_1 happy_var_3
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  39 happyReduction_68
happyReduction_68 _
	(HappyAbsSyn33  happy_var_2)
	_
	 =  HappyAbsSyn33
		 (happy_var_2
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_1  39 happyReduction_69
happyReduction_69 (HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn33
		 (IdExp 0 happy_var_1
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_2  39 happyReduction_70
happyReduction_70 (HappyAbsSyn40  happy_var_2)
	(HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn33
		 (InvocExp 0 happy_var_1 $ reverse happy_var_2
	)
happyReduction_70 _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_1  39 happyReduction_71
happyReduction_71 (HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn33
		 (IntExp 0 happy_var_1
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  39 happyReduction_72
happyReduction_72 _
	 =  HappyAbsSyn33
		 (TrueExp 0
	)

happyReduce_73 = happySpecReduce_1  39 happyReduction_73
happyReduction_73 _
	 =  HappyAbsSyn33
		 (FalseExp 0
	)

happyReduce_74 = happySpecReduce_2  39 happyReduction_74
happyReduction_74 (HappyTerminal (TokenId happy_var_2))
	_
	 =  HappyAbsSyn33
		 (NewExp 0 happy_var_2
	)
happyReduction_74 _ _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1  39 happyReduction_75
happyReduction_75 _
	 =  HappyAbsSyn33
		 (NullExp 0
	)

happyReduce_76 = happySpecReduce_2  40 happyReduction_76
happyReduction_76 _
	_
	 =  HappyAbsSyn40
		 ([]
	)

happyReduce_77 = happySpecReduce_3  40 happyReduction_77
happyReduction_77 _
	(HappyAbsSyn40  happy_var_2)
	_
	 =  HappyAbsSyn40
		 (happy_var_2
	)
happyReduction_77 _ _ _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_1  41 happyReduction_78
happyReduction_78 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn40
		 ([happy_var_1]
	)
happyReduction_78 _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_3  41 happyReduction_79
happyReduction_79 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (happy_var_3 : happy_var_1
	)
happyReduction_79 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TokenEOF -> action 76 76 tk (HappyState action) sts stk;
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
	TokenId happy_dollar_dollar -> cont 59;
	TokenBoolOp happy_dollar_dollar -> cont 60;
	TokenCmpOp happy_dollar_dollar -> cont 61;
	TokenNum happy_dollar_dollar -> cont 62;
	TokenEq -> cont 63;
	TokenPlus -> cont 64;
	TokenMinus -> cont 65;
	TokenTimes -> cont 66;
	TokenDiv -> cont 67;
	TokenOB -> cont 68;
	TokenCB -> cont 69;
	TokenLBrace -> cont 70;
	TokenRBrace -> cont 71;
	TokenSemi -> cont 72;
	TokenComma -> cont 73;
	TokenDot -> cont 74;
	TokenBang -> cont 75;
	_ -> happyError' tk
	})

happyError_ 76 tk = happyError' tk
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

calc = happySomeParser where
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
    ("null", TokenNull),
    ("endl", TokenEndl)
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
                        failP (show line ++ ": parse error at "
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
        (fromMaybe (TokenId word) (snd <$> find ((==) word . fst) keywords)) rest
    where (word, rest) = span (\x -> isAlpha x || isDigit x) s
 
fromParseResult :: ParseResult a -> a
fromParseResult (Ok a) = a
fromParseResult (Failed s) = error s

parse :: String -> Program
parse fileString = fromParseResult $ calc fileString 1
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
