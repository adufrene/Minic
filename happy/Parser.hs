{-# OPTIONS_GHC -w #-}
module Mini.Parser (parse) where

import Control.Applicative ((<$>))
import Control.Monad.Reader
import Data.Char
import Data.Either
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
	| HappyAbsSyn5 ([TypeDecl])
	| HappyAbsSyn7 ([Field])
	| HappyAbsSyn8 (Field)
	| HappyAbsSyn9 (Type)
	| HappyAbsSyn11 (Id -> [TypeDecl])
	| HappyAbsSyn12 ([Declaration])
	| HappyAbsSyn14 ([Id])
	| HappyAbsSyn15 ([Function])
	| HappyAbsSyn16 (Function)
	| HappyAbsSyn20 (Statement)
	| HappyAbsSyn22 ([Statement])
	| HappyAbsSyn25 (Bool)
	| HappyAbsSyn28 (Maybe Statement)
	| HappyAbsSyn32 (Maybe Expression)
	| HappyAbsSyn34 (LValue)
	| HappyAbsSyn35 (Expression)
	| HappyAbsSyn42 ([Expression])

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
 action_145 :: () => Int -> ({-HappyReduction (P) = -}
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
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83,
 happyReduce_84 :: () => ({-HappyReduction (P) = -}
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

action_2 (44) = happyShift action_7
action_2 (45) = happyShift action_8
action_2 (46) = happyShift action_9
action_2 (6) = happyGoto action_4
action_2 (10) = happyGoto action_5
action_2 (15) = happyGoto action_6
action_2 _ = happyReduce_21

action_3 (78) = happyAccept
action_3 _ = happyFail

action_4 _ = happyReduce_3

action_5 (61) = happyShift action_14
action_5 (14) = happyGoto action_13
action_5 _ = happyFail

action_6 (47) = happyShift action_12
action_6 (16) = happyGoto action_11
action_6 _ = happyReduce_1

action_7 (61) = happyShift action_10
action_7 _ = happyFail

action_8 _ = happyReduce_12

action_9 _ = happyReduce_13

action_10 (61) = happyShift action_14
action_10 (72) = happyShift action_20
action_10 (11) = happyGoto action_18
action_10 (14) = happyGoto action_19
action_10 _ = happyFail

action_11 _ = happyReduce_22

action_12 (61) = happyShift action_17
action_12 _ = happyFail

action_13 (74) = happyShift action_15
action_13 (75) = happyShift action_16
action_13 _ = happyFail

action_14 _ = happyReduce_19

action_15 _ = happyReduce_5

action_16 (61) = happyShift action_25
action_16 _ = happyFail

action_17 (70) = happyShift action_24
action_17 (17) = happyGoto action_23
action_17 _ = happyFail

action_18 _ = happyReduce_4

action_19 (74) = happyShift action_22
action_19 (75) = happyShift action_16
action_19 _ = happyFail

action_20 (7) = happyGoto action_21
action_20 _ = happyReduce_6

action_21 (44) = happyShift action_29
action_21 (45) = happyShift action_30
action_21 (46) = happyShift action_31
action_21 (73) = happyShift action_37
action_21 (8) = happyGoto action_36
action_21 (9) = happyGoto action_27
action_21 _ = happyFail

action_22 _ = happyReduce_15

action_23 (44) = happyShift action_29
action_23 (45) = happyShift action_30
action_23 (46) = happyShift action_31
action_23 (48) = happyShift action_35
action_23 (9) = happyGoto action_33
action_23 (19) = happyGoto action_34
action_23 _ = happyFail

action_24 (44) = happyShift action_29
action_24 (45) = happyShift action_30
action_24 (46) = happyShift action_31
action_24 (71) = happyShift action_32
action_24 (8) = happyGoto action_26
action_24 (9) = happyGoto action_27
action_24 (18) = happyGoto action_28
action_24 _ = happyFail

action_25 _ = happyReduce_20

action_26 _ = happyReduce_26

action_27 (61) = happyShift action_44
action_27 _ = happyFail

action_28 (71) = happyShift action_42
action_28 (75) = happyShift action_43
action_28 _ = happyFail

action_29 (61) = happyShift action_41
action_29 _ = happyFail

action_30 _ = happyReduce_9

action_31 _ = happyReduce_10

action_32 _ = happyReduce_24

action_33 _ = happyReduce_28

action_34 (72) = happyShift action_40
action_34 _ = happyFail

action_35 _ = happyReduce_29

action_36 (74) = happyShift action_39
action_36 _ = happyFail

action_37 (74) = happyShift action_38
action_37 _ = happyFail

action_38 _ = happyReduce_14

action_39 _ = happyReduce_7

action_40 (12) = happyGoto action_46
action_40 _ = happyReduce_16

action_41 _ = happyReduce_11

action_42 _ = happyReduce_25

action_43 (44) = happyShift action_29
action_43 (45) = happyShift action_30
action_43 (46) = happyShift action_31
action_43 (8) = happyGoto action_45
action_43 (9) = happyGoto action_27
action_43 _ = happyFail

action_44 _ = happyReduce_8

action_45 _ = happyReduce_27

action_46 (44) = happyShift action_29
action_46 (45) = happyShift action_30
action_46 (46) = happyShift action_31
action_46 (9) = happyGoto action_47
action_46 (13) = happyGoto action_48
action_46 (22) = happyGoto action_49
action_46 _ = happyReduce_40

action_47 (61) = happyShift action_14
action_47 (14) = happyGoto action_70
action_47 _ = happyFail

action_48 _ = happyReduce_17

action_49 (49) = happyShift action_61
action_49 (51) = happyShift action_62
action_49 (52) = happyShift action_63
action_49 (54) = happyShift action_64
action_49 (55) = happyShift action_65
action_49 (56) = happyShift action_66
action_49 (61) = happyShift action_67
action_49 (72) = happyShift action_68
action_49 (73) = happyShift action_69
action_49 (20) = happyGoto action_50
action_49 (21) = happyGoto action_51
action_49 (23) = happyGoto action_52
action_49 (24) = happyGoto action_53
action_49 (26) = happyGoto action_54
action_49 (27) = happyGoto action_55
action_49 (29) = happyGoto action_56
action_49 (30) = happyGoto action_57
action_49 (31) = happyGoto action_58
action_49 (33) = happyGoto action_59
action_49 (34) = happyGoto action_60
action_49 _ = happyFail

action_50 _ = happyReduce_41

action_51 _ = happyReduce_30

action_52 _ = happyReduce_31

action_53 _ = happyReduce_32

action_54 _ = happyReduce_33

action_55 _ = happyReduce_34

action_56 _ = happyReduce_35

action_57 _ = happyReduce_36

action_58 _ = happyReduce_37

action_59 _ = happyReduce_38

action_60 (65) = happyShift action_98
action_60 (76) = happyShift action_99
action_60 _ = happyFail

action_61 (57) = happyShift action_83
action_61 (58) = happyShift action_84
action_61 (59) = happyShift action_85
action_61 (60) = happyShift action_86
action_61 (61) = happyShift action_87
action_61 (64) = happyShift action_88
action_61 (67) = happyShift action_89
action_61 (70) = happyShift action_90
action_61 (77) = happyShift action_91
action_61 (35) = happyGoto action_97
action_61 (36) = happyGoto action_77
action_61 (37) = happyGoto action_78
action_61 (38) = happyGoto action_79
action_61 (39) = happyGoto action_80
action_61 (40) = happyGoto action_81
action_61 (41) = happyGoto action_82
action_61 _ = happyFail

action_62 (61) = happyShift action_96
action_62 (34) = happyGoto action_95
action_62 _ = happyFail

action_63 (70) = happyShift action_94
action_63 _ = happyFail

action_64 (70) = happyShift action_93
action_64 _ = happyFail

action_65 (57) = happyShift action_83
action_65 (58) = happyShift action_84
action_65 (59) = happyShift action_85
action_65 (60) = happyShift action_86
action_65 (61) = happyShift action_87
action_65 (64) = happyShift action_88
action_65 (67) = happyShift action_89
action_65 (70) = happyShift action_90
action_65 (77) = happyShift action_91
action_65 (35) = happyGoto action_92
action_65 (36) = happyGoto action_77
action_65 (37) = happyGoto action_78
action_65 (38) = happyGoto action_79
action_65 (39) = happyGoto action_80
action_65 (40) = happyGoto action_81
action_65 (41) = happyGoto action_82
action_65 _ = happyFail

action_66 (57) = happyShift action_83
action_66 (58) = happyShift action_84
action_66 (59) = happyShift action_85
action_66 (60) = happyShift action_86
action_66 (61) = happyShift action_87
action_66 (64) = happyShift action_88
action_66 (67) = happyShift action_89
action_66 (70) = happyShift action_90
action_66 (77) = happyShift action_91
action_66 (32) = happyGoto action_75
action_66 (35) = happyGoto action_76
action_66 (36) = happyGoto action_77
action_66 (37) = happyGoto action_78
action_66 (38) = happyGoto action_79
action_66 (39) = happyGoto action_80
action_66 (40) = happyGoto action_81
action_66 (41) = happyGoto action_82
action_66 _ = happyReduce_53

action_67 (70) = happyShift action_74
action_67 (42) = happyGoto action_73
action_67 _ = happyReduce_56

action_68 (22) = happyGoto action_72
action_68 _ = happyReduce_40

action_69 _ = happyReduce_23

action_70 (74) = happyShift action_71
action_70 (75) = happyShift action_16
action_70 _ = happyFail

action_71 _ = happyReduce_18

action_72 (49) = happyShift action_61
action_72 (51) = happyShift action_62
action_72 (52) = happyShift action_63
action_72 (54) = happyShift action_64
action_72 (55) = happyShift action_65
action_72 (56) = happyShift action_66
action_72 (61) = happyShift action_67
action_72 (72) = happyShift action_68
action_72 (73) = happyShift action_125
action_72 (20) = happyGoto action_50
action_72 (21) = happyGoto action_51
action_72 (23) = happyGoto action_52
action_72 (24) = happyGoto action_53
action_72 (26) = happyGoto action_54
action_72 (27) = happyGoto action_55
action_72 (29) = happyGoto action_56
action_72 (30) = happyGoto action_57
action_72 (31) = happyGoto action_58
action_72 (33) = happyGoto action_59
action_72 (34) = happyGoto action_60
action_72 _ = happyFail

action_73 (74) = happyShift action_124
action_73 _ = happyFail

action_74 (57) = happyShift action_83
action_74 (58) = happyShift action_84
action_74 (59) = happyShift action_85
action_74 (60) = happyShift action_86
action_74 (61) = happyShift action_87
action_74 (64) = happyShift action_88
action_74 (67) = happyShift action_89
action_74 (70) = happyShift action_90
action_74 (71) = happyShift action_123
action_74 (77) = happyShift action_91
action_74 (35) = happyGoto action_121
action_74 (36) = happyGoto action_77
action_74 (37) = happyGoto action_78
action_74 (38) = happyGoto action_79
action_74 (39) = happyGoto action_80
action_74 (40) = happyGoto action_81
action_74 (41) = happyGoto action_82
action_74 (43) = happyGoto action_122
action_74 _ = happyFail

action_75 (74) = happyShift action_120
action_75 _ = happyFail

action_76 (62) = happyShift action_104
action_76 _ = happyReduce_54

action_77 (63) = happyShift action_119
action_77 _ = happyReduce_58

action_78 (66) = happyShift action_117
action_78 (67) = happyShift action_118
action_78 _ = happyReduce_60

action_79 (68) = happyShift action_115
action_79 (69) = happyShift action_116
action_79 _ = happyReduce_62

action_80 _ = happyReduce_65

action_81 (76) = happyShift action_114
action_81 _ = happyReduce_68

action_82 _ = happyReduce_71

action_83 _ = happyReduce_77

action_84 _ = happyReduce_78

action_85 (61) = happyShift action_113
action_85 _ = happyFail

action_86 _ = happyReduce_80

action_87 (70) = happyShift action_74
action_87 (42) = happyGoto action_112
action_87 _ = happyReduce_74

action_88 _ = happyReduce_76

action_89 (57) = happyShift action_83
action_89 (58) = happyShift action_84
action_89 (59) = happyShift action_85
action_89 (60) = happyShift action_86
action_89 (61) = happyShift action_87
action_89 (64) = happyShift action_88
action_89 (67) = happyShift action_89
action_89 (70) = happyShift action_90
action_89 (77) = happyShift action_91
action_89 (39) = happyGoto action_111
action_89 (40) = happyGoto action_81
action_89 (41) = happyGoto action_82
action_89 _ = happyFail

action_90 (57) = happyShift action_83
action_90 (58) = happyShift action_84
action_90 (59) = happyShift action_85
action_90 (60) = happyShift action_86
action_90 (61) = happyShift action_87
action_90 (64) = happyShift action_88
action_90 (67) = happyShift action_89
action_90 (70) = happyShift action_90
action_90 (77) = happyShift action_91
action_90 (35) = happyGoto action_110
action_90 (36) = happyGoto action_77
action_90 (37) = happyGoto action_78
action_90 (38) = happyGoto action_79
action_90 (39) = happyGoto action_80
action_90 (40) = happyGoto action_81
action_90 (41) = happyGoto action_82
action_90 _ = happyFail

action_91 (57) = happyShift action_83
action_91 (58) = happyShift action_84
action_91 (59) = happyShift action_85
action_91 (60) = happyShift action_86
action_91 (61) = happyShift action_87
action_91 (64) = happyShift action_88
action_91 (67) = happyShift action_89
action_91 (70) = happyShift action_90
action_91 (77) = happyShift action_91
action_91 (39) = happyGoto action_109
action_91 (40) = happyGoto action_81
action_91 (41) = happyGoto action_82
action_91 _ = happyFail

action_92 (62) = happyShift action_104
action_92 (74) = happyShift action_108
action_92 _ = happyFail

action_93 (57) = happyShift action_83
action_93 (58) = happyShift action_84
action_93 (59) = happyShift action_85
action_93 (60) = happyShift action_86
action_93 (61) = happyShift action_87
action_93 (64) = happyShift action_88
action_93 (67) = happyShift action_89
action_93 (70) = happyShift action_90
action_93 (77) = happyShift action_91
action_93 (35) = happyGoto action_107
action_93 (36) = happyGoto action_77
action_93 (37) = happyGoto action_78
action_93 (38) = happyGoto action_79
action_93 (39) = happyGoto action_80
action_93 (40) = happyGoto action_81
action_93 (41) = happyGoto action_82
action_93 _ = happyFail

action_94 (57) = happyShift action_83
action_94 (58) = happyShift action_84
action_94 (59) = happyShift action_85
action_94 (60) = happyShift action_86
action_94 (61) = happyShift action_87
action_94 (64) = happyShift action_88
action_94 (67) = happyShift action_89
action_94 (70) = happyShift action_90
action_94 (77) = happyShift action_91
action_94 (35) = happyGoto action_106
action_94 (36) = happyGoto action_77
action_94 (37) = happyGoto action_78
action_94 (38) = happyGoto action_79
action_94 (39) = happyGoto action_80
action_94 (40) = happyGoto action_81
action_94 (41) = happyGoto action_82
action_94 _ = happyFail

action_95 (74) = happyShift action_105
action_95 (76) = happyShift action_99
action_95 _ = happyFail

action_96 _ = happyReduce_56

action_97 (50) = happyShift action_103
action_97 (62) = happyShift action_104
action_97 (25) = happyGoto action_102
action_97 _ = happyReduce_44

action_98 (57) = happyShift action_83
action_98 (58) = happyShift action_84
action_98 (59) = happyShift action_85
action_98 (60) = happyShift action_86
action_98 (61) = happyShift action_87
action_98 (64) = happyShift action_88
action_98 (67) = happyShift action_89
action_98 (70) = happyShift action_90
action_98 (77) = happyShift action_91
action_98 (35) = happyGoto action_101
action_98 (36) = happyGoto action_77
action_98 (37) = happyGoto action_78
action_98 (38) = happyGoto action_79
action_98 (39) = happyGoto action_80
action_98 (40) = happyGoto action_81
action_98 (41) = happyGoto action_82
action_98 _ = happyFail

action_99 (61) = happyShift action_100
action_99 _ = happyFail

action_100 _ = happyReduce_57

action_101 (62) = happyShift action_104
action_101 (74) = happyShift action_139
action_101 _ = happyFail

action_102 (74) = happyShift action_138
action_102 _ = happyFail

action_103 _ = happyReduce_45

action_104 (57) = happyShift action_83
action_104 (58) = happyShift action_84
action_104 (59) = happyShift action_85
action_104 (60) = happyShift action_86
action_104 (61) = happyShift action_87
action_104 (64) = happyShift action_88
action_104 (67) = happyShift action_89
action_104 (70) = happyShift action_90
action_104 (77) = happyShift action_91
action_104 (36) = happyGoto action_137
action_104 (37) = happyGoto action_78
action_104 (38) = happyGoto action_79
action_104 (39) = happyGoto action_80
action_104 (40) = happyGoto action_81
action_104 (41) = happyGoto action_82
action_104 _ = happyFail

action_105 _ = happyReduce_46

action_106 (62) = happyShift action_104
action_106 (71) = happyShift action_136
action_106 _ = happyFail

action_107 (62) = happyShift action_104
action_107 (71) = happyShift action_135
action_107 _ = happyFail

action_108 _ = happyReduce_51

action_109 _ = happyReduce_69

action_110 (62) = happyShift action_104
action_110 (71) = happyShift action_134
action_110 _ = happyFail

action_111 _ = happyReduce_70

action_112 _ = happyReduce_75

action_113 _ = happyReduce_79

action_114 (61) = happyShift action_133
action_114 _ = happyFail

action_115 (57) = happyShift action_83
action_115 (58) = happyShift action_84
action_115 (59) = happyShift action_85
action_115 (60) = happyShift action_86
action_115 (61) = happyShift action_87
action_115 (64) = happyShift action_88
action_115 (67) = happyShift action_89
action_115 (70) = happyShift action_90
action_115 (77) = happyShift action_91
action_115 (39) = happyGoto action_132
action_115 (40) = happyGoto action_81
action_115 (41) = happyGoto action_82
action_115 _ = happyFail

action_116 (57) = happyShift action_83
action_116 (58) = happyShift action_84
action_116 (59) = happyShift action_85
action_116 (60) = happyShift action_86
action_116 (61) = happyShift action_87
action_116 (64) = happyShift action_88
action_116 (67) = happyShift action_89
action_116 (70) = happyShift action_90
action_116 (77) = happyShift action_91
action_116 (39) = happyGoto action_131
action_116 (40) = happyGoto action_81
action_116 (41) = happyGoto action_82
action_116 _ = happyFail

action_117 (57) = happyShift action_83
action_117 (58) = happyShift action_84
action_117 (59) = happyShift action_85
action_117 (60) = happyShift action_86
action_117 (61) = happyShift action_87
action_117 (64) = happyShift action_88
action_117 (67) = happyShift action_89
action_117 (70) = happyShift action_90
action_117 (77) = happyShift action_91
action_117 (38) = happyGoto action_130
action_117 (39) = happyGoto action_80
action_117 (40) = happyGoto action_81
action_117 (41) = happyGoto action_82
action_117 _ = happyFail

action_118 (57) = happyShift action_83
action_118 (58) = happyShift action_84
action_118 (59) = happyShift action_85
action_118 (60) = happyShift action_86
action_118 (61) = happyShift action_87
action_118 (64) = happyShift action_88
action_118 (67) = happyShift action_89
action_118 (70) = happyShift action_90
action_118 (77) = happyShift action_91
action_118 (38) = happyGoto action_129
action_118 (39) = happyGoto action_80
action_118 (40) = happyGoto action_81
action_118 (41) = happyGoto action_82
action_118 _ = happyFail

action_119 (57) = happyShift action_83
action_119 (58) = happyShift action_84
action_119 (59) = happyShift action_85
action_119 (60) = happyShift action_86
action_119 (61) = happyShift action_87
action_119 (64) = happyShift action_88
action_119 (67) = happyShift action_89
action_119 (70) = happyShift action_90
action_119 (77) = happyShift action_91
action_119 (37) = happyGoto action_128
action_119 (38) = happyGoto action_79
action_119 (39) = happyGoto action_80
action_119 (40) = happyGoto action_81
action_119 (41) = happyGoto action_82
action_119 _ = happyFail

action_120 _ = happyReduce_52

action_121 (62) = happyShift action_104
action_121 _ = happyReduce_83

action_122 (71) = happyShift action_126
action_122 (75) = happyShift action_127
action_122 _ = happyFail

action_123 _ = happyReduce_81

action_124 _ = happyReduce_55

action_125 _ = happyReduce_39

action_126 _ = happyReduce_82

action_127 (57) = happyShift action_83
action_127 (58) = happyShift action_84
action_127 (59) = happyShift action_85
action_127 (60) = happyShift action_86
action_127 (61) = happyShift action_87
action_127 (64) = happyShift action_88
action_127 (67) = happyShift action_89
action_127 (70) = happyShift action_90
action_127 (77) = happyShift action_91
action_127 (35) = happyGoto action_142
action_127 (36) = happyGoto action_77
action_127 (37) = happyGoto action_78
action_127 (38) = happyGoto action_79
action_127 (39) = happyGoto action_80
action_127 (40) = happyGoto action_81
action_127 (41) = happyGoto action_82
action_127 _ = happyFail

action_128 (66) = happyShift action_117
action_128 (67) = happyShift action_118
action_128 _ = happyReduce_61

action_129 (68) = happyShift action_115
action_129 (69) = happyShift action_116
action_129 _ = happyReduce_64

action_130 (68) = happyShift action_115
action_130 (69) = happyShift action_116
action_130 _ = happyReduce_63

action_131 _ = happyReduce_67

action_132 _ = happyReduce_66

action_133 _ = happyReduce_72

action_134 _ = happyReduce_73

action_135 (72) = happyShift action_68
action_135 (21) = happyGoto action_141
action_135 _ = happyFail

action_136 (72) = happyShift action_68
action_136 (21) = happyGoto action_140
action_136 _ = happyFail

action_137 (63) = happyShift action_119
action_137 _ = happyReduce_59

action_138 _ = happyReduce_43

action_139 _ = happyReduce_42

action_140 (53) = happyShift action_144
action_140 (28) = happyGoto action_143
action_140 _ = happyReduce_48

action_141 _ = happyReduce_50

action_142 (62) = happyShift action_104
action_142 _ = happyReduce_84

action_143 _ = happyReduce_47

action_144 (72) = happyShift action_68
action_144 (21) = happyGoto action_145
action_144 _ = happyFail

action_145 _ = happyReduce_49

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (let (types,decls) = fromTypeDecls happy_var_1
                            in Program (reverse types) (reverse decls) (reverse happy_var_2)
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 ([]
	)

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_2 ++ happy_var_1
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  6 happyReduction_4
happyReduction_4 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal (TokenId happy_var_2))
	_
	 =  HappyAbsSyn5
		 (happy_var_3 happy_var_2
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happyMonadReduce 3 6 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return $ fmap (Decl l happy_var_1) happy_var_2)
	) (\r -> happyReturn (HappyAbsSyn5 r))

happyReduce_6 = happySpecReduce_0  7 happyReduction_6
happyReduction_6  =  HappyAbsSyn7
		 ([]
	)

happyReduce_7 = happySpecReduce_3  7 happyReduction_7
happyReduction_7 _
	(HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_2 : happy_var_1
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happyMonadReduce 2 8 happyReduction_8
happyReduction_8 ((HappyTerminal (TokenId happy_var_2)) `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return $ Field l happy_var_1 happy_var_2)
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_9 = happySpecReduce_1  9 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn9
		 (intType
	)

happyReduce_10 = happySpecReduce_1  9 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn9
		 (boolType
	)

happyReduce_11 = happySpecReduce_2  9 happyReduction_11
happyReduction_11 (HappyTerminal (TokenId happy_var_2))
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  10 happyReduction_12
happyReduction_12 _
	 =  HappyAbsSyn9
		 (intType
	)

happyReduce_13 = happySpecReduce_1  10 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn9
		 (boolType
	)

happyReduce_14 = happyMonadReduce 4 11 happyReduction_14
happyReduction_14 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return 
                                                    $ \x -> [TDef l x $ reverse happy_var_2])
	) (\r -> happyReturn (HappyAbsSyn11 r))

happyReduce_15 = happyMonadReduce 2 11 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return 
                                                    $ \x -> fmap (Decl 0 x) happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn11 r))

happyReduce_16 = happySpecReduce_0  12 happyReduction_16
happyReduction_16  =  HappyAbsSyn12
		 ([]
	)

happyReduce_17 = happySpecReduce_2  12 happyReduction_17
happyReduction_17 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_2 ++ happy_var_1
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happyMonadReduce 3 13 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return $ fmap (Declaration l happy_var_1) happy_var_2)
	) (\r -> happyReturn (HappyAbsSyn12 r))

happyReduce_19 = happySpecReduce_1  14 happyReduction_19
happyReduction_19 (HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn14
		 ([happy_var_1]
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  14 happyReduction_20
happyReduction_20 (HappyTerminal (TokenId happy_var_3))
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_3 : happy_var_1
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_0  15 happyReduction_21
happyReduction_21  =  HappyAbsSyn15
		 ([]
	)

happyReduce_22 = happySpecReduce_2  15 happyReduction_22
happyReduction_22 (HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_2 : happy_var_1
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happyMonadReduce 8 16 happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyAbsSyn22  happy_var_7) `HappyStk`
	(HappyAbsSyn12  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	(HappyTerminal (TokenId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return $ 
                                                    Function l happy_var_2 (reverse happy_var_3) (reverse happy_var_6) (reverse happy_var_7) happy_var_4)
	) (\r -> happyReturn (HappyAbsSyn16 r))

happyReduce_24 = happySpecReduce_2  17 happyReduction_24
happyReduction_24 _
	_
	 =  HappyAbsSyn7
		 ([]
	)

happyReduce_25 = happySpecReduce_3  17 happyReduction_25
happyReduction_25 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  18 happyReduction_26
happyReduction_26 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  18 happyReduction_27
happyReduction_27 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_3 : happy_var_1
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  19 happyReduction_28
happyReduction_28 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  19 happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn9
		 (voidType
	)

happyReduce_30 = happySpecReduce_1  20 happyReduction_30
happyReduction_30 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  20 happyReduction_31
happyReduction_31 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  20 happyReduction_32
happyReduction_32 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  20 happyReduction_33
happyReduction_33 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  20 happyReduction_34
happyReduction_34 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  20 happyReduction_35
happyReduction_35 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  20 happyReduction_36
happyReduction_36 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  20 happyReduction_37
happyReduction_37 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  20 happyReduction_38
happyReduction_38 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  21 happyReduction_39
happyReduction_39 _
	(HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (Block (reverse happy_var_2)
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_0  22 happyReduction_40
happyReduction_40  =  HappyAbsSyn22
		 ([]
	)

happyReduce_41 = happySpecReduce_2  22 happyReduction_41
happyReduction_41 (HappyAbsSyn20  happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_2 : happy_var_1
	)
happyReduction_41 _ _  = notHappyAtAll 

happyReduce_42 = happyMonadReduce 4 23 happyReduction_42
happyReduction_42 (_ `HappyStk`
	(HappyAbsSyn35  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn34  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return $ Asgn l happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn20 r))

happyReduce_43 = happyMonadReduce 4 24 happyReduction_43
happyReduction_43 (_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	(HappyAbsSyn35  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return $ Print l happy_var_2 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn20 r))

happyReduce_44 = happySpecReduce_0  25 happyReduction_44
happyReduction_44  =  HappyAbsSyn25
		 (False
	)

happyReduce_45 = happySpecReduce_1  25 happyReduction_45
happyReduction_45 _
	 =  HappyAbsSyn25
		 (True
	)

happyReduce_46 = happyMonadReduce 3 26 happyReduction_46
happyReduction_46 (_ `HappyStk`
	(HappyAbsSyn34  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return $ Read l happy_var_2)
	) (\r -> happyReturn (HappyAbsSyn20 r))

happyReduce_47 = happyMonadReduce 6 27 happyReduction_47
happyReduction_47 ((HappyAbsSyn28  happy_var_6) `HappyStk`
	(HappyAbsSyn20  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return $ Cond l happy_var_3 happy_var_5 happy_var_6)
	) (\r -> happyReturn (HappyAbsSyn20 r))

happyReduce_48 = happySpecReduce_0  28 happyReduction_48
happyReduction_48  =  HappyAbsSyn28
		 (Nothing
	)

happyReduce_49 = happySpecReduce_2  28 happyReduction_49
happyReduction_49 (HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn28
		 (Just happy_var_2
	)
happyReduction_49 _ _  = notHappyAtAll 

happyReduce_50 = happyMonadReduce 5 29 happyReduction_50
happyReduction_50 ((HappyAbsSyn20  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return $ Loop l happy_var_3 happy_var_5)
	) (\r -> happyReturn (HappyAbsSyn20 r))

happyReduce_51 = happyMonadReduce 3 30 happyReduction_51
happyReduction_51 (_ `HappyStk`
	(HappyAbsSyn35  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return $ Delete l happy_var_2)
	) (\r -> happyReturn (HappyAbsSyn20 r))

happyReduce_52 = happyMonadReduce 3 31 happyReduction_52
happyReduction_52 (_ `HappyStk`
	(HappyAbsSyn32  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return $ Ret l happy_var_2)
	) (\r -> happyReturn (HappyAbsSyn20 r))

happyReduce_53 = happySpecReduce_0  32 happyReduction_53
happyReduction_53  =  HappyAbsSyn32
		 (Nothing
	)

happyReduce_54 = happySpecReduce_1  32 happyReduction_54
happyReduction_54 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn32
		 (Just happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happyMonadReduce 3 33 happyReduction_55
happyReduction_55 (_ `HappyStk`
	(HappyAbsSyn42  happy_var_2) `HappyStk`
	(HappyTerminal (TokenId happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return $ InvocSt l happy_var_1 $ reverse happy_var_2)
	) (\r -> happyReturn (HappyAbsSyn20 r))

happyReduce_56 = happySpecReduce_1  34 happyReduction_56
happyReduction_56 (HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn34
		 (LValue Nothing happy_var_1 Nothing
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happyMonadReduce 3 34 happyReduction_57
happyReduction_57 ((HappyTerminal (TokenId happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn34  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return $ LValue (Just l) happy_var_3 (Just happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn34 r))

happyReduce_58 = happySpecReduce_1  35 happyReduction_58
happyReduction_58 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn35
		 (happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happyMonadReduce 3 35 happyReduction_59
happyReduction_59 ((HappyAbsSyn35  happy_var_3) `HappyStk`
	(HappyTerminal (TokenBoolOp happy_var_2)) `HappyStk`
	(HappyAbsSyn35  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return $ BinExp l happy_var_2 happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn35 r))

happyReduce_60 = happySpecReduce_1  36 happyReduction_60
happyReduction_60 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn35
		 (happy_var_1
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happyMonadReduce 3 36 happyReduction_61
happyReduction_61 ((HappyAbsSyn35  happy_var_3) `HappyStk`
	(HappyTerminal (TokenCmpOp happy_var_2)) `HappyStk`
	(HappyAbsSyn35  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return $ BinExp l happy_var_2 happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn35 r))

happyReduce_62 = happySpecReduce_1  37 happyReduction_62
happyReduction_62 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn35
		 (happy_var_1
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happyMonadReduce 3 37 happyReduction_63
happyReduction_63 ((HappyAbsSyn35  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return $ BinExp l "+" happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn35 r))

happyReduce_64 = happyMonadReduce 3 37 happyReduction_64
happyReduction_64 ((HappyAbsSyn35  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return $ BinExp l "-" happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn35 r))

happyReduce_65 = happySpecReduce_1  38 happyReduction_65
happyReduction_65 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn35
		 (happy_var_1
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happyMonadReduce 3 38 happyReduction_66
happyReduction_66 ((HappyAbsSyn35  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return $ BinExp l "*" happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn35 r))

happyReduce_67 = happyMonadReduce 3 38 happyReduction_67
happyReduction_67 ((HappyAbsSyn35  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return $ BinExp l "/" happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn35 r))

happyReduce_68 = happySpecReduce_1  39 happyReduction_68
happyReduction_68 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn35
		 (happy_var_1
	)
happyReduction_68 _  = notHappyAtAll 

happyReduce_69 = happyMonadReduce 2 39 happyReduction_69
happyReduction_69 ((HappyAbsSyn35  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return $ UExp l "!" happy_var_2)
	) (\r -> happyReturn (HappyAbsSyn35 r))

happyReduce_70 = happyMonadReduce 2 39 happyReduction_70
happyReduction_70 ((HappyAbsSyn35  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return $ UExp l "-" happy_var_2)
	) (\r -> happyReturn (HappyAbsSyn35 r))

happyReduce_71 = happySpecReduce_1  40 happyReduction_71
happyReduction_71 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn35
		 (happy_var_1
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happyMonadReduce 3 40 happyReduction_72
happyReduction_72 ((HappyTerminal (TokenId happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return $ DotExp l happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn35 r))

happyReduce_73 = happySpecReduce_3  41 happyReduction_73
happyReduction_73 _
	(HappyAbsSyn35  happy_var_2)
	_
	 =  HappyAbsSyn35
		 (happy_var_2
	)
happyReduction_73 _ _ _  = notHappyAtAll 

happyReduce_74 = happyMonadReduce 1 41 happyReduction_74
happyReduction_74 ((HappyTerminal (TokenId happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return $ IdExp l happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn35 r))

happyReduce_75 = happyMonadReduce 2 41 happyReduction_75
happyReduction_75 ((HappyAbsSyn42  happy_var_2) `HappyStk`
	(HappyTerminal (TokenId happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return $ InvocExp l happy_var_1 $ reverse happy_var_2)
	) (\r -> happyReturn (HappyAbsSyn35 r))

happyReduce_76 = happyMonadReduce 1 41 happyReduction_76
happyReduction_76 ((HappyTerminal (TokenNum happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return $ IntExp l happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn35 r))

happyReduce_77 = happyMonadReduce 1 41 happyReduction_77
happyReduction_77 (_ `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return $ TrueExp l)
	) (\r -> happyReturn (HappyAbsSyn35 r))

happyReduce_78 = happyMonadReduce 1 41 happyReduction_78
happyReduction_78 (_ `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return $ FalseExp l)
	) (\r -> happyReturn (HappyAbsSyn35 r))

happyReduce_79 = happyMonadReduce 2 41 happyReduction_79
happyReduction_79 ((HappyTerminal (TokenId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return $ NewExp l happy_var_2)
	) (\r -> happyReturn (HappyAbsSyn35 r))

happyReduce_80 = happyMonadReduce 1 41 happyReduction_80
happyReduction_80 (_ `HappyStk`
	happyRest) tk
	 = happyThen (( lineP >>= \l -> return $ NullExp l)
	) (\r -> happyReturn (HappyAbsSyn35 r))

happyReduce_81 = happySpecReduce_2  42 happyReduction_81
happyReduction_81 _
	_
	 =  HappyAbsSyn42
		 ([]
	)

happyReduce_82 = happySpecReduce_3  42 happyReduction_82
happyReduction_82 _
	(HappyAbsSyn42  happy_var_2)
	_
	 =  HappyAbsSyn42
		 (happy_var_2
	)
happyReduction_82 _ _ _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_1  43 happyReduction_83
happyReduction_83 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn42
		 ([happy_var_1]
	)
happyReduction_83 _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_3  43 happyReduction_84
happyReduction_84 (HappyAbsSyn35  happy_var_3)
	_
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn42
		 (happy_var_3 : happy_var_1
	)
happyReduction_84 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TokenEOF -> action 78 78 tk (HappyState action) sts stk;
	TokenStruct -> cont 44;
	TokenInt -> cont 45;
	TokenBool -> cont 46;
	TokenFun -> cont 47;
	TokenVoid -> cont 48;
	TokenPrint -> cont 49;
	TokenEndl -> cont 50;
	TokenRead -> cont 51;
	TokenIf -> cont 52;
	TokenElse -> cont 53;
	TokenWhile -> cont 54;
	TokenDelete -> cont 55;
	TokenReturn -> cont 56;
	TokenTrue -> cont 57;
	TokenFalse -> cont 58;
	TokenNew -> cont 59;
	TokenNull -> cont 60;
	TokenId happy_dollar_dollar -> cont 61;
	TokenBoolOp happy_dollar_dollar -> cont 62;
	TokenCmpOp happy_dollar_dollar -> cont 63;
	TokenNum happy_dollar_dollar -> cont 64;
	TokenEq -> cont 65;
	TokenPlus -> cont 66;
	TokenMinus -> cont 67;
	TokenTimes -> cont 68;
	TokenDiv -> cont 69;
	TokenOB -> cont 70;
	TokenCB -> cont 71;
	TokenLBrace -> cont 72;
	TokenRBrace -> cont 73;
	TokenSemi -> cont 74;
	TokenComma -> cont 75;
	TokenDot -> cont 76;
	TokenBang -> cont 77;
	_ -> happyError' tk
	})

happyError_ 78 tk = happyError' tk
happyError_ _ tk = happyError' tk

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = (>>=)
happyReturn :: () => a -> P a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 :: () => a -> P a
happyReturn1 = happyReturn
happyError' :: () => (Token) -> P a
happyError' tk = (\token -> happyError) tk

calc = happySomeParser where
  happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data TypeDecl = TDef Int Id [Field]
              | Decl Int Type Id

type ParseResult = Either String
type P a = ReaderT (String, Int) ParseResult a

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
keywords = [ ("struct", TokenStruct)
           , ("int", TokenInt)
           , ("bool", TokenBool)
           , ("fun", TokenFun)
           , ("void", TokenVoid)
           , ("print", TokenPrint)
           , ("read", TokenRead)
           , ("if", TokenIf)
           , ("else", TokenElse)
           , ("while", TokenWhile)
           , ("delete", TokenDelete)
           , ("return", TokenReturn)
           , ("true", TokenTrue)
           , ("false", TokenFalse)
           , ("new", TokenNew)
           , ("null", TokenNull)
           , ("endl", TokenEndl) ]
    

charTks :: [(Char, Token)]
charTks = [ ('=', TokenEq)
          , ('+', TokenPlus)
          , ('-', TokenMinus)
          , ('*', TokenTimes)
          , ('/', TokenDiv)
          , ('(', TokenOB)
          , (')', TokenCB)
          , ('{', TokenLBrace)
          , ('}', TokenRBrace)
          , (';', TokenSemi)
          , (',', TokenComma)
          , ('.', TokenDot)
          , ('<', TokenCmpOp "<")
          , ('>', TokenCmpOp ">")
          , ('!', TokenBang) ]

fromTypeDecls :: [TypeDecl] -> ([TypeDef], [Declaration])
fromTypeDecls = foldr foldFun ([],[])
    where foldFun (TDef l i f) (ts, ds) = ((TypeDef l i f):ts, ds)
          foldFun (Decl l t i) (ts, ds) = (ts, (Declaration l t i):ds)

mkP :: (String -> Int -> ParseResult a) -> P a
mkP = ReaderT . uncurry

runP :: P a -> String -> Int -> ParseResult a
runP f s l = runReaderT f (s, l)

lineP :: P Int
lineP = asks snd >>= return

happyError :: P a
happyError = lineP >>= \l -> fail (show l ++ ": Parse error")

lexer :: (Token -> P a) -> P a
lexer cont = mkP lexer'
    where lexer' [] = returnToken cont TokenEOF []
          lexer' ('#':cs) = lexer' (dropWhile (/= '\n') cs)
          lexer' s = nextLex cont s

returnToken :: (t -> P a) -> t -> String -> Int -> ParseResult a
returnToken cont tok = runP (cont tok)

nextLex :: (Token -> P a) -> String -> Int -> ParseResult a
nextLex cont s = case s of
        ('\n':cs) -> \line -> returnToken lexer cont cs (line+1)
        (c:cs)
            | isSpace c -> runP (lexer cont) cs
            | isAlpha c -> lexText cont (c:cs)
            | isDigit c -> lexNum cont (c:cs)
        ('<':'=':cs) -> returnToken cont (TokenCmpOp "<=") cs
        ('>':'=':cs) -> returnToken cont (TokenCmpOp ">=") cs
        ('=':'=':cs) -> returnToken cont (TokenCmpOp "==") cs
        ('!':'=':cs) -> returnToken cont (TokenCmpOp "!=") cs
        ('&':'&':cs) -> returnToken cont (TokenBoolOp "&&") cs
        ('|':'|':cs) -> returnToken cont (TokenBoolOp "||") cs
        (c:cs)
            | isJust (charTk c) -> returnToken cont (fromJust $ charTk c) cs
            | otherwise -> lexError ("Unexpected token " ++ [c]) s
    where charTk c = snd <$> find ((==) c . fst) charTks

lexNum :: (Token -> P a) -> String -> Int -> ParseResult a
lexNum cont s = returnToken cont (TokenNum (read num)) rest
    where (num, rest) = span isDigit s

lexText :: (Token -> P a) -> String -> Int -> ParseResult a
lexText cont s = returnToken cont
        (fromMaybe (TokenId word) (snd <$> find ((==) word . fst) keywords)) rest
    where (word, rest) = span (\x -> isAlpha x || isDigit x) s

lexError :: String -> String -> Int -> ParseResult a
lexError err = runP (lineP >>= \l -> fail (show l ++ ": " ++ err))
 
parse :: String -> Program
parse fileString = either (\x -> error x) id $ runP calc fileString 1
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
