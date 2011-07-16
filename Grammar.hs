-- parser produced by Happy Version 1.11

module Grammar where
import Types
import qualified CheckDup
import qualified CheckDecl
import qualified CheckTypes
import Char
import Optimizations
import qualified CheckIdentsUse
import Namemangling

data HappyAbsSyn 
	= HappyTerminal Token
	| HappyErrorToken Int
	| HappyAbsSyn4 (Program)
	| HappyAbsSyn5 (Declarations)
	| HappyAbsSyn6 (Declaration)
	| HappyAbsSyn7 (Parameters)
	| HappyAbsSyn8 (Parameter)
	| HappyAbsSyn9 (Statements)
	| HappyAbsSyn12 (Statement)
	| HappyAbsSyn13 (Exp)
	| HappyAbsSyn14 (Exps)

type HappyReduction = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> P(HappyAbsSyn))
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> P(HappyAbsSyn))] 
	-> HappyStk HappyAbsSyn 
	-> P(HappyAbsSyn)

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
 action_108 :: Int -> HappyReduction

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
 happyReduce_51 :: HappyReduction

action_0 (15) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail

action_1 (15) = happyShift action_2
action_1 _ = happyFail

action_2 (16) = happyShift action_4
action_2 _ = happyFail

action_3 (53) = happyAccept
action_3 _ = happyFail

action_4 (16) = happyShift action_7
action_4 (17) = happyShift action_8
action_4 (18) = happyShift action_9
action_4 (23) = happyShift action_10
action_4 (5) = happyGoto action_5
action_4 (6) = happyGoto action_6
action_4 _ = happyReduce_2

action_5 (32) = happyShift action_17
action_5 (9) = happyGoto action_16
action_5 _ = happyFail

action_6 (51) = happyShift action_15
action_6 _ = happyReduce_3

action_7 (48) = happyShift action_14
action_7 _ = happyFail

action_8 (16) = happyShift action_13
action_8 _ = happyFail

action_9 (16) = happyShift action_12
action_9 _ = happyFail

action_10 (16) = happyShift action_11
action_10 _ = happyFail

action_11 _ = happyReduce_6

action_12 (49) = happyShift action_37
action_12 _ = happyFail

action_13 (49) = happyShift action_36
action_13 _ = happyFail

action_14 (16) = happyShift action_29
action_14 (20) = happyShift action_30
action_14 (21) = happyShift action_31
action_14 (22) = happyShift action_32
action_14 (36) = happyShift action_33
action_14 (45) = happyShift action_34
action_14 (49) = happyShift action_35
action_14 (13) = happyGoto action_28
action_14 _ = happyFail

action_15 (16) = happyShift action_7
action_15 (17) = happyShift action_8
action_15 (18) = happyShift action_9
action_15 (23) = happyShift action_10
action_15 (5) = happyGoto action_27
action_15 (6) = happyGoto action_6
action_15 _ = happyReduce_2

action_16 _ = happyReduce_1

action_17 (16) = happyShift action_20
action_17 (19) = happyShift action_21
action_17 (24) = happyShift action_22
action_17 (27) = happyShift action_23
action_17 (29) = happyShift action_24
action_17 (30) = happyShift action_25
action_17 (31) = happyShift action_26
action_17 (10) = happyGoto action_18
action_17 (12) = happyGoto action_19
action_17 _ = happyReduce_14

action_18 (33) = happyShift action_67
action_18 _ = happyFail

action_19 (51) = happyShift action_66
action_19 _ = happyReduce_15

action_20 (37) = happyShift action_64
action_20 (49) = happyShift action_65
action_20 _ = happyFail

action_21 (16) = happyShift action_29
action_21 (20) = happyShift action_30
action_21 (21) = happyShift action_31
action_21 (22) = happyShift action_32
action_21 (36) = happyShift action_33
action_21 (45) = happyShift action_34
action_21 (49) = happyShift action_35
action_21 (13) = happyGoto action_63
action_21 _ = happyFail

action_22 (16) = happyShift action_29
action_22 (20) = happyShift action_30
action_22 (21) = happyShift action_31
action_22 (22) = happyShift action_32
action_22 (36) = happyShift action_33
action_22 (45) = happyShift action_34
action_22 (49) = happyShift action_35
action_22 (13) = happyGoto action_62
action_22 _ = happyFail

action_23 (16) = happyShift action_29
action_23 (20) = happyShift action_30
action_23 (21) = happyShift action_31
action_23 (22) = happyShift action_32
action_23 (36) = happyShift action_33
action_23 (45) = happyShift action_34
action_23 (49) = happyShift action_35
action_23 (13) = happyGoto action_61
action_23 _ = happyFail

action_24 (16) = happyShift action_59
action_24 (22) = happyShift action_60
action_24 _ = happyFail

action_25 _ = happyReduce_27

action_26 (16) = happyShift action_29
action_26 (20) = happyShift action_30
action_26 (21) = happyShift action_31
action_26 (22) = happyShift action_32
action_26 (36) = happyShift action_33
action_26 (45) = happyShift action_34
action_26 (49) = happyShift action_35
action_26 (13) = happyGoto action_58
action_26 _ = happyFail

action_27 _ = happyReduce_4

action_28 (34) = happyShift action_46
action_28 (35) = happyShift action_47
action_28 (38) = happyShift action_48
action_28 (39) = happyShift action_49
action_28 (40) = happyShift action_50
action_28 (41) = happyShift action_51
action_28 (42) = happyShift action_52
action_28 (43) = happyShift action_53
action_28 (44) = happyShift action_54
action_28 (45) = happyShift action_55
action_28 (46) = happyShift action_56
action_28 (47) = happyShift action_57
action_28 _ = happyReduce_5

action_29 (49) = happyShift action_45
action_29 _ = happyReduce_46

action_30 _ = happyReduce_39

action_31 _ = happyReduce_29

action_32 _ = happyReduce_45

action_33 (16) = happyShift action_29
action_33 (20) = happyShift action_30
action_33 (21) = happyShift action_31
action_33 (22) = happyShift action_32
action_33 (36) = happyShift action_33
action_33 (45) = happyShift action_34
action_33 (49) = happyShift action_35
action_33 (13) = happyGoto action_44
action_33 _ = happyFail

action_34 (16) = happyShift action_29
action_34 (20) = happyShift action_30
action_34 (21) = happyShift action_31
action_34 (22) = happyShift action_32
action_34 (36) = happyShift action_33
action_34 (45) = happyShift action_34
action_34 (49) = happyShift action_35
action_34 (13) = happyGoto action_43
action_34 _ = happyFail

action_35 (16) = happyShift action_29
action_35 (20) = happyShift action_30
action_35 (21) = happyShift action_31
action_35 (22) = happyShift action_32
action_35 (36) = happyShift action_33
action_35 (45) = happyShift action_34
action_35 (49) = happyShift action_35
action_35 (13) = happyGoto action_42
action_35 _ = happyFail

action_36 (23) = happyShift action_40
action_36 (7) = happyGoto action_41
action_36 (8) = happyGoto action_39
action_36 _ = happyReduce_9

action_37 (23) = happyShift action_40
action_37 (7) = happyGoto action_38
action_37 (8) = happyGoto action_39
action_37 _ = happyReduce_9

action_38 (50) = happyShift action_91
action_38 _ = happyFail

action_39 (51) = happyShift action_90
action_39 _ = happyReduce_10

action_40 (16) = happyShift action_89
action_40 _ = happyFail

action_41 (50) = happyShift action_88
action_41 _ = happyFail

action_42 (34) = happyShift action_46
action_42 (35) = happyShift action_47
action_42 (38) = happyShift action_48
action_42 (39) = happyShift action_49
action_42 (40) = happyShift action_50
action_42 (41) = happyShift action_51
action_42 (42) = happyShift action_52
action_42 (43) = happyShift action_53
action_42 (44) = happyShift action_54
action_42 (45) = happyShift action_55
action_42 (46) = happyShift action_56
action_42 (47) = happyShift action_57
action_42 (50) = happyShift action_87
action_42 _ = happyFail

action_43 (46) = happyShift action_56
action_43 (47) = happyShift action_57
action_43 _ = happyReduce_44

action_44 (44) = happyShift action_54
action_44 (45) = happyShift action_55
action_44 (46) = happyShift action_56
action_44 (47) = happyShift action_57
action_44 _ = happyReduce_32

action_45 (16) = happyShift action_29
action_45 (20) = happyShift action_30
action_45 (21) = happyShift action_31
action_45 (22) = happyShift action_32
action_45 (36) = happyShift action_33
action_45 (45) = happyShift action_34
action_45 (49) = happyShift action_35
action_45 (13) = happyGoto action_69
action_45 (14) = happyGoto action_86
action_45 _ = happyReduce_49

action_46 (16) = happyShift action_29
action_46 (20) = happyShift action_30
action_46 (21) = happyShift action_31
action_46 (22) = happyShift action_32
action_46 (36) = happyShift action_33
action_46 (45) = happyShift action_34
action_46 (49) = happyShift action_35
action_46 (13) = happyGoto action_85
action_46 _ = happyFail

action_47 (16) = happyShift action_29
action_47 (20) = happyShift action_30
action_47 (21) = happyShift action_31
action_47 (22) = happyShift action_32
action_47 (36) = happyShift action_33
action_47 (45) = happyShift action_34
action_47 (49) = happyShift action_35
action_47 (13) = happyGoto action_84
action_47 _ = happyFail

action_48 (16) = happyShift action_29
action_48 (20) = happyShift action_30
action_48 (21) = happyShift action_31
action_48 (22) = happyShift action_32
action_48 (36) = happyShift action_33
action_48 (45) = happyShift action_34
action_48 (49) = happyShift action_35
action_48 (13) = happyGoto action_83
action_48 _ = happyFail

action_49 (16) = happyShift action_29
action_49 (20) = happyShift action_30
action_49 (21) = happyShift action_31
action_49 (22) = happyShift action_32
action_49 (36) = happyShift action_33
action_49 (45) = happyShift action_34
action_49 (49) = happyShift action_35
action_49 (13) = happyGoto action_82
action_49 _ = happyFail

action_50 (16) = happyShift action_29
action_50 (20) = happyShift action_30
action_50 (21) = happyShift action_31
action_50 (22) = happyShift action_32
action_50 (36) = happyShift action_33
action_50 (45) = happyShift action_34
action_50 (49) = happyShift action_35
action_50 (13) = happyGoto action_81
action_50 _ = happyFail

action_51 (16) = happyShift action_29
action_51 (20) = happyShift action_30
action_51 (21) = happyShift action_31
action_51 (22) = happyShift action_32
action_51 (36) = happyShift action_33
action_51 (45) = happyShift action_34
action_51 (49) = happyShift action_35
action_51 (13) = happyGoto action_80
action_51 _ = happyFail

action_52 (16) = happyShift action_29
action_52 (20) = happyShift action_30
action_52 (21) = happyShift action_31
action_52 (22) = happyShift action_32
action_52 (36) = happyShift action_33
action_52 (45) = happyShift action_34
action_52 (49) = happyShift action_35
action_52 (13) = happyGoto action_79
action_52 _ = happyFail

action_53 (16) = happyShift action_29
action_53 (20) = happyShift action_30
action_53 (21) = happyShift action_31
action_53 (22) = happyShift action_32
action_53 (36) = happyShift action_33
action_53 (45) = happyShift action_34
action_53 (49) = happyShift action_35
action_53 (13) = happyGoto action_78
action_53 _ = happyFail

action_54 (16) = happyShift action_29
action_54 (20) = happyShift action_30
action_54 (21) = happyShift action_31
action_54 (22) = happyShift action_32
action_54 (36) = happyShift action_33
action_54 (45) = happyShift action_34
action_54 (49) = happyShift action_35
action_54 (13) = happyGoto action_77
action_54 _ = happyFail

action_55 (16) = happyShift action_29
action_55 (20) = happyShift action_30
action_55 (21) = happyShift action_31
action_55 (22) = happyShift action_32
action_55 (36) = happyShift action_33
action_55 (45) = happyShift action_34
action_55 (49) = happyShift action_35
action_55 (13) = happyGoto action_76
action_55 _ = happyFail

action_56 (16) = happyShift action_29
action_56 (20) = happyShift action_30
action_56 (21) = happyShift action_31
action_56 (22) = happyShift action_32
action_56 (36) = happyShift action_33
action_56 (45) = happyShift action_34
action_56 (49) = happyShift action_35
action_56 (13) = happyGoto action_75
action_56 _ = happyFail

action_57 (16) = happyShift action_29
action_57 (20) = happyShift action_30
action_57 (21) = happyShift action_31
action_57 (22) = happyShift action_32
action_57 (36) = happyShift action_33
action_57 (45) = happyShift action_34
action_57 (49) = happyShift action_35
action_57 (13) = happyGoto action_74
action_57 _ = happyFail

action_58 (34) = happyShift action_46
action_58 (35) = happyShift action_47
action_58 (38) = happyShift action_48
action_58 (39) = happyShift action_49
action_58 (40) = happyShift action_50
action_58 (41) = happyShift action_51
action_58 (42) = happyShift action_52
action_58 (43) = happyShift action_53
action_58 (44) = happyShift action_54
action_58 (45) = happyShift action_55
action_58 (46) = happyShift action_56
action_58 (47) = happyShift action_57
action_58 _ = happyReduce_26

action_59 _ = happyReduce_24

action_60 _ = happyReduce_25

action_61 (28) = happyShift action_73
action_61 (34) = happyShift action_46
action_61 (35) = happyShift action_47
action_61 (38) = happyShift action_48
action_61 (39) = happyShift action_49
action_61 (40) = happyShift action_50
action_61 (41) = happyShift action_51
action_61 (42) = happyShift action_52
action_61 (43) = happyShift action_53
action_61 (44) = happyShift action_54
action_61 (45) = happyShift action_55
action_61 (46) = happyShift action_56
action_61 (47) = happyShift action_57
action_61 _ = happyFail

action_62 (25) = happyShift action_72
action_62 (34) = happyShift action_46
action_62 (35) = happyShift action_47
action_62 (38) = happyShift action_48
action_62 (39) = happyShift action_49
action_62 (40) = happyShift action_50
action_62 (41) = happyShift action_51
action_62 (42) = happyShift action_52
action_62 (43) = happyShift action_53
action_62 (44) = happyShift action_54
action_62 (45) = happyShift action_55
action_62 (46) = happyShift action_56
action_62 (47) = happyShift action_57
action_62 _ = happyFail

action_63 (34) = happyShift action_46
action_63 (35) = happyShift action_47
action_63 (38) = happyShift action_48
action_63 (39) = happyShift action_49
action_63 (40) = happyShift action_50
action_63 (41) = happyShift action_51
action_63 (42) = happyShift action_52
action_63 (43) = happyShift action_53
action_63 (44) = happyShift action_54
action_63 (45) = happyShift action_55
action_63 (46) = happyShift action_56
action_63 (47) = happyShift action_57
action_63 _ = happyReduce_28

action_64 (16) = happyShift action_29
action_64 (20) = happyShift action_30
action_64 (21) = happyShift action_31
action_64 (22) = happyShift action_32
action_64 (36) = happyShift action_33
action_64 (45) = happyShift action_34
action_64 (49) = happyShift action_35
action_64 (13) = happyGoto action_71
action_64 _ = happyFail

action_65 (16) = happyShift action_29
action_65 (20) = happyShift action_30
action_65 (21) = happyShift action_31
action_65 (22) = happyShift action_32
action_65 (36) = happyShift action_33
action_65 (45) = happyShift action_34
action_65 (49) = happyShift action_35
action_65 (13) = happyGoto action_69
action_65 (14) = happyGoto action_70
action_65 _ = happyReduce_49

action_66 (16) = happyShift action_20
action_66 (19) = happyShift action_21
action_66 (24) = happyShift action_22
action_66 (27) = happyShift action_23
action_66 (29) = happyShift action_24
action_66 (30) = happyShift action_25
action_66 (31) = happyShift action_26
action_66 (10) = happyGoto action_68
action_66 (12) = happyGoto action_19
action_66 _ = happyReduce_14

action_67 _ = happyReduce_13

action_68 _ = happyReduce_16

action_69 (34) = happyShift action_46
action_69 (35) = happyShift action_47
action_69 (38) = happyShift action_48
action_69 (39) = happyShift action_49
action_69 (40) = happyShift action_50
action_69 (41) = happyShift action_51
action_69 (42) = happyShift action_52
action_69 (43) = happyShift action_53
action_69 (44) = happyShift action_54
action_69 (45) = happyShift action_55
action_69 (46) = happyShift action_56
action_69 (47) = happyShift action_57
action_69 (52) = happyShift action_101
action_69 _ = happyReduce_50

action_70 (50) = happyShift action_100
action_70 _ = happyFail

action_71 (34) = happyShift action_46
action_71 (35) = happyShift action_47
action_71 (38) = happyShift action_48
action_71 (39) = happyShift action_49
action_71 (40) = happyShift action_50
action_71 (41) = happyShift action_51
action_71 (42) = happyShift action_52
action_71 (43) = happyShift action_53
action_71 (44) = happyShift action_54
action_71 (45) = happyShift action_55
action_71 (46) = happyShift action_56
action_71 (47) = happyShift action_57
action_71 _ = happyReduce_19

action_72 (16) = happyShift action_20
action_72 (19) = happyShift action_21
action_72 (24) = happyShift action_22
action_72 (27) = happyShift action_23
action_72 (29) = happyShift action_24
action_72 (30) = happyShift action_25
action_72 (31) = happyShift action_26
action_72 (32) = happyShift action_17
action_72 (9) = happyGoto action_96
action_72 (11) = happyGoto action_99
action_72 (12) = happyGoto action_98
action_72 _ = happyFail

action_73 (16) = happyShift action_20
action_73 (19) = happyShift action_21
action_73 (24) = happyShift action_22
action_73 (27) = happyShift action_23
action_73 (29) = happyShift action_24
action_73 (30) = happyShift action_25
action_73 (31) = happyShift action_26
action_73 (32) = happyShift action_17
action_73 (9) = happyGoto action_96
action_73 (11) = happyGoto action_97
action_73 (12) = happyGoto action_98
action_73 _ = happyFail

action_74 _ = happyReduce_43

action_75 _ = happyReduce_42

action_76 (46) = happyShift action_56
action_76 (47) = happyShift action_57
action_76 _ = happyReduce_41

action_77 (46) = happyShift action_56
action_77 (47) = happyShift action_57
action_77 _ = happyReduce_40

action_78 (34) = happyShift action_46
action_78 (35) = happyShift action_47
action_78 (38) = happyFail
action_78 (39) = happyFail
action_78 (40) = happyFail
action_78 (41) = happyFail
action_78 (42) = happyFail
action_78 (43) = happyFail
action_78 (44) = happyShift action_54
action_78 (45) = happyShift action_55
action_78 (46) = happyShift action_56
action_78 (47) = happyShift action_57
action_78 _ = happyReduce_38

action_79 (34) = happyShift action_46
action_79 (35) = happyShift action_47
action_79 (38) = happyFail
action_79 (39) = happyFail
action_79 (40) = happyFail
action_79 (41) = happyFail
action_79 (42) = happyFail
action_79 (43) = happyFail
action_79 (44) = happyShift action_54
action_79 (45) = happyShift action_55
action_79 (46) = happyShift action_56
action_79 (47) = happyShift action_57
action_79 _ = happyReduce_37

action_80 (34) = happyShift action_46
action_80 (35) = happyShift action_47
action_80 (38) = happyFail
action_80 (39) = happyFail
action_80 (40) = happyFail
action_80 (41) = happyFail
action_80 (42) = happyFail
action_80 (43) = happyFail
action_80 (44) = happyShift action_54
action_80 (45) = happyShift action_55
action_80 (46) = happyShift action_56
action_80 (47) = happyShift action_57
action_80 _ = happyReduce_36

action_81 (34) = happyShift action_46
action_81 (35) = happyShift action_47
action_81 (38) = happyFail
action_81 (39) = happyFail
action_81 (40) = happyFail
action_81 (41) = happyFail
action_81 (42) = happyFail
action_81 (43) = happyFail
action_81 (44) = happyShift action_54
action_81 (45) = happyShift action_55
action_81 (46) = happyShift action_56
action_81 (47) = happyShift action_57
action_81 _ = happyReduce_35

action_82 (34) = happyShift action_46
action_82 (35) = happyShift action_47
action_82 (38) = happyFail
action_82 (39) = happyFail
action_82 (40) = happyFail
action_82 (41) = happyFail
action_82 (42) = happyFail
action_82 (43) = happyFail
action_82 (44) = happyShift action_54
action_82 (45) = happyShift action_55
action_82 (46) = happyShift action_56
action_82 (47) = happyShift action_57
action_82 _ = happyReduce_34

action_83 (34) = happyShift action_46
action_83 (35) = happyShift action_47
action_83 (38) = happyFail
action_83 (39) = happyFail
action_83 (40) = happyFail
action_83 (41) = happyFail
action_83 (42) = happyFail
action_83 (43) = happyFail
action_83 (44) = happyShift action_54
action_83 (45) = happyShift action_55
action_83 (46) = happyShift action_56
action_83 (47) = happyShift action_57
action_83 _ = happyReduce_33

action_84 (44) = happyShift action_54
action_84 (45) = happyShift action_55
action_84 (46) = happyShift action_56
action_84 (47) = happyShift action_57
action_84 _ = happyReduce_31

action_85 (44) = happyShift action_54
action_85 (45) = happyShift action_55
action_85 (46) = happyShift action_56
action_85 (47) = happyShift action_57
action_85 _ = happyReduce_30

action_86 (50) = happyShift action_95
action_86 _ = happyFail

action_87 _ = happyReduce_48

action_88 (16) = happyShift action_7
action_88 (17) = happyShift action_8
action_88 (18) = happyShift action_9
action_88 (23) = happyShift action_10
action_88 (5) = happyGoto action_94
action_88 (6) = happyGoto action_6
action_88 _ = happyReduce_2

action_89 _ = happyReduce_12

action_90 (23) = happyShift action_40
action_90 (7) = happyGoto action_93
action_90 (8) = happyGoto action_39
action_90 _ = happyReduce_9

action_91 (48) = happyShift action_92
action_91 _ = happyFail

action_92 (23) = happyShift action_105
action_92 _ = happyFail

action_93 _ = happyReduce_11

action_94 (32) = happyShift action_17
action_94 (9) = happyGoto action_104
action_94 _ = happyFail

action_95 _ = happyReduce_47

action_96 _ = happyReduce_18

action_97 _ = happyReduce_22

action_98 _ = happyReduce_17

action_99 (26) = happyShift action_103
action_99 _ = happyReduce_21

action_100 _ = happyReduce_23

action_101 (16) = happyShift action_29
action_101 (20) = happyShift action_30
action_101 (21) = happyShift action_31
action_101 (22) = happyShift action_32
action_101 (36) = happyShift action_33
action_101 (45) = happyShift action_34
action_101 (49) = happyShift action_35
action_101 (13) = happyGoto action_69
action_101 (14) = happyGoto action_102
action_101 _ = happyReduce_49

action_102 _ = happyReduce_51

action_103 (16) = happyShift action_20
action_103 (19) = happyShift action_21
action_103 (24) = happyShift action_22
action_103 (27) = happyShift action_23
action_103 (29) = happyShift action_24
action_103 (30) = happyShift action_25
action_103 (31) = happyShift action_26
action_103 (32) = happyShift action_17
action_103 (9) = happyGoto action_96
action_103 (11) = happyGoto action_107
action_103 (12) = happyGoto action_98
action_103 _ = happyFail

action_104 _ = happyReduce_7

action_105 (16) = happyShift action_7
action_105 (17) = happyShift action_8
action_105 (18) = happyShift action_9
action_105 (23) = happyShift action_10
action_105 (5) = happyGoto action_106
action_105 (6) = happyGoto action_6
action_105 _ = happyReduce_2

action_106 (32) = happyShift action_17
action_106 (9) = happyGoto action_108
action_106 _ = happyFail

action_107 _ = happyReduce_20

action_108 _ = happyReduce_8

happyReduce_1 = happyMonadReduce 4 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn9  happy_var_4) `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	(HappyTerminal (TId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (Program (Proc happy_var_2 [] happy_var_3 happy_var_4 l)) s l
	) (\r -> happyReturn (HappyAbsSyn4 r))

happyReduce_2 = happySpecReduce_0 5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 ([]
	)

happyReduce_3 = happySpecReduce_1 5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3 5 happyReduction_4
happyReduction_4 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happyMonadReduce 3 6 happyReduction_5
happyReduction_5 ((HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TId happy_var_1)) `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (Const happy_var_1 happy_var_3 l) s l
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_6 = happyMonadReduce 2 6 happyReduction_6
happyReduction_6 ((HappyTerminal (TId happy_var_2)) `HappyStk`
	(HappyTerminal (TType happy_var_1)) `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (Var happy_var_2 happy_var_1 l) s l
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_7 = happyMonadReduce 7 6 happyReduction_7
happyReduction_7 ((HappyAbsSyn9  happy_var_7) `HappyStk`
	(HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (Proc happy_var_2 happy_var_4 happy_var_6 happy_var_7 l) s l
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_8 = happyMonadReduce 9 6 happyReduction_8
happyReduction_8 ((HappyAbsSyn9  happy_var_9) `HappyStk`
	(HappyAbsSyn5  happy_var_8) `HappyStk`
	(HappyTerminal (TType happy_var_7)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP  (Fun happy_var_2 happy_var_4 happy_var_7 happy_var_8 happy_var_9 l) s l
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_9 = happySpecReduce_0 7 happyReduction_9
happyReduction_9  =  HappyAbsSyn7
		 ([]
	)

happyReduce_10 = happySpecReduce_1 7 happyReduction_10
happyReduction_10 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3 7 happyReduction_11
happyReduction_11 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1:happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happyMonadReduce 2 8 happyReduction_12
happyReduction_12 ((HappyTerminal (TId happy_var_2)) `HappyStk`
	(HappyTerminal (TType happy_var_1)) `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (Param happy_var_2 happy_var_1 l) s l
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_13 = happySpecReduce_3 9 happyReduction_13
happyReduction_13 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_0 10 happyReduction_14
happyReduction_14  =  HappyAbsSyn9
		 ([]
	)

happyReduce_15 = happySpecReduce_1 10 happyReduction_15
happyReduction_15 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3 10 happyReduction_16
happyReduction_16 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 : happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1 11 happyReduction_17
happyReduction_17 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1 11 happyReduction_18
happyReduction_18 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happyMonadReduce 3 12 happyReduction_19
happyReduction_19 ((HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TId happy_var_1)) `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (Assign happy_var_1 happy_var_3 l) s l
	) (\r -> happyReturn (HappyAbsSyn12 r))

happyReduce_20 = happyMonadReduce 6 12 happyReduction_20
happyReduction_20 ((HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (IfThenElse happy_var_2 happy_var_4 happy_var_6 l) s l
	) (\r -> happyReturn (HappyAbsSyn12 r))

happyReduce_21 = happyMonadReduce 4 12 happyReduction_21
happyReduction_21 ((HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (IfThen happy_var_2 happy_var_4 l) s l
	) (\r -> happyReturn (HappyAbsSyn12 r))

happyReduce_22 = happyMonadReduce 4 12 happyReduction_22
happyReduction_22 ((HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (WhileDo happy_var_2 happy_var_4 l) s l
	) (\r -> happyReturn (HappyAbsSyn12 r))

happyReduce_23 = happyMonadReduce 4 12 happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TId happy_var_1)) `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (Call happy_var_1 happy_var_3 l) s l
	) (\r -> happyReturn (HappyAbsSyn12 r))

happyReduce_24 = happyMonadReduce 2 12 happyReduction_24
happyReduction_24 ((HappyTerminal (TId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (Write (Id happy_var_2 l) l) s l
	) (\r -> happyReturn (HappyAbsSyn12 r))

happyReduce_25 = happyMonadReduce 2 12 happyReduction_25
happyReduction_25 ((HappyTerminal (TCString happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (Write (CString happy_var_2 l) l) s l
	) (\r -> happyReturn (HappyAbsSyn12 r))

happyReduce_26 = happyMonadReduce 2 12 happyReduction_26
happyReduction_26 ((HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (WriteNum happy_var_2 l) s l
	) (\r -> happyReturn (HappyAbsSyn12 r))

happyReduce_27 = happyMonadReduce 1 12 happyReduction_27
happyReduction_27 (_ `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (WriteLn l) s l
	) (\r -> happyReturn (HappyAbsSyn12 r))

happyReduce_28 = happyMonadReduce 2 12 happyReduction_28
happyReduction_28 ((HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (Return happy_var_2 l) s l
	) (\r -> happyReturn (HappyAbsSyn12 r))

happyReduce_29 = happyMonadReduce 1 13 happyReduction_29
happyReduction_29 ((HappyTerminal (TCBool happy_var_1)) `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (CBool happy_var_1 l) s l
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_30 = happyMonadReduce 3 13 happyReduction_30
happyReduction_30 ((HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_1) `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (And happy_var_1 happy_var_3 l) s l
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_31 = happyMonadReduce 3 13 happyReduction_31
happyReduction_31 ((HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_1) `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (Or happy_var_1 happy_var_3 l) s l
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_32 = happyMonadReduce 2 13 happyReduction_32
happyReduction_32 ((HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (Not happy_var_2 l) s l
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_33 = happyMonadReduce 3 13 happyReduction_33
happyReduction_33 ((HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_1) `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (Eq happy_var_1 happy_var_3 l) s l
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_34 = happyMonadReduce 3 13 happyReduction_34
happyReduction_34 ((HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_1) `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (NEq happy_var_1 happy_var_3 l) s l
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_35 = happyMonadReduce 3 13 happyReduction_35
happyReduction_35 ((HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_1) `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (Lt happy_var_1 happy_var_3 l) s l
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_36 = happyMonadReduce 3 13 happyReduction_36
happyReduction_36 ((HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_1) `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (LEq happy_var_1 happy_var_3 l) s l
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_37 = happyMonadReduce 3 13 happyReduction_37
happyReduction_37 ((HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_1) `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (Gt happy_var_1 happy_var_3 l) s l
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_38 = happyMonadReduce 3 13 happyReduction_38
happyReduction_38 ((HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_1) `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (GEq happy_var_1 happy_var_3 l) s l
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_39 = happyMonadReduce 1 13 happyReduction_39
happyReduction_39 ((HappyTerminal (TCInt happy_var_1)) `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (CInt happy_var_1 l) s l
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_40 = happyMonadReduce 3 13 happyReduction_40
happyReduction_40 ((HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_1) `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (Add happy_var_1 happy_var_3 l) s l
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_41 = happyMonadReduce 3 13 happyReduction_41
happyReduction_41 ((HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_1) `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (Sub happy_var_1 happy_var_3 l) s l
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_42 = happyMonadReduce 3 13 happyReduction_42
happyReduction_42 ((HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_1) `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (Mul happy_var_1 happy_var_3 l) s l
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_43 = happyMonadReduce 3 13 happyReduction_43
happyReduction_43 ((HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_1) `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (Div happy_var_1 happy_var_3 l) s l
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_44 = happyMonadReduce 2 13 happyReduction_44
happyReduction_44 ((HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (Sub (CInt 0 l) happy_var_2 l) s l
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_45 = happyMonadReduce 1 13 happyReduction_45
happyReduction_45 ((HappyTerminal (TCString happy_var_1)) `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (CString happy_var_1 l) s l
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_46 = happyMonadReduce 1 13 happyReduction_46
happyReduction_46 ((HappyTerminal (TId happy_var_1)) `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (Id happy_var_1 l) s l
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_47 = happyMonadReduce 4 13 happyReduction_47
happyReduction_47 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TId happy_var_1)) `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP (Func happy_var_1 happy_var_3 l) s l
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_48 = happyMonadReduce 3 13 happyReduction_48
happyReduction_48 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = happyThen ( \s l -> returnP happy_var_2 s l
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_49 = happySpecReduce_0 14 happyReduction_49
happyReduction_49  =  HappyAbsSyn14
		 ([]
	)

happyReduce_50 = happySpecReduce_1 14 happyReduction_50
happyReduction_50 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn14
		 ([happy_var_1]
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3 14 happyReduction_51
happyReduction_51 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1 : happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TEOF -> action 53 53 (error "reading EOF!") (HappyState action) sts stk;
	TProgram -> cont 15;
	TId happy_dollar_dollar -> cont 16;
	TProc -> cont 17;
	TFun -> cont 18;
	TRet -> cont 19;
	TCInt happy_dollar_dollar -> cont 20;
	TCBool happy_dollar_dollar -> cont 21;
	TCString happy_dollar_dollar -> cont 22;
	TType happy_dollar_dollar -> cont 23;
	TIf -> cont 24;
	TThen -> cont 25;
	TElse -> cont 26;
	TWhile -> cont 27;
	TDo -> cont 28;
	TWrite -> cont 29;
	TWriteLn -> cont 30;
	TWriteNum -> cont 31;
	TBegin -> cont 32;
	TEnd -> cont 33;
	TAnd -> cont 34;
	TOr -> cont 35;
	TNot -> cont 36;
	TAssign -> cont 37;
	TEq -> cont 38;
	TNEq -> cont 39;
	TLt -> cont 40;
	TLE -> cont 41;
	TGt -> cont 42;
	TGE -> cont 43;
	TAdd -> cont 44;
	TSub -> cont 45;
	TTimes -> cont 46;
	TDiv -> cont 47;
	TE -> cont 48;
	TOB -> cont 49;
	TCB -> cont 50;
	TSep -> cont 51;
	TSepP -> cont 52;
	})

happyThen :: P a -> (a -> P b) -> P b
happyThen = (thenP)
happyReturn = (returnP)
happyThen1 = happyThen
happyReturn1 = happyReturn

lis = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

type Warnings = [String]

data ParseResult a
      = ParseOk a Warnings
      | ParseFail String

type P a = String -> LineNumber -> Warnings -> ParseResult a

getLineNum :: P Int
getLineNum = \s line w -> ParseOk line []

getWarnings :: P Warnings
getWarnings = \s l w -> ParseOk w []

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l w -> case m s l w of
                        ParseFail msg -> ParseFail msg
                        ParseOk a w -> k a s l w

returnP :: a -> P a
returnP a = \s l w -> ParseOk a w

happyError :: P a
happyError = getLineNum `thenP` \line ->
             getWarnings `thenP` \ws -> 
             failP $ (showWarnings ws) ++ "Error de parsing en linea " ++ show line ++ "\n"

lexError :: String -> P a
lexError msg = getLineNum `thenP` \line ->
               getWarnings `thenP` \ws -> 
               failP $ (showWarnings ws) ++ "Error lexico en linea " ++ show line ++ ": " ++ msg ++ "\n"

failP :: String -> P a
failP err = \s l w -> ParseFail err

lexer :: (Token -> P a) -> P a
lexer cont []           = cont TEOF []
lexer cont ('\'':cs)    = let (_, cs') = break (== '\n') cs in \line -> lexer cont cs' line
lexer cont ('\n':cs)    = \line -> lexer cont cs (line + 1)
lexer cont (c:cs)       | isSpace c = lexer cont cs
                        | isAlpha c = lexAlpha cont (c:cs)
                        | isDigit c = lexNum   cont (c:cs)
lexer cont ('.':cs)     = lexNum cont ('.':cs)
lexer cont ('"':cs)     = lexString cont cs
lexer cont ('+':cs)     = cont TAdd    cs
lexer cont ('-':cs)     = cont TSub    cs
lexer cont ('*':cs)     = cont TTimes  cs
lexer cont ('=':'=':cs) = cont TEq     cs
lexer cont ('=':cs)     = cont TAssign cs
lexer cont (':':cs)     = cont TE      cs
lexer cont ('/':'=':cs) = cont TNEq    cs
lexer cont ('/':cs)     = cont TDiv    cs
lexer cont ('<':'=':cs) = cont TLE     cs
lexer cont ('<':cs)     = cont TLt     cs
lexer cont ('>':'=':cs) = cont TGE     cs
lexer cont ('>':cs)     = cont TGt     cs
lexer cont ('&':'&':cs) = cont TAnd    cs
lexer cont ('(':cs)     = cont TOB     cs
lexer cont (')':cs)     = cont TCB     cs
lexer cont (';':cs)     = cont TSep    cs
lexer cont (',':cs)     = cont TSepP   cs
lexer cont ('|':'|':cs) = cont TOr     cs
lexer cont cs           = lexError ("simbolo no esperado '" ++ [head cs] ++ "'") cs 

lexNum cont cs = let (num,rest) = lexNum1 cs
		   in cont (TCInt (read num)) rest
                 where lexNum1  cs = span isDigit cs











lexString cont cs =
       \l w -> let (str,x:rest) = span (\c -> (c /= '"') && (c /= '\n')) cs
                   w' = w ++ [("Warning: en la linea " ++ show l ++ " string terminado con fin de linea")]
               in case x of
                    '"'  -> cont (TCString str) rest l w 
                    '\n' -> cont (TCString str) rest l w' 

lexAlpha cont cs = case span (\c ->isAlpha c || isDigit c) cs of
                    ("program",rest) -> cont TProgram        rest
                    ("True",   rest) -> cont (TCBool True)   rest
                    ("False",  rest) -> cont (TCBool False)  rest
                    ("if",     rest) -> cont TIf             rest
                    ("then",   rest) -> cont TThen           rest
                    ("else",   rest) -> cont TElse           rest
                    ("while",  rest) -> cont TWhile          rest
                    ("do",     rest) -> cont TDo             rest
                    ("begin",  rest) -> cont TBegin          rest
                    ("end",    rest) -> cont TEnd            rest
                    ("proc",   rest) -> cont TProc           rest
                    ("fun",    rest) -> cont TFun            rest
                    ("return", rest) -> cont TRet            rest
                    ("bool",   rest) -> cont (TType Boolean) rest
                    ("int",    rest) -> cont (TType Number)  rest
                    ("string", rest) -> cont (TType Str)     rest
                    ("real",   rest) -> cont (TType Real)    rest
                    ("writeln",  rest) -> cont TWriteLn      rest
                    ("writenum",  rest) -> cont TWriteNum    rest
                    ("write",  rest) -> cont TWrite          rest
                    ("not",    rest) -> cont TNot            rest
                    (id,       rest) -> cont (TId id)        rest
                    (s,r)            -> lexError ("simbolo no esperado '" ++  s ++ "'") cs 

basicParse :: String -> (Program,Warnings)
basicParse s = case lis s 1 [] of
                 ParseOk p w -> (p,w)
                 ParseFail msg -> error msg

parse :: String -> Either (Program,Warnings) String
parse s =  case lis s 1 [] of
                 ParseOk p ws -> let  ws' = CheckIdentsUse.check p
                                 in CheckDup.check p `ifNoErr`
                                    CheckDecl.check p `ifNoErr`
                                    CheckTypes.check p `noErr`
	                              Left (namemangling(declStr(optimize p)),ws'++ ws )
                 ParseFail msg -> Right msg


showWarnings = (concatMap (\s -> s ++ "\n")). filter (/="")

parseFile :: FilePath -> IO (Either (Program,Warnings) String)
parseFile f = do s <- readFile f
                 return $ parse s
{-# LINE 1 "GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.18 2001/09/25 14:39:03 simonmar Exp $

{-# LINE 15 "GenericTemplate.hs" #-}






















































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

happyAccept j tk st sts (HappyStk ans _) = 

					   (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 138 "GenericTemplate.hs" #-}


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
     = action nt j tk st sts (fn v1 `HappyStk` stk')

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = action nt j tk st sts (fn v1 v2 `HappyStk` stk')

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = action nt j tk st sts (fn v1 v2 v3 `HappyStk` stk')

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk = action nt j tk st1 sts1 (fn stk)
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError


{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









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
