{-# OPTIONS_GHC -w #-}
module A1Parser where
import A1Lexer
import Types
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11

action_0 (33) = happyShift action_4
action_0 (4) = happyGoto action_5
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 _ = happyFail

action_1 (33) = happyShift action_4
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 _ = happyFail

action_2 (23) = happyShift action_10
action_2 (27) = happyShift action_11
action_2 (30) = happyShift action_12
action_2 (31) = happyShift action_13
action_2 (32) = happyShift action_14
action_2 (33) = happyShift action_4
action_2 (6) = happyGoto action_7
action_2 (7) = happyGoto action_8
action_2 (8) = happyGoto action_9
action_2 _ = happyFail

action_3 _ = happyReduce_2

action_4 (32) = happyShift action_6
action_4 _ = happyFail

action_5 (36) = happyAccept
action_5 _ = happyFail

action_6 (35) = happyShift action_28
action_6 _ = happyFail

action_7 _ = happyReduce_3

action_8 (23) = happyShift action_10
action_8 (27) = happyShift action_11
action_8 (30) = happyShift action_12
action_8 (31) = happyShift action_13
action_8 (32) = happyShift action_14
action_8 (8) = happyGoto action_27
action_8 _ = happyReduce_1

action_9 _ = happyReduce_7

action_10 (32) = happyShift action_26
action_10 _ = happyFail

action_11 (32) = happyShift action_25
action_11 _ = happyFail

action_12 (14) = happyShift action_19
action_12 (17) = happyShift action_20
action_12 (18) = happyShift action_21
action_12 (22) = happyShift action_22
action_12 (32) = happyShift action_23
action_12 (9) = happyGoto action_16
action_12 (10) = happyGoto action_24
action_12 (11) = happyGoto action_18
action_12 _ = happyFail

action_13 (14) = happyShift action_19
action_13 (17) = happyShift action_20
action_13 (18) = happyShift action_21
action_13 (22) = happyShift action_22
action_13 (32) = happyShift action_23
action_13 (9) = happyGoto action_16
action_13 (10) = happyGoto action_17
action_13 (11) = happyGoto action_18
action_13 _ = happyFail

action_14 (16) = happyShift action_15
action_14 _ = happyFail

action_15 (14) = happyShift action_19
action_15 (17) = happyShift action_20
action_15 (18) = happyShift action_21
action_15 (32) = happyShift action_23
action_15 (9) = happyGoto action_16
action_15 (11) = happyGoto action_41
action_15 _ = happyFail

action_16 _ = happyReduce_24

action_17 (34) = happyShift action_40
action_17 _ = happyFail

action_18 (12) = happyShift action_36
action_18 (13) = happyShift action_37
action_18 (14) = happyShift action_38
action_18 (15) = happyShift action_39
action_18 _ = happyReduce_18

action_19 (14) = happyShift action_19
action_19 (17) = happyShift action_20
action_19 (18) = happyShift action_21
action_19 (32) = happyShift action_23
action_19 (9) = happyGoto action_16
action_19 (11) = happyGoto action_35
action_19 _ = happyFail

action_20 _ = happyReduce_15

action_21 _ = happyReduce_16

action_22 _ = happyReduce_17

action_23 _ = happyReduce_25

action_24 (34) = happyShift action_34
action_24 _ = happyFail

action_25 (29) = happyShift action_33
action_25 _ = happyFail

action_26 (24) = happyShift action_32
action_26 _ = happyFail

action_27 _ = happyReduce_8

action_28 (19) = happyShift action_29
action_28 (20) = happyShift action_30
action_28 (21) = happyShift action_31
action_28 _ = happyFail

action_29 (34) = happyShift action_51
action_29 _ = happyFail

action_30 (34) = happyShift action_50
action_30 _ = happyFail

action_31 (34) = happyShift action_49
action_31 _ = happyFail

action_32 (23) = happyShift action_10
action_32 (27) = happyShift action_11
action_32 (30) = happyShift action_12
action_32 (31) = happyShift action_13
action_32 (32) = happyShift action_14
action_32 (7) = happyGoto action_48
action_32 (8) = happyGoto action_9
action_32 _ = happyFail

action_33 (23) = happyShift action_10
action_33 (27) = happyShift action_11
action_33 (30) = happyShift action_12
action_33 (31) = happyShift action_13
action_33 (32) = happyShift action_14
action_33 (7) = happyGoto action_47
action_33 (8) = happyGoto action_9
action_33 _ = happyFail

action_34 _ = happyReduce_13

action_35 _ = happyReduce_23

action_36 (14) = happyShift action_19
action_36 (17) = happyShift action_20
action_36 (18) = happyShift action_21
action_36 (32) = happyShift action_23
action_36 (9) = happyGoto action_16
action_36 (11) = happyGoto action_46
action_36 _ = happyFail

action_37 (14) = happyShift action_19
action_37 (17) = happyShift action_20
action_37 (18) = happyShift action_21
action_37 (32) = happyShift action_23
action_37 (9) = happyGoto action_16
action_37 (11) = happyGoto action_45
action_37 _ = happyFail

action_38 (14) = happyShift action_19
action_38 (17) = happyShift action_20
action_38 (18) = happyShift action_21
action_38 (32) = happyShift action_23
action_38 (9) = happyGoto action_16
action_38 (11) = happyGoto action_44
action_38 _ = happyFail

action_39 (14) = happyShift action_19
action_39 (17) = happyShift action_20
action_39 (18) = happyShift action_21
action_39 (32) = happyShift action_23
action_39 (9) = happyGoto action_16
action_39 (11) = happyGoto action_43
action_39 _ = happyFail

action_40 _ = happyReduce_12

action_41 (12) = happyShift action_36
action_41 (13) = happyShift action_37
action_41 (14) = happyShift action_38
action_41 (15) = happyShift action_39
action_41 (34) = happyShift action_42
action_41 _ = happyFail

action_42 _ = happyReduce_14

action_43 _ = happyReduce_22

action_44 (13) = happyShift action_37
action_44 (15) = happyShift action_39
action_44 _ = happyReduce_21

action_45 _ = happyReduce_19

action_46 (13) = happyShift action_37
action_46 (15) = happyShift action_39
action_46 _ = happyReduce_20

action_47 (23) = happyShift action_10
action_47 (27) = happyShift action_11
action_47 (28) = happyShift action_54
action_47 (30) = happyShift action_12
action_47 (31) = happyShift action_13
action_47 (32) = happyShift action_14
action_47 (8) = happyGoto action_27
action_47 _ = happyFail

action_48 (23) = happyShift action_10
action_48 (25) = happyShift action_52
action_48 (26) = happyShift action_53
action_48 (27) = happyShift action_11
action_48 (30) = happyShift action_12
action_48 (31) = happyShift action_13
action_48 (32) = happyShift action_14
action_48 (8) = happyGoto action_27
action_48 _ = happyFail

action_49 _ = happyReduce_6

action_50 _ = happyReduce_4

action_51 _ = happyReduce_5

action_52 (23) = happyShift action_10
action_52 (27) = happyShift action_11
action_52 (30) = happyShift action_12
action_52 (31) = happyShift action_13
action_52 (32) = happyShift action_14
action_52 (7) = happyGoto action_55
action_52 (8) = happyGoto action_9
action_52 _ = happyFail

action_53 _ = happyReduce_9

action_54 _ = happyReduce_11

action_55 (23) = happyShift action_10
action_55 (26) = happyShift action_56
action_55 (27) = happyShift action_11
action_55 (30) = happyShift action_12
action_55 (31) = happyShift action_13
action_55 (32) = happyShift action_14
action_55 (8) = happyGoto action_27
action_55 _ = happyFail

action_56 _ = happyReduce_10

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (Program happy_var_1 happy_var_2
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (DeclarationsList DEmpty happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (DeclarationsList happy_var_1 happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 5 6 happyReduction_4
happyReduction_4 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((TokenId happy_var_2, _))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (DeclarationInt happy_var_2
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 5 6 happyReduction_5
happyReduction_5 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((TokenId happy_var_2, _))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (DeclarationFloat happy_var_2
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 5 6 happyReduction_6
happyReduction_6 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((TokenId happy_var_2, _))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (DeclarationString happy_var_2
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_1  7 happyReduction_7
happyReduction_7 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (StatementList SEmpty happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  7 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (StatementList happy_var_1 happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happyReduce 5 8 happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((TokenId happy_var_2, _))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (IfState happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_10 = happyReduce 7 8 happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((TokenId happy_var_2, _))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (IfElseState happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 5 8 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((TokenId happy_var_2, _))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (WhileState happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_3  8 happyReduction_12
happyReduction_12 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (PrintState happy_var_2
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  8 happyReduction_13
happyReduction_13 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (ReadState happy_var_2
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happyReduce 4 8 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((TokenId happy_var_1, _))) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (AssnStatement happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_1  9 happyReduction_15
happyReduction_15 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn9
		 (NumInt happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  9 happyReduction_16
happyReduction_16 (HappyTerminal (TokenFloat happy_var_1))
	 =  HappyAbsSyn9
		 (NumFloat happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  10 happyReduction_17
happyReduction_17 (HappyTerminal (TokenString happy_var_1))
	 =  HappyAbsSyn10
		 (StringLit happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  10 happyReduction_18
happyReduction_18 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (StringExp happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  11 happyReduction_19
happyReduction_19 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (Mult happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  11 happyReduction_20
happyReduction_20 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (Plus happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  11 happyReduction_21
happyReduction_21 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (Subt happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  11 happyReduction_22
happyReduction_22 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (Divi happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_2  11 happyReduction_23
happyReduction_23 (HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (Neg happy_var_2
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  11 happyReduction_24
happyReduction_24 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn11
		 (NUM happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  11 happyReduction_25
happyReduction_25 (HappyTerminal ((TokenId happy_var_1, _)))
	 =  HappyAbsSyn11
		 (Id happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 36 36 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenPlus -> cont 12;
	TokenMult -> cont 13;
	TokenMinus -> cont 14;
	TokenDiv -> cont 15;
	TokenEqual -> cont 16;
	TokenInt happy_dollar_dollar -> cont 17;
	TokenFloat happy_dollar_dollar -> cont 18;
	TokenTypeF -> cont 19;
	TokenTypeI -> cont 20;
	TokenTypeS -> cont 21;
	TokenString happy_dollar_dollar -> cont 22;
	TokenIf -> cont 23;
	TokenThen -> cont 24;
	TokenElse -> cont 25;
	TokenEndIf -> cont 26;
	TokenWhile -> cont 27;
	TokenDone -> cont 28;
	TokenDo -> cont 29;
	TokenRead -> cont 30;
	TokenPrint -> cont 31;
	(TokenId happy_dollar_dollar, _) -> cont 32;
	TokenVar -> cont 33;
	TokenSemicolon -> cont 34;
	TokenColon -> cont 35;
	_ -> happyError' (tk:tks)
	}

happyError_ 36 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = return
    (<*>) = ap
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

calc tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse Error"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}






# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4















































{-# LINE 6 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc-7.10.2/include/ghcversion.h" #-}

















{-# LINE 6 "<command-line>" #-}
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
