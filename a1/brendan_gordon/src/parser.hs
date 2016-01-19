{-# OPTIONS_GHC -w #-}
module main where
import lexer
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10

action_0 (30) = happyShift action_4
action_0 (4) = happyGoto action_5
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 _ = happyFail

action_1 (30) = happyShift action_4
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 _ = happyFail

action_2 (31) = happyShift action_7
action_2 _ = happyFail

action_3 _ = happyReduce_2

action_4 (29) = happyShift action_6
action_4 _ = happyFail

action_5 (33) = happyAccept
action_5 _ = happyFail

action_6 (32) = happyShift action_17
action_6 _ = happyFail

action_7 (20) = happyShift action_12
action_7 (24) = happyShift action_13
action_7 (27) = happyShift action_14
action_7 (28) = happyShift action_15
action_7 (29) = happyShift action_16
action_7 (30) = happyShift action_4
action_7 (6) = happyGoto action_8
action_7 (7) = happyGoto action_9
action_7 (8) = happyGoto action_10
action_7 (9) = happyGoto action_11
action_7 _ = happyFail

action_8 (31) = happyShift action_27
action_8 _ = happyFail

action_9 _ = happyReduce_13

action_10 (31) = happyShift action_26
action_10 _ = happyFail

action_11 _ = happyReduce_12

action_12 (29) = happyShift action_25
action_12 _ = happyFail

action_13 (29) = happyShift action_24
action_13 _ = happyFail

action_14 (16) = happyShift action_23
action_14 _ = happyFail

action_15 (16) = happyShift action_22
action_15 _ = happyFail

action_16 (15) = happyShift action_21
action_16 _ = happyFail

action_17 (17) = happyShift action_18
action_17 (18) = happyShift action_19
action_17 (19) = happyShift action_20
action_17 _ = happyFail

action_18 _ = happyReduce_4

action_19 _ = happyReduce_5

action_20 _ = happyReduce_6

action_21 (13) = happyShift action_32
action_21 (16) = happyShift action_33
action_21 (29) = happyShift action_34
action_21 (10) = happyGoto action_36
action_21 _ = happyFail

action_22 (13) = happyShift action_32
action_22 (16) = happyShift action_33
action_22 (29) = happyShift action_34
action_22 (10) = happyGoto action_35
action_22 _ = happyFail

action_23 (13) = happyShift action_32
action_23 (16) = happyShift action_33
action_23 (29) = happyShift action_34
action_23 (10) = happyGoto action_31
action_23 _ = happyFail

action_24 (26) = happyShift action_30
action_24 _ = happyFail

action_25 (21) = happyShift action_29
action_25 _ = happyFail

action_26 (20) = happyShift action_12
action_26 (24) = happyShift action_13
action_26 (27) = happyShift action_14
action_26 (28) = happyShift action_15
action_26 (29) = happyShift action_16
action_26 (7) = happyGoto action_28
action_26 (9) = happyGoto action_11
action_26 _ = happyReduce_1

action_27 _ = happyReduce_3

action_28 _ = happyReduce_14

action_29 (20) = happyShift action_12
action_29 (24) = happyShift action_13
action_29 (27) = happyShift action_14
action_29 (28) = happyShift action_15
action_29 (29) = happyShift action_16
action_29 (7) = happyGoto action_9
action_29 (8) = happyGoto action_46
action_29 (9) = happyGoto action_11
action_29 _ = happyFail

action_30 (20) = happyShift action_12
action_30 (24) = happyShift action_13
action_30 (27) = happyShift action_14
action_30 (28) = happyShift action_15
action_30 (29) = happyShift action_16
action_30 (7) = happyGoto action_9
action_30 (8) = happyGoto action_45
action_30 (9) = happyGoto action_11
action_30 _ = happyFail

action_31 (11) = happyShift action_37
action_31 (12) = happyShift action_38
action_31 (13) = happyShift action_39
action_31 (14) = happyShift action_40
action_31 (16) = happyShift action_44
action_31 _ = happyFail

action_32 (13) = happyShift action_32
action_32 (16) = happyShift action_33
action_32 (29) = happyShift action_34
action_32 (10) = happyGoto action_43
action_32 _ = happyFail

action_33 (13) = happyShift action_32
action_33 (16) = happyShift action_33
action_33 (29) = happyShift action_34
action_33 (10) = happyGoto action_42
action_33 _ = happyFail

action_34 _ = happyReduce_22

action_35 (11) = happyShift action_37
action_35 (12) = happyShift action_38
action_35 (13) = happyShift action_39
action_35 (14) = happyShift action_40
action_35 (16) = happyShift action_41
action_35 _ = happyFail

action_36 (11) = happyShift action_37
action_36 (12) = happyShift action_38
action_36 (13) = happyShift action_39
action_36 (14) = happyShift action_40
action_36 _ = happyReduce_15

action_37 (13) = happyShift action_32
action_37 (16) = happyShift action_33
action_37 (29) = happyShift action_34
action_37 (10) = happyGoto action_55
action_37 _ = happyFail

action_38 (13) = happyShift action_32
action_38 (16) = happyShift action_33
action_38 (29) = happyShift action_34
action_38 (10) = happyGoto action_54
action_38 _ = happyFail

action_39 (13) = happyShift action_32
action_39 (16) = happyShift action_33
action_39 (29) = happyShift action_34
action_39 (10) = happyGoto action_53
action_39 _ = happyFail

action_40 (13) = happyShift action_32
action_40 (16) = happyShift action_33
action_40 (29) = happyShift action_34
action_40 (10) = happyGoto action_52
action_40 _ = happyFail

action_41 _ = happyReduce_10

action_42 (11) = happyShift action_37
action_42 (12) = happyShift action_38
action_42 (13) = happyShift action_39
action_42 (14) = happyShift action_40
action_42 (16) = happyShift action_51
action_42 _ = happyFail

action_43 _ = happyReduce_21

action_44 _ = happyReduce_11

action_45 (25) = happyShift action_50
action_45 (31) = happyShift action_49
action_45 _ = happyFail

action_46 (22) = happyShift action_47
action_46 (23) = happyShift action_48
action_46 (31) = happyShift action_49
action_46 _ = happyFail

action_47 (20) = happyShift action_12
action_47 (24) = happyShift action_13
action_47 (27) = happyShift action_14
action_47 (28) = happyShift action_15
action_47 (29) = happyShift action_16
action_47 (7) = happyGoto action_9
action_47 (8) = happyGoto action_56
action_47 (9) = happyGoto action_11
action_47 _ = happyFail

action_48 _ = happyReduce_7

action_49 (20) = happyShift action_12
action_49 (24) = happyShift action_13
action_49 (27) = happyShift action_14
action_49 (28) = happyShift action_15
action_49 (29) = happyShift action_16
action_49 (7) = happyGoto action_28
action_49 (9) = happyGoto action_11
action_49 _ = happyFail

action_50 _ = happyReduce_9

action_51 _ = happyReduce_16

action_52 _ = happyReduce_20

action_53 (12) = happyShift action_38
action_53 (14) = happyShift action_40
action_53 _ = happyReduce_19

action_54 _ = happyReduce_17

action_55 (12) = happyShift action_38
action_55 (14) = happyShift action_40
action_55 _ = happyReduce_18

action_56 (23) = happyShift action_57
action_56 (31) = happyShift action_49
action_56 _ = happyFail

action_57 _ = happyReduce_8

happyReduce_1 = happyReduce 4 4 happyReduction_1
happyReduction_1 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Program happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (DeclarationsList happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happyReduce 4 5 happyReduction_3
happyReduction_3 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (DeclarationsList happy_var_1 3
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 4 6 happyReduction_4
happyReduction_4 (_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Id happy_var_2
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 4 6 happyReduction_5
happyReduction_5 (_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Id happy_var_2
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 4 6 happyReduction_6
happyReduction_6 (_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Id happy_var_2
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 5 7 happyReduction_7
happyReduction_7 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Id happy_var_2
	) `HappyStk` happyRest

happyReduce_8 = happyReduce 7 7 happyReduction_8
happyReduction_8 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Id #2
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 5 7 happyReduction_9
happyReduction_9 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Id happy_var_2
	) `HappyStk` happyRest

happyReduce_10 = happyReduce 4 7 happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Statement happy_var_3
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 4 7 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Statement happy_var_3
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_1  7 happyReduction_12
happyReduction_12 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  8 happyReduction_13
happyReduction_13 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  8 happyReduction_14
happyReduction_14 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (StatementList happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  9 happyReduction_15
happyReduction_15 _
	_
	(HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn9
		 (Id happy_var_1
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  10 happyReduction_16
happyReduction_16 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (String happy_var_2
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  10 happyReduction_17
happyReduction_17 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Mult happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  10 happyReduction_18
happyReduction_18 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Plus happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  10 happyReduction_19
happyReduction_19 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Subt happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  10 happyReduction_20
happyReduction_20 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Divi happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  10 happyReduction_21
happyReduction_21 (HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (Neg happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  10 happyReduction_22
happyReduction_22 (HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 33 33 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenPlus -> cont 11;
	TokenMult -> cont 12;
	TokenMinus -> cont 13;
	TokenDiv -> cont 14;
	TokenEq -> cont 15;
	TokenQuote -> cont 16;
	TokenInt happy_dollar_dollar -> cont 17;
	TokenFloat happy_dollar_dollar -> cont 18;
	TokenString happy_dollar_dollar -> cont 19;
	TokenIf -> cont 20;
	TokenThen -> cont 21;
	TokenElse -> cont 22;
	TokenEndif -> cont 23;
	TokenWhile -> cont 24;
	TokenDone -> cont 25;
	TokenDo -> cont 26;
	TokenRead -> cont 27;
	TokenPrint -> cont 28;
	TokenId happy_dollar_dollar -> cont 29;
	TokenVar -> cont 30;
	TokenSemicolon -> cont 31;
	TokenColon -> cont 32;
	_ -> happyError' (tk:tks)
	}

happyError_ 33 tk tks = happyError' tks
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
parseError _ = error 'Parse error'
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
