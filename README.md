# Todo

- Reorganize functions in files
- Fix Benchmarks
    - SegFaults
- in iloc, merge branch with successor if alternate path guarantees return

## Doesn't Work

- binaryConvert -- Segmentation Fault
- fact_sum -- Segmentation Fault
- killerBubbles -- Segmentation Fault
- primes -- Segmentation Fault
- TicTac -- Diff

## Works

- BenchMarkishTopics
- bert
- biggest
- creativeBenchMarkName
- Fibonacci
- GeneralFunctAndOptimize
- hailstone
- hanoi_benchmark
- mile1
- mixed
- programBreaker
- stats
- uncreativeBenchmark
- wasteOfCycles
- OptimizationBenchmark -- Register Allocation SLOW
src/Mini/Iloc/Types.hs:178:29: Warning: Redundant bracket
Found:
  (Cbreq l1 l2)
Why not:
  Cbreq l1 l2

src/Mini/Iloc/Types.hs:179:29: Warning: Redundant bracket
Found:
  (Cbrge l1 l2)
Why not:
  Cbrge l1 l2

src/Mini/Iloc/Types.hs:180:29: Warning: Redundant bracket
Found:
  (Cbrgt l1 l2)
Why not:
  Cbrgt l1 l2

src/Mini/Iloc/Types.hs:181:29: Warning: Redundant bracket
Found:
  (Cbrle l1 l2)
Why not:
  Cbrle l1 l2

src/Mini/Iloc/Types.hs:182:29: Warning: Redundant bracket
Found:
  (Cbrlt l1 l2)
Why not:
  Cbrlt l1 l2

src/Mini/Iloc/Types.hs:183:29: Warning: Redundant bracket
Found:
  (Cbrne l1 l2)
Why not:
  Cbrne l1 l2

src/Mini/Iloc/Types.hs:184:26: Warning: Redundant bracket
Found:
  (Jumpi l1)
Why not:
  Jumpi l1

src/Mini/Iloc/Types.hs:192:37: Warning: Redundant bracket
Found:
  (Restoreformal s1 i1)
Why not:
  Restoreformal s1 i1

src/Mini/Iloc/Types.hs:201:25: Warning: Redundant bracket
Found:
  (Call l1)
Why not:
  Call l1

src/Mini/Iloc/Types.hs:219:28: Warning: Redundant bracket
Found:
  (PrepArgs i)
Why not:
  PrepArgs i

src/Mini/Iloc/Types.hs:220:30: Warning: Redundant bracket
Found:
  (UnprepArgs i)
Why not:
  UnprepArgs i

src/Mini/Parser.hs:4:1: Error: Use fewer imports
Found:
  import Control.Applicative ((<$>))
  import Control.Applicative (Applicative(..))
Why not:
  import Control.Applicative ((<$>), Applicative(..))

src/Mini/Parser.hs:199:22: Warning: Redundant bracket
Found:
  Int ->
    (Int ->
       (Token) ->
         HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn) ->
           [HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn)] ->
             HappyStk HappyAbsSyn -> (P) HappyAbsSyn)
Why not:
  Int ->
    Int ->
      (Token) ->
        HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn) ->
          [HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn)] ->
            HappyStk HappyAbsSyn -> (P) HappyAbsSyn

src/Mini/Parser.hs:201:12: Error: Redundant bracket
Found:
  (Token)
Why not:
  Token

src/Mini/Parser.hs:202:23: Error: Redundant bracket
Found:
  (Token)
Why not:
  Token

src/Mini/Parser.hs:202:56: Error: Redundant bracket
Found:
  (P)
Why not:
  P

src/Mini/Parser.hs:203:24: Error: Redundant bracket
Found:
  (Token)
Why not:
  Token

src/Mini/Parser.hs:203:57: Error: Redundant bracket
Found:
  (P)
Why not:
  P

src/Mini/Parser.hs:205:12: Error: Redundant bracket
Found:
  (P)
Why not:
  P

src/Mini/Parser.hs:292:12: Error: Redundant bracket
Found:
  (Token)
Why not:
  Token

src/Mini/Parser.hs:293:23: Error: Redundant bracket
Found:
  (Token)
Why not:
  Token

src/Mini/Parser.hs:293:56: Error: Redundant bracket
Found:
  (P)
Why not:
  P

src/Mini/Parser.hs:294:24: Error: Redundant bracket
Found:
  (Token)
Why not:
  Token

src/Mini/Parser.hs:294:57: Error: Redundant bracket
Found:
  (P)
Why not:
  P

src/Mini/Parser.hs:296:12: Error: Redundant bracket
Found:
  (P)
Why not:
  P

src/Mini/Parser.hs:1019:18: Error: Redundant bracket
Found:
  ([])
Why not:
  []

src/Mini/Parser.hs:1041:9: Warning: Redundant bracket
Found:
  (HappyAbsSyn14 happy_var_2) `HappyStk`
    ((HappyAbsSyn9 happy_var_1) `HappyStk` happyRest)
Why not:
  HappyAbsSyn14 happy_var_2 `HappyStk`
    ((HappyAbsSyn9 happy_var_1) `HappyStk` happyRest)

src/Mini/Parser.hs:1042:9: Warning: Redundant bracket
Found:
  (HappyAbsSyn9 happy_var_1) `HappyStk` happyRest
Why not:
  HappyAbsSyn9 happy_var_1 `HappyStk` happyRest

src/Mini/Parser.hs:1044:22: Error: Redundant bracket
Found:
  ((lineP >>= \ l -> return $ fmap (Decl l happy_var_1) happy_var_2))
Why not:
  (lineP >>= \ l -> return $ fmap (Decl l happy_var_1) happy_var_2)

src/Mini/Parser.hs:1045:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn5 r)
Why not:
  happyReturn . HappyAbsSyn5

src/Mini/Parser.hs:1049:18: Error: Redundant bracket
Found:
  ([])
Why not:
  []

src/Mini/Parser.hs:1062:19: Warning: Redundant bracket
Found:
  (HappyTerminal (TokenId happy_var_2)) `HappyStk`
    ((HappyAbsSyn9 happy_var_1) `HappyStk` happyRest)
Why not:
  HappyTerminal (TokenId happy_var_2) `HappyStk`
    ((HappyAbsSyn9 happy_var_1) `HappyStk` happyRest)

src/Mini/Parser.hs:1063:9: Warning: Redundant bracket
Found:
  (HappyAbsSyn9 happy_var_1) `HappyStk` happyRest
Why not:
  HappyAbsSyn9 happy_var_1 `HappyStk` happyRest

src/Mini/Parser.hs:1065:22: Error: Redundant bracket
Found:
  ((lineP >>= \ l -> return $ Field l happy_var_1 happy_var_2))
Why not:
  (lineP >>= \ l -> return $ Field l happy_var_1 happy_var_2)

src/Mini/Parser.hs:1066:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn8 r)
Why not:
  happyReturn . HappyAbsSyn8

src/Mini/Parser.hs:1071:18: Error: Redundant bracket
Found:
  (intType)
Why not:
  intType

src/Mini/Parser.hs:1077:18: Error: Redundant bracket
Found:
  (boolType)
Why not:
  boolType

src/Mini/Parser.hs:1084:18: Error: Redundant bracket
Found:
  (happy_var_2)
Why not:
  happy_var_2

src/Mini/Parser.hs:1091:18: Error: Redundant bracket
Found:
  (intType)
Why not:
  intType

src/Mini/Parser.hs:1097:18: Error: Redundant bracket
Found:
  (boolType)
Why not:
  boolType

src/Mini/Parser.hs:1103:9: Warning: Redundant bracket
Found:
  (HappyAbsSyn7 happy_var_2) `HappyStk` (_ `HappyStk` happyRest)
Why not:
  HappyAbsSyn7 happy_var_2 `HappyStk` (_ `HappyStk` happyRest)

src/Mini/Parser.hs:1106:22: Error: Redundant bracket
Found:
  ((lineP >>=
      \ l -> return $ \ x -> [TDef l x $ reverse happy_var_2]))
Why not:
  (lineP >>= \ l -> return $ \ x -> [TDef l x $ reverse happy_var_2])

src/Mini/Parser.hs:1108:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn11 r)
Why not:
  happyReturn . HappyAbsSyn11

src/Mini/Parser.hs:1112:9: Warning: Redundant bracket
Found:
  (HappyAbsSyn14 happy_var_1) `HappyStk` happyRest
Why not:
  HappyAbsSyn14 happy_var_1 `HappyStk` happyRest

src/Mini/Parser.hs:1114:22: Error: Redundant bracket
Found:
  ((lineP >>= \ l -> return $ \ x -> fmap (Decl 0 x) happy_var_1))
Why not:
  (lineP >>= \ l -> return $ \ x -> fmap (Decl 0 x) happy_var_1)

src/Mini/Parser.hs:1116:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn11 r)
Why not:
  happyReturn . HappyAbsSyn11

src/Mini/Parser.hs:1120:18: Error: Redundant bracket
Found:
  ([])
Why not:
  []

src/Mini/Parser.hs:1133:9: Warning: Redundant bracket
Found:
  (HappyAbsSyn14 happy_var_2) `HappyStk`
    ((HappyAbsSyn9 happy_var_1) `HappyStk` happyRest)
Why not:
  HappyAbsSyn14 happy_var_2 `HappyStk`
    ((HappyAbsSyn9 happy_var_1) `HappyStk` happyRest)

src/Mini/Parser.hs:1134:9: Warning: Redundant bracket
Found:
  (HappyAbsSyn9 happy_var_1) `HappyStk` happyRest
Why not:
  HappyAbsSyn9 happy_var_1 `HappyStk` happyRest

src/Mini/Parser.hs:1136:22: Error: Redundant bracket
Found:
  ((lineP >>=
      \ l -> return $ fmap (Declaration l happy_var_1) happy_var_2))
Why not:
  (lineP >>=
     \ l -> return $ fmap (Declaration l happy_var_1) happy_var_2)

src/Mini/Parser.hs:1137:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn12 r)
Why not:
  happyReturn . HappyAbsSyn12

src/Mini/Parser.hs:1142:18: Error: Redundant bracket
Found:
  ([happy_var_1])
Why not:
  [happy_var_1]

src/Mini/Parser.hs:1157:18: Error: Redundant bracket
Found:
  ([])
Why not:
  []

src/Mini/Parser.hs:1170:9: Warning: Redundant bracket
Found:
  (HappyAbsSyn22 happy_var_7) `HappyStk`
    ((HappyAbsSyn12 happy_var_6) `HappyStk`
       (_ `HappyStk`
          ((HappyAbsSyn9 happy_var_4) `HappyStk`
             ((HappyAbsSyn7 happy_var_3) `HappyStk`
                ((HappyTerminal (TokenId happy_var_2)) `HappyStk`
                   (_ `HappyStk` happyRest))))))
Why not:
  HappyAbsSyn22 happy_var_7 `HappyStk`
    ((HappyAbsSyn12 happy_var_6) `HappyStk`
       (_ `HappyStk`
          ((HappyAbsSyn9 happy_var_4) `HappyStk`
             ((HappyAbsSyn7 happy_var_3) `HappyStk`
                ((HappyTerminal (TokenId happy_var_2)) `HappyStk`
                   (_ `HappyStk` happyRest))))))

src/Mini/Parser.hs:1171:9: Warning: Redundant bracket
Found:
  (HappyAbsSyn12 happy_var_6) `HappyStk`
    (_ `HappyStk`
       ((HappyAbsSyn9 happy_var_4) `HappyStk`
          ((HappyAbsSyn7 happy_var_3) `HappyStk`
             ((HappyTerminal (TokenId happy_var_2)) `HappyStk`
                (_ `HappyStk` happyRest)))))
Why not:
  HappyAbsSyn12 happy_var_6 `HappyStk`
    (_ `HappyStk`
       ((HappyAbsSyn9 happy_var_4) `HappyStk`
          ((HappyAbsSyn7 happy_var_3) `HappyStk`
             ((HappyTerminal (TokenId happy_var_2)) `HappyStk`
                (_ `HappyStk` happyRest)))))

src/Mini/Parser.hs:1173:9: Warning: Redundant bracket
Found:
  (HappyAbsSyn9 happy_var_4) `HappyStk`
    ((HappyAbsSyn7 happy_var_3) `HappyStk`
       ((HappyTerminal (TokenId happy_var_2)) `HappyStk`
          (_ `HappyStk` happyRest)))
Why not:
  HappyAbsSyn9 happy_var_4 `HappyStk`
    ((HappyAbsSyn7 happy_var_3) `HappyStk`
       ((HappyTerminal (TokenId happy_var_2)) `HappyStk`
          (_ `HappyStk` happyRest)))

src/Mini/Parser.hs:1174:9: Warning: Redundant bracket
Found:
  (HappyAbsSyn7 happy_var_3) `HappyStk`
    ((HappyTerminal (TokenId happy_var_2)) `HappyStk`
       (_ `HappyStk` happyRest))
Why not:
  HappyAbsSyn7 happy_var_3 `HappyStk`
    ((HappyTerminal (TokenId happy_var_2)) `HappyStk`
       (_ `HappyStk` happyRest))

src/Mini/Parser.hs:1175:9: Warning: Redundant bracket
Found:
  (HappyTerminal (TokenId happy_var_2)) `HappyStk`
    (_ `HappyStk` happyRest)
Why not:
  HappyTerminal (TokenId happy_var_2) `HappyStk`
    (_ `HappyStk` happyRest)

src/Mini/Parser.hs:1178:22: Error: Redundant bracket
Found:
  ((lineP >>=
      \ l ->
        return $
          Function l happy_var_2 (reverse happy_var_3) (reverse happy_var_6)
            (reverse happy_var_7)
            happy_var_4))
Why not:
  (lineP >>=
     \ l ->
       return $
         Function l happy_var_2 (reverse happy_var_3) (reverse happy_var_6)
           (reverse happy_var_7)
           happy_var_4)

src/Mini/Parser.hs:1180:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn16 r)
Why not:
  happyReturn . HappyAbsSyn16

src/Mini/Parser.hs:1186:18: Error: Redundant bracket
Found:
  ([])
Why not:
  []

src/Mini/Parser.hs:1194:18: Error: Redundant bracket
Found:
  (happy_var_2)
Why not:
  happy_var_2

src/Mini/Parser.hs:1201:18: Error: Redundant bracket
Found:
  ([happy_var_1])
Why not:
  [happy_var_1]

src/Mini/Parser.hs:1217:18: Error: Redundant bracket
Found:
  (happy_var_1)
Why not:
  happy_var_1

src/Mini/Parser.hs:1224:18: Error: Redundant bracket
Found:
  (voidType)
Why not:
  voidType

src/Mini/Parser.hs:1230:18: Error: Redundant bracket
Found:
  (happy_var_1)
Why not:
  happy_var_1

src/Mini/Parser.hs:1237:18: Error: Redundant bracket
Found:
  (happy_var_1)
Why not:
  happy_var_1

src/Mini/Parser.hs:1244:18: Error: Redundant bracket
Found:
  (happy_var_1)
Why not:
  happy_var_1

src/Mini/Parser.hs:1251:18: Error: Redundant bracket
Found:
  (happy_var_1)
Why not:
  happy_var_1

src/Mini/Parser.hs:1258:18: Error: Redundant bracket
Found:
  (happy_var_1)
Why not:
  happy_var_1

src/Mini/Parser.hs:1265:18: Error: Redundant bracket
Found:
  (happy_var_1)
Why not:
  happy_var_1

src/Mini/Parser.hs:1272:18: Error: Redundant bracket
Found:
  (happy_var_1)
Why not:
  happy_var_1

src/Mini/Parser.hs:1279:18: Error: Redundant bracket
Found:
  (happy_var_1)
Why not:
  happy_var_1

src/Mini/Parser.hs:1286:18: Error: Redundant bracket
Found:
  (happy_var_1)
Why not:
  happy_var_1

src/Mini/Parser.hs:1301:18: Error: Redundant bracket
Found:
  ([])
Why not:
  []

src/Mini/Parser.hs:1314:9: Warning: Redundant bracket
Found:
  (HappyAbsSyn35 happy_var_3) `HappyStk`
    (_ `HappyStk` ((HappyAbsSyn34 happy_var_1) `HappyStk` happyRest))
Why not:
  HappyAbsSyn35 happy_var_3 `HappyStk`
    (_ `HappyStk` ((HappyAbsSyn34 happy_var_1) `HappyStk` happyRest))

src/Mini/Parser.hs:1316:9: Warning: Redundant bracket
Found:
  (HappyAbsSyn34 happy_var_1) `HappyStk` happyRest
Why not:
  HappyAbsSyn34 happy_var_1 `HappyStk` happyRest

src/Mini/Parser.hs:1318:22: Error: Redundant bracket
Found:
  ((lineP >>= \ l -> return $ Asgn l happy_var_1 happy_var_3))
Why not:
  (lineP >>= \ l -> return $ Asgn l happy_var_1 happy_var_3)

src/Mini/Parser.hs:1319:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn20 r)
Why not:
  happyReturn . HappyAbsSyn20

src/Mini/Parser.hs:1323:9: Warning: Redundant bracket
Found:
  (HappyAbsSyn25 happy_var_3) `HappyStk`
    ((HappyAbsSyn35 happy_var_2) `HappyStk` (_ `HappyStk` happyRest))
Why not:
  HappyAbsSyn25 happy_var_3 `HappyStk`
    ((HappyAbsSyn35 happy_var_2) `HappyStk` (_ `HappyStk` happyRest))

src/Mini/Parser.hs:1324:9: Warning: Redundant bracket
Found:
  (HappyAbsSyn35 happy_var_2) `HappyStk` (_ `HappyStk` happyRest)
Why not:
  HappyAbsSyn35 happy_var_2 `HappyStk` (_ `HappyStk` happyRest)

src/Mini/Parser.hs:1327:22: Error: Redundant bracket
Found:
  ((lineP >>= \ l -> return $ Print l happy_var_2 happy_var_3))
Why not:
  (lineP >>= \ l -> return $ Print l happy_var_2 happy_var_3)

src/Mini/Parser.hs:1328:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn20 r)
Why not:
  happyReturn . HappyAbsSyn20

src/Mini/Parser.hs:1332:18: Error: Redundant bracket
Found:
  (False)
Why not:
  False

src/Mini/Parser.hs:1338:18: Error: Redundant bracket
Found:
  (True)
Why not:
  True

src/Mini/Parser.hs:1343:9: Warning: Redundant bracket
Found:
  (HappyAbsSyn34 happy_var_2) `HappyStk` (_ `HappyStk` happyRest)
Why not:
  HappyAbsSyn34 happy_var_2 `HappyStk` (_ `HappyStk` happyRest)

src/Mini/Parser.hs:1346:22: Error: Redundant bracket
Found:
  ((lineP >>= \ l -> return $ Read l happy_var_2))
Why not:
  (lineP >>= \ l -> return $ Read l happy_var_2)

src/Mini/Parser.hs:1347:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn20 r)
Why not:
  happyReturn . HappyAbsSyn20

src/Mini/Parser.hs:1350:20: Warning: Redundant bracket
Found:
  (HappyAbsSyn28 happy_var_6) `HappyStk`
    ((HappyAbsSyn20 happy_var_5) `HappyStk`
       (_ `HappyStk`
          ((HappyAbsSyn35 happy_var_3) `HappyStk`
             (_ `HappyStk` (_ `HappyStk` happyRest)))))
Why not:
  HappyAbsSyn28 happy_var_6 `HappyStk`
    ((HappyAbsSyn20 happy_var_5) `HappyStk`
       (_ `HappyStk`
          ((HappyAbsSyn35 happy_var_3) `HappyStk`
             (_ `HappyStk` (_ `HappyStk` happyRest)))))

src/Mini/Parser.hs:1351:9: Warning: Redundant bracket
Found:
  (HappyAbsSyn20 happy_var_5) `HappyStk`
    (_ `HappyStk`
       ((HappyAbsSyn35 happy_var_3) `HappyStk`
          (_ `HappyStk` (_ `HappyStk` happyRest))))
Why not:
  HappyAbsSyn20 happy_var_5 `HappyStk`
    (_ `HappyStk`
       ((HappyAbsSyn35 happy_var_3) `HappyStk`
          (_ `HappyStk` (_ `HappyStk` happyRest))))

src/Mini/Parser.hs:1353:9: Warning: Redundant bracket
Found:
  (HappyAbsSyn35 happy_var_3) `HappyStk`
    (_ `HappyStk` (_ `HappyStk` happyRest))
Why not:
  HappyAbsSyn35 happy_var_3 `HappyStk`
    (_ `HappyStk` (_ `HappyStk` happyRest))

src/Mini/Parser.hs:1357:22: Error: Redundant bracket
Found:
  ((lineP >>=
      \ l -> return $ Cond l happy_var_3 happy_var_5 happy_var_6))
Why not:
  (lineP >>=
     \ l -> return $ Cond l happy_var_3 happy_var_5 happy_var_6)

src/Mini/Parser.hs:1358:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn20 r)
Why not:
  happyReturn . HappyAbsSyn20

src/Mini/Parser.hs:1362:18: Error: Redundant bracket
Found:
  (Nothing)
Why not:
  Nothing

src/Mini/Parser.hs:1374:20: Warning: Redundant bracket
Found:
  (HappyAbsSyn20 happy_var_5) `HappyStk`
    (_ `HappyStk`
       ((HappyAbsSyn35 happy_var_3) `HappyStk`
          (_ `HappyStk` (_ `HappyStk` happyRest))))
Why not:
  HappyAbsSyn20 happy_var_5 `HappyStk`
    (_ `HappyStk`
       ((HappyAbsSyn35 happy_var_3) `HappyStk`
          (_ `HappyStk` (_ `HappyStk` happyRest))))

src/Mini/Parser.hs:1376:9: Warning: Redundant bracket
Found:
  (HappyAbsSyn35 happy_var_3) `HappyStk`
    (_ `HappyStk` (_ `HappyStk` happyRest))
Why not:
  HappyAbsSyn35 happy_var_3 `HappyStk`
    (_ `HappyStk` (_ `HappyStk` happyRest))

src/Mini/Parser.hs:1380:22: Error: Redundant bracket
Found:
  ((lineP >>= \ l -> return $ Loop l happy_var_3 happy_var_5))
Why not:
  (lineP >>= \ l -> return $ Loop l happy_var_3 happy_var_5)

src/Mini/Parser.hs:1381:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn20 r)
Why not:
  happyReturn . HappyAbsSyn20

src/Mini/Parser.hs:1385:9: Warning: Redundant bracket
Found:
  (HappyAbsSyn35 happy_var_2) `HappyStk` (_ `HappyStk` happyRest)
Why not:
  HappyAbsSyn35 happy_var_2 `HappyStk` (_ `HappyStk` happyRest)

src/Mini/Parser.hs:1388:22: Error: Redundant bracket
Found:
  ((lineP >>= \ l -> return $ Delete l happy_var_2))
Why not:
  (lineP >>= \ l -> return $ Delete l happy_var_2)

src/Mini/Parser.hs:1389:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn20 r)
Why not:
  happyReturn . HappyAbsSyn20

src/Mini/Parser.hs:1393:9: Warning: Redundant bracket
Found:
  (HappyAbsSyn32 happy_var_2) `HappyStk` (_ `HappyStk` happyRest)
Why not:
  HappyAbsSyn32 happy_var_2 `HappyStk` (_ `HappyStk` happyRest)

src/Mini/Parser.hs:1396:22: Error: Redundant bracket
Found:
  ((lineP >>= \ l -> return $ Ret l happy_var_2))
Why not:
  (lineP >>= \ l -> return $ Ret l happy_var_2)

src/Mini/Parser.hs:1397:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn20 r)
Why not:
  happyReturn . HappyAbsSyn20

src/Mini/Parser.hs:1401:18: Error: Redundant bracket
Found:
  (Nothing)
Why not:
  Nothing

src/Mini/Parser.hs:1413:9: Warning: Redundant bracket
Found:
  (HappyAbsSyn42 happy_var_2) `HappyStk`
    ((HappyTerminal (TokenId happy_var_1)) `HappyStk` happyRest)
Why not:
  HappyAbsSyn42 happy_var_2 `HappyStk`
    ((HappyTerminal (TokenId happy_var_1)) `HappyStk` happyRest)

src/Mini/Parser.hs:1414:9: Warning: Redundant bracket
Found:
  (HappyTerminal (TokenId happy_var_1)) `HappyStk` happyRest
Why not:
  HappyTerminal (TokenId happy_var_1) `HappyStk` happyRest

src/Mini/Parser.hs:1416:22: Error: Redundant bracket
Found:
  ((lineP >>=
      \ l -> return $ InvocSt l happy_var_1 $ reverse happy_var_2))
Why not:
  (lineP >>=
     \ l -> return $ InvocSt l happy_var_1 $ reverse happy_var_2)

src/Mini/Parser.hs:1417:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn20 r)
Why not:
  happyReturn . HappyAbsSyn20

src/Mini/Parser.hs:1427:20: Warning: Redundant bracket
Found:
  (HappyTerminal (TokenId happy_var_3)) `HappyStk`
    (_ `HappyStk` ((HappyAbsSyn34 happy_var_1) `HappyStk` happyRest))
Why not:
  HappyTerminal (TokenId happy_var_3) `HappyStk`
    (_ `HappyStk` ((HappyAbsSyn34 happy_var_1) `HappyStk` happyRest))

src/Mini/Parser.hs:1429:9: Warning: Redundant bracket
Found:
  (HappyAbsSyn34 happy_var_1) `HappyStk` happyRest
Why not:
  HappyAbsSyn34 happy_var_1 `HappyStk` happyRest

src/Mini/Parser.hs:1431:22: Error: Redundant bracket
Found:
  ((lineP >>=
      \ l -> return $ LValue (Just l) happy_var_3 (Just happy_var_1)))
Why not:
  (lineP >>=
     \ l -> return $ LValue (Just l) happy_var_3 (Just happy_var_1))

src/Mini/Parser.hs:1432:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn34 r)
Why not:
  happyReturn . HappyAbsSyn34

src/Mini/Parser.hs:1437:18: Error: Redundant bracket
Found:
  (happy_var_1)
Why not:
  happy_var_1

src/Mini/Parser.hs:1442:20: Warning: Redundant bracket
Found:
  (HappyAbsSyn35 happy_var_3) `HappyStk`
    ((HappyTerminal (TokenBoolOp happy_var_2)) `HappyStk`
       ((HappyAbsSyn35 happy_var_1) `HappyStk` happyRest))
Why not:
  HappyAbsSyn35 happy_var_3 `HappyStk`
    ((HappyTerminal (TokenBoolOp happy_var_2)) `HappyStk`
       ((HappyAbsSyn35 happy_var_1) `HappyStk` happyRest))

src/Mini/Parser.hs:1443:9: Warning: Redundant bracket
Found:
  (HappyTerminal (TokenBoolOp happy_var_2)) `HappyStk`
    ((HappyAbsSyn35 happy_var_1) `HappyStk` happyRest)
Why not:
  HappyTerminal (TokenBoolOp happy_var_2) `HappyStk`
    ((HappyAbsSyn35 happy_var_1) `HappyStk` happyRest)

src/Mini/Parser.hs:1444:9: Warning: Redundant bracket
Found:
  (HappyAbsSyn35 happy_var_1) `HappyStk` happyRest
Why not:
  HappyAbsSyn35 happy_var_1 `HappyStk` happyRest

src/Mini/Parser.hs:1446:22: Error: Redundant bracket
Found:
  ((lineP >>=
      \ l -> return $ BinExp l happy_var_2 happy_var_1 happy_var_3))
Why not:
  (lineP >>=
     \ l -> return $ BinExp l happy_var_2 happy_var_1 happy_var_3)

src/Mini/Parser.hs:1447:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn35 r)
Why not:
  happyReturn . HappyAbsSyn35

src/Mini/Parser.hs:1452:18: Error: Redundant bracket
Found:
  (happy_var_1)
Why not:
  happy_var_1

src/Mini/Parser.hs:1457:20: Warning: Redundant bracket
Found:
  (HappyAbsSyn35 happy_var_3) `HappyStk`
    ((HappyTerminal (TokenCmpOp happy_var_2)) `HappyStk`
       ((HappyAbsSyn35 happy_var_1) `HappyStk` happyRest))
Why not:
  HappyAbsSyn35 happy_var_3 `HappyStk`
    ((HappyTerminal (TokenCmpOp happy_var_2)) `HappyStk`
       ((HappyAbsSyn35 happy_var_1) `HappyStk` happyRest))

src/Mini/Parser.hs:1458:9: Warning: Redundant bracket
Found:
  (HappyTerminal (TokenCmpOp happy_var_2)) `HappyStk`
    ((HappyAbsSyn35 happy_var_1) `HappyStk` happyRest)
Why not:
  HappyTerminal (TokenCmpOp happy_var_2) `HappyStk`
    ((HappyAbsSyn35 happy_var_1) `HappyStk` happyRest)

src/Mini/Parser.hs:1459:9: Warning: Redundant bracket
Found:
  (HappyAbsSyn35 happy_var_1) `HappyStk` happyRest
Why not:
  HappyAbsSyn35 happy_var_1 `HappyStk` happyRest

src/Mini/Parser.hs:1461:22: Error: Redundant bracket
Found:
  ((lineP >>=
      \ l -> return $ BinExp l happy_var_2 happy_var_1 happy_var_3))
Why not:
  (lineP >>=
     \ l -> return $ BinExp l happy_var_2 happy_var_1 happy_var_3)

src/Mini/Parser.hs:1462:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn35 r)
Why not:
  happyReturn . HappyAbsSyn35

src/Mini/Parser.hs:1467:18: Error: Redundant bracket
Found:
  (happy_var_1)
Why not:
  happy_var_1

src/Mini/Parser.hs:1472:20: Warning: Redundant bracket
Found:
  (HappyAbsSyn35 happy_var_3) `HappyStk`
    (_ `HappyStk` ((HappyAbsSyn35 happy_var_1) `HappyStk` happyRest))
Why not:
  HappyAbsSyn35 happy_var_3 `HappyStk`
    (_ `HappyStk` ((HappyAbsSyn35 happy_var_1) `HappyStk` happyRest))

src/Mini/Parser.hs:1474:9: Warning: Redundant bracket
Found:
  (HappyAbsSyn35 happy_var_1) `HappyStk` happyRest
Why not:
  HappyAbsSyn35 happy_var_1 `HappyStk` happyRest

src/Mini/Parser.hs:1476:22: Error: Redundant bracket
Found:
  ((lineP >>= \ l -> return $ BinExp l "+" happy_var_1 happy_var_3))
Why not:
  (lineP >>= \ l -> return $ BinExp l "+" happy_var_1 happy_var_3)

src/Mini/Parser.hs:1477:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn35 r)
Why not:
  happyReturn . HappyAbsSyn35

src/Mini/Parser.hs:1480:20: Warning: Redundant bracket
Found:
  (HappyAbsSyn35 happy_var_3) `HappyStk`
    (_ `HappyStk` ((HappyAbsSyn35 happy_var_1) `HappyStk` happyRest))
Why not:
  HappyAbsSyn35 happy_var_3 `HappyStk`
    (_ `HappyStk` ((HappyAbsSyn35 happy_var_1) `HappyStk` happyRest))

src/Mini/Parser.hs:1482:9: Warning: Redundant bracket
Found:
  (HappyAbsSyn35 happy_var_1) `HappyStk` happyRest
Why not:
  HappyAbsSyn35 happy_var_1 `HappyStk` happyRest

src/Mini/Parser.hs:1484:22: Error: Redundant bracket
Found:
  ((lineP >>= \ l -> return $ BinExp l "-" happy_var_1 happy_var_3))
Why not:
  (lineP >>= \ l -> return $ BinExp l "-" happy_var_1 happy_var_3)

src/Mini/Parser.hs:1485:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn35 r)
Why not:
  happyReturn . HappyAbsSyn35

src/Mini/Parser.hs:1490:18: Error: Redundant bracket
Found:
  (happy_var_1)
Why not:
  happy_var_1

src/Mini/Parser.hs:1495:20: Warning: Redundant bracket
Found:
  (HappyAbsSyn35 happy_var_3) `HappyStk`
    (_ `HappyStk` ((HappyAbsSyn35 happy_var_1) `HappyStk` happyRest))
Why not:
  HappyAbsSyn35 happy_var_3 `HappyStk`
    (_ `HappyStk` ((HappyAbsSyn35 happy_var_1) `HappyStk` happyRest))

src/Mini/Parser.hs:1497:9: Warning: Redundant bracket
Found:
  (HappyAbsSyn35 happy_var_1) `HappyStk` happyRest
Why not:
  HappyAbsSyn35 happy_var_1 `HappyStk` happyRest

src/Mini/Parser.hs:1499:22: Error: Redundant bracket
Found:
  ((lineP >>= \ l -> return $ BinExp l "*" happy_var_1 happy_var_3))
Why not:
  (lineP >>= \ l -> return $ BinExp l "*" happy_var_1 happy_var_3)

src/Mini/Parser.hs:1500:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn35 r)
Why not:
  happyReturn . HappyAbsSyn35

src/Mini/Parser.hs:1503:20: Warning: Redundant bracket
Found:
  (HappyAbsSyn35 happy_var_3) `HappyStk`
    (_ `HappyStk` ((HappyAbsSyn35 happy_var_1) `HappyStk` happyRest))
Why not:
  HappyAbsSyn35 happy_var_3 `HappyStk`
    (_ `HappyStk` ((HappyAbsSyn35 happy_var_1) `HappyStk` happyRest))

src/Mini/Parser.hs:1505:9: Warning: Redundant bracket
Found:
  (HappyAbsSyn35 happy_var_1) `HappyStk` happyRest
Why not:
  HappyAbsSyn35 happy_var_1 `HappyStk` happyRest

src/Mini/Parser.hs:1507:22: Error: Redundant bracket
Found:
  ((lineP >>= \ l -> return $ BinExp l "/" happy_var_1 happy_var_3))
Why not:
  (lineP >>= \ l -> return $ BinExp l "/" happy_var_1 happy_var_3)

src/Mini/Parser.hs:1508:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn35 r)
Why not:
  happyReturn . HappyAbsSyn35

src/Mini/Parser.hs:1513:18: Error: Redundant bracket
Found:
  (happy_var_1)
Why not:
  happy_var_1

src/Mini/Parser.hs:1518:20: Warning: Redundant bracket
Found:
  (HappyAbsSyn35 happy_var_2) `HappyStk` (_ `HappyStk` happyRest)
Why not:
  HappyAbsSyn35 happy_var_2 `HappyStk` (_ `HappyStk` happyRest)

src/Mini/Parser.hs:1521:22: Error: Redundant bracket
Found:
  ((lineP >>= \ l -> return $ UExp l "!" happy_var_2))
Why not:
  (lineP >>= \ l -> return $ UExp l "!" happy_var_2)

src/Mini/Parser.hs:1522:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn35 r)
Why not:
  happyReturn . HappyAbsSyn35

src/Mini/Parser.hs:1525:20: Warning: Redundant bracket
Found:
  (HappyAbsSyn35 happy_var_2) `HappyStk` (_ `HappyStk` happyRest)
Why not:
  HappyAbsSyn35 happy_var_2 `HappyStk` (_ `HappyStk` happyRest)

src/Mini/Parser.hs:1528:22: Error: Redundant bracket
Found:
  ((lineP >>= \ l -> return $ UExp l "-" happy_var_2))
Why not:
  (lineP >>= \ l -> return $ UExp l "-" happy_var_2)

src/Mini/Parser.hs:1529:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn35 r)
Why not:
  happyReturn . HappyAbsSyn35

src/Mini/Parser.hs:1534:18: Error: Redundant bracket
Found:
  (happy_var_1)
Why not:
  happy_var_1

src/Mini/Parser.hs:1539:20: Warning: Redundant bracket
Found:
  (HappyTerminal (TokenId happy_var_3)) `HappyStk`
    (_ `HappyStk` ((HappyAbsSyn35 happy_var_1) `HappyStk` happyRest))
Why not:
  HappyTerminal (TokenId happy_var_3) `HappyStk`
    (_ `HappyStk` ((HappyAbsSyn35 happy_var_1) `HappyStk` happyRest))

src/Mini/Parser.hs:1541:9: Warning: Redundant bracket
Found:
  (HappyAbsSyn35 happy_var_1) `HappyStk` happyRest
Why not:
  HappyAbsSyn35 happy_var_1 `HappyStk` happyRest

src/Mini/Parser.hs:1543:22: Error: Redundant bracket
Found:
  ((lineP >>= \ l -> return $ DotExp l happy_var_1 happy_var_3))
Why not:
  (lineP >>= \ l -> return $ DotExp l happy_var_1 happy_var_3)

src/Mini/Parser.hs:1544:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn35 r)
Why not:
  happyReturn . HappyAbsSyn35

src/Mini/Parser.hs:1551:18: Error: Redundant bracket
Found:
  (happy_var_2)
Why not:
  happy_var_2

src/Mini/Parser.hs:1556:20: Warning: Redundant bracket
Found:
  (HappyTerminal (TokenId happy_var_1)) `HappyStk` happyRest
Why not:
  HappyTerminal (TokenId happy_var_1) `HappyStk` happyRest

src/Mini/Parser.hs:1558:22: Error: Redundant bracket
Found:
  ((lineP >>= \ l -> return $ IdExp l happy_var_1))
Why not:
  (lineP >>= \ l -> return $ IdExp l happy_var_1)

src/Mini/Parser.hs:1559:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn35 r)
Why not:
  happyReturn . HappyAbsSyn35

src/Mini/Parser.hs:1562:20: Warning: Redundant bracket
Found:
  (HappyAbsSyn42 happy_var_2) `HappyStk`
    ((HappyTerminal (TokenId happy_var_1)) `HappyStk` happyRest)
Why not:
  HappyAbsSyn42 happy_var_2 `HappyStk`
    ((HappyTerminal (TokenId happy_var_1)) `HappyStk` happyRest)

src/Mini/Parser.hs:1563:9: Warning: Redundant bracket
Found:
  (HappyTerminal (TokenId happy_var_1)) `HappyStk` happyRest
Why not:
  HappyTerminal (TokenId happy_var_1) `HappyStk` happyRest

src/Mini/Parser.hs:1565:22: Error: Redundant bracket
Found:
  ((lineP >>=
      \ l -> return $ InvocExp l happy_var_1 $ reverse happy_var_2))
Why not:
  (lineP >>=
     \ l -> return $ InvocExp l happy_var_1 $ reverse happy_var_2)

src/Mini/Parser.hs:1566:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn35 r)
Why not:
  happyReturn . HappyAbsSyn35

src/Mini/Parser.hs:1569:20: Warning: Redundant bracket
Found:
  (HappyTerminal (TokenNum happy_var_1)) `HappyStk` happyRest
Why not:
  HappyTerminal (TokenNum happy_var_1) `HappyStk` happyRest

src/Mini/Parser.hs:1571:22: Error: Redundant bracket
Found:
  ((lineP >>= \ l -> return $ IntExp l happy_var_1))
Why not:
  (lineP >>= \ l -> return $ IntExp l happy_var_1)

src/Mini/Parser.hs:1572:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn35 r)
Why not:
  happyReturn . HappyAbsSyn35

src/Mini/Parser.hs:1577:22: Error: Redundant bracket
Found:
  ((lineP >>= \ l -> return $ TrueExp l))
Why not:
  (lineP >>= \ l -> return $ TrueExp l)

src/Mini/Parser.hs:1578:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn35 r)
Why not:
  happyReturn . HappyAbsSyn35

src/Mini/Parser.hs:1583:22: Error: Redundant bracket
Found:
  ((lineP >>= \ l -> return $ FalseExp l))
Why not:
  (lineP >>= \ l -> return $ FalseExp l)

src/Mini/Parser.hs:1584:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn35 r)
Why not:
  happyReturn . HappyAbsSyn35

src/Mini/Parser.hs:1587:20: Warning: Redundant bracket
Found:
  (HappyTerminal (TokenId happy_var_2)) `HappyStk`
    (_ `HappyStk` happyRest)
Why not:
  HappyTerminal (TokenId happy_var_2) `HappyStk`
    (_ `HappyStk` happyRest)

src/Mini/Parser.hs:1590:22: Error: Redundant bracket
Found:
  ((lineP >>= \ l -> return $ NewExp l happy_var_2))
Why not:
  (lineP >>= \ l -> return $ NewExp l happy_var_2)

src/Mini/Parser.hs:1591:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn35 r)
Why not:
  happyReturn . HappyAbsSyn35

src/Mini/Parser.hs:1596:22: Error: Redundant bracket
Found:
  ((lineP >>= \ l -> return $ NullExp l))
Why not:
  (lineP >>= \ l -> return $ NullExp l)

src/Mini/Parser.hs:1597:12: Warning: Avoid lambda
Found:
  \ r -> happyReturn (HappyAbsSyn35 r)
Why not:
  happyReturn . HappyAbsSyn35

src/Mini/Parser.hs:1603:18: Error: Redundant bracket
Found:
  ([])
Why not:
  []

src/Mini/Parser.hs:1611:18: Error: Redundant bracket
Found:
  (happy_var_2)
Why not:
  happy_var_2

src/Mini/Parser.hs:1618:18: Error: Redundant bracket
Found:
  ([happy_var_1])
Why not:
  [happy_var_1]

src/Mini/Parser.hs:1679:15: Warning: Redundant bracket
Found:
  (return)
Why not:
  return

src/Mini/Parser.hs:1683:22: Error: Redundant bracket
Found:
  (Token)
Why not:
  Token

src/Mini/Parser.hs:1684:1: Error: Eta reduce
Found:
  happyError' tk = (\ token -> happyError) tk
Why not:
  happyError' = (\ token -> happyError)

src/Mini/Parser.hs:1684:19: Warning: Use const
Found:
  \ token -> happyError
Why not:
  const happyError

src/Mini/Parser.hs:1774:44: Warning: Redundant bracket
Found:
  (TypeDef l i f) : ts
Why not:
  TypeDef l i f : ts

src/Mini/Parser.hs:1775:48: Warning: Redundant bracket
Found:
  (Declaration l t i) : ds
Why not:
  Declaration l t i : ds

src/Mini/Parser.hs:1784:9: Error: Monad law, right identity
Found:
  asks snd >>= return
Why not:
  asks snd

src/Mini/Parser.hs:1829:28: Error: Avoid lambda
Found:
  \ x -> error x
Why not:
  error

templates/GenericTemplate.hs:104:10: Warning: Redundant bracket
Found:
  (happyReturn1 ans)
Why not:
  happyReturn1 ans

templates/GenericTemplate.hs:175:43: Error: Redundant bracket
Found:
  (i)
Why not:
  i

templates/GenericTemplate.hs:177:35: Error: Redundant bracket
Found:
  (new_state)
Why not:
  new_state

templates/GenericTemplate.hs:177:49: Error: Redundant bracket
Found:
  (st)
Why not:
  st

templates/GenericTemplate.hs:177:54: Error: Redundant bracket
Found:
  (sts)
Why not:
  sts

templates/GenericTemplate.hs:177:61: Error: Redundant bracket
Found:
  (stk)
Why not:
  stk

templates/GenericTemplate.hs:180:31: Error: Redundant bracket
Found:
  (st)
Why not:
  st

templates/GenericTemplate.hs:180:36: Error: Redundant bracket
Found:
  (sts)
Why not:
  sts

templates/GenericTemplate.hs:180:44: Warning: Redundant bracket
Found:
  (HappyTerminal (tk)) `HappyStk` stk
Why not:
  HappyTerminal (tk) `HappyStk` stk

templates/GenericTemplate.hs:180:59: Error: Redundant bracket
Found:
  (tk)
Why not:
  tk

templates/GenericTemplate.hs:185:18: Error: Redundant bracket
Found:
  (1)
Why not:
  1

templates/GenericTemplate.hs:186:33: Error: Redundant bracket
Found:
  ((HappyState (action)))
Why not:
  (HappyState (action))

templates/GenericTemplate.hs:186:46: Error: Redundant bracket
Found:
  (action)
Why not:
  action

templates/GenericTemplate.hs:187:27: Error: Redundant bracket
Found:
  (st)
Why not:
  st

templates/GenericTemplate.hs:187:32: Error: Redundant bracket
Found:
  (sts)
Why not:
  sts

templates/GenericTemplate.hs:190:18: Error: Redundant bracket
Found:
  (1)
Why not:
  1

templates/GenericTemplate.hs:191:36: Error: Redundant bracket
Found:
  (((st@(HappyState (action))) : (_)))
Why not:
  ((st@(HappyState (action))) : (_))

templates/GenericTemplate.hs:191:54: Error: Redundant bracket
Found:
  (action)
Why not:
  action

templates/GenericTemplate.hs:191:65: Error: Redundant bracket
Found:
  (_)
Why not:
  _

templates/GenericTemplate.hs:196:18: Error: Redundant bracket
Found:
  (1)
Why not:
  1

templates/GenericTemplate.hs:197:33: Error: Redundant bracket
Found:
  (_)
Why not:
  _

templates/GenericTemplate.hs:197:42: Error: Redundant bracket
Found:
  (((st@(HappyState (action))) : (_)))
Why not:
  ((st@(HappyState (action))) : (_))

templates/GenericTemplate.hs:197:60: Error: Redundant bracket
Found:
  (action)
Why not:
  action

templates/GenericTemplate.hs:197:71: Error: Redundant bracket
Found:
  (_)
Why not:
  _

templates/GenericTemplate.hs:202:18: Error: Redundant bracket
Found:
  (1)
Why not:
  1

templates/GenericTemplate.hs:203:33: Error: Redundant bracket
Found:
  (_)
Why not:
  _

templates/GenericTemplate.hs:203:37: Error: Redundant bracket
Found:
  (((_) : (sts@(((st@(HappyState (action))) : (_))))))
Why not:
  ((_) : (sts@(((st@(HappyState (action))) : (_)))))

templates/GenericTemplate.hs:203:39: Error: Redundant bracket
Found:
  (_)
Why not:
  _

templates/GenericTemplate.hs:203:48: Error: Redundant bracket
Found:
  (((st@(HappyState (action))) : (_)))
Why not:
  ((st@(HappyState (action))) : (_))

templates/GenericTemplate.hs:203:66: Error: Redundant bracket
Found:
  (action)
Why not:
  action

templates/GenericTemplate.hs:203:77: Error: Redundant bracket
Found:
  (_)
Why not:
  _

templates/GenericTemplate.hs:208:18: Error: Redundant bracket
Found:
  (1)
Why not:
  1

templates/GenericTemplate.hs:210:29: Error: Redundant bracket
Found:
  (1)
Why not:
  1

templates/GenericTemplate.hs:211:15: Error: Redundant bracket
Found:
  (((st1@(HappyState (action))) : (_)))
Why not:
  ((st1@(HappyState (action))) : (_))

templates/GenericTemplate.hs:211:34: Error: Redundant bracket
Found:
  (action)
Why not:
  action

templates/GenericTemplate.hs:211:45: Error: Redundant bracket
Found:
  (_)
Why not:
  _

templates/GenericTemplate.hs:216:18: Error: Redundant bracket
Found:
  (1)
Why not:
  1

templates/GenericTemplate.hs:218:25: Error: Redundant bracket
Found:
  (st)
Why not:
  st

templates/GenericTemplate.hs:218:30: Error: Redundant bracket
Found:
  (sts)
Why not:
  sts

templates/GenericTemplate.hs:219:14: Error: Redundant bracket
Found:
  (((st1@(HappyState (action))) : (_)))
Why not:
  ((st1@(HappyState (action))) : (_))

templates/GenericTemplate.hs:219:33: Error: Redundant bracket
Found:
  (action)
Why not:
  action

templates/GenericTemplate.hs:219:44: Error: Redundant bracket
Found:
  (_)
Why not:
  _

templates/GenericTemplate.hs:224:18: Error: Redundant bracket
Found:
  (1)
Why not:
  1

templates/GenericTemplate.hs:226:25: Error: Redundant bracket
Found:
  (st)
Why not:
  st

templates/GenericTemplate.hs:226:30: Error: Redundant bracket
Found:
  (sts)
Why not:
  sts

templates/GenericTemplate.hs:227:14: Error: Redundant bracket
Found:
  (((st1@(HappyState (action))) : (_)))
Why not:
  ((st1@(HappyState (action))) : (_))

templates/GenericTemplate.hs:227:33: Error: Redundant bracket
Found:
  (action)
Why not:
  action

templates/GenericTemplate.hs:227:44: Error: Redundant bracket
Found:
  (_)
Why not:
  _

templates/GenericTemplate.hs:240:14: Error: Redundant bracket
Found:
  (_)
Why not:
  _

templates/GenericTemplate.hs:240:18: Error: Redundant bracket
Found:
  (t)
Why not:
  t

templates/GenericTemplate.hs:240:41: Error: Redundant bracket
Found:
  (1)
Why not:
  1

templates/GenericTemplate.hs:243:55: Error: Redundant bracket
Found:
  (1)
Why not:
  1

templates/GenericTemplate.hs:264:43: Error: Redundant bracket
Found:
  (i)
Why not:
  i

templates/GenericTemplate.hs:281:29: Error: Redundant bracket
Found:
  (action)
Why not:
  action

templates/GenericTemplate.hs:283:16: Error: Redundant bracket
Found:
  (1)
Why not:
  1

templates/GenericTemplate.hs:283:20: Error: Redundant bracket
Found:
  (1)
Why not:
  1

templates/GenericTemplate.hs:283:39: Error: Redundant bracket
Found:
  (action)
Why not:
  action

templates/GenericTemplate.hs:283:55: Warning: Redundant bracket
Found:
  (HappyErrorToken (i)) `HappyStk` stk
Why not:
  HappyErrorToken (i) `HappyStk` stk

templates/GenericTemplate.hs:283:72: Error: Redundant bracket
Found:
  (i)
Why not:
  i

src/Mini/CFG.hs:308:79: Warning: Redundant bracket
Found:
  (edges graph) ++ newEdges
Why not:
  edges graph ++ newEdges

src/Mini/CFG.hs:310:19: Warning: Redundant bracket
Found:
  (getVerticesFromEdges newEdges) ++ (vertices graph)
Why not:
  getVerticesFromEdges newEdges ++ (vertices graph)

src/Mini/CFG.hs:310:19: Warning: Redundant bracket
Found:
  (getVerticesFromEdges newEdges) ++ (vertices graph)
Why not:
  (getVerticesFromEdges newEdges) ++ vertices graph

src/Mini/CFG.hs:321:9: Warning: Use guards
Found:
  newBounds
    = if toKill == lower then (lower + 1, upper) else
        if toKill == upper then (lower, upper - 1) else (lower, upper)
Why not:
  newBounds
    | toKill == lower = (lower + 1, upper)
    | toKill == upper = (lower, upper - 1)
    | otherwise = (lower, upper)

249 suggestions
