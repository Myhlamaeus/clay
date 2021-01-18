module Clay.Internal where

import Clay.Directional

axialToTuple :: Axial a -> (Maybe a, Maybe a)
axialToTuple (AxialBoth a) = (Just a, Just a)
axialToTuple (AxialEach b i) = (Just b, Just i)
axialToTuple (AxialBlock b) = (Just b, Nothing)
axialToTuple (AxialInline b) = (Nothing, Just b)

directionalToTuple :: Directional a -> (Maybe a, Maybe a, Maybe a, Maybe a)
directionalToTuple dir = (axBlock =<< dirStart dir, axInline =<< dirStart dir, axBlock =<< dirEnd dir, axInline =<< dirEnd dir)
  where
    dirStart (DirectionalEach s _) = Just s
    dirStart (DirectionalStart s) = Just s
    dirStart _ = Nothing
    dirEnd (DirectionalEach _ e) = Just e
    dirEnd (DirectionalEnd e) = Just e
    dirEnd _ = Nothing
    axBlock = fst . axialToTuple
    axInline = snd . axialToTuple

unsafeModifyDirectionalAsTuple :: ((Maybe a, Maybe a, Maybe a, Maybe a) -> (Maybe a, Maybe a, Maybe a, Maybe a)) -> Directional a -> Directional a
unsafeModifyDirectionalAsTuple f = unsafeTupleToDirectional . f . directionalToTuple
  where
    tupleToAxial (Just b, Just i) = Just $ AxialEach b i
    tupleToAxial (Just b, Nothing) = Just $ AxialBlock b
    tupleToAxial (Nothing, Just i) = Just $ AxialInline i
    tupleToAxial _ = Nothing

    unsafeTupleToDirectional (bs, is, be, ie) = case (tupleToAxial (bs, is), tupleToAxial (be, ie)) of
      (Just s, Just e) -> DirectionalEach s e
      (Just s, Nothing) -> DirectionalStart s
      (Nothing, Just e) -> DirectionalEnd e
      (Nothing, Nothing) -> error "There's no Directional with no value for all directions"
