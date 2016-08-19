transpose [] = []
transpose (xs:xss) =
  let h = head xs
      tss = map (\x -> [x]) (tail xs)
      yss = transpose xss
  in
    case yss of
      []       -> [h]:tss
      (zs:zss) -> (h:zs):map (\(ts, zs') -> (head ts):zs') (zip tss zss)
