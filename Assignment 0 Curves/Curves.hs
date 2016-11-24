module Curves where
import Text.Printf

data Point = Point (Double, Double)
    deriving(Show)

point :: (Double, Double) -> Point
point (x, y) = Point (x, y)

pointX :: Point -> Double
pointX (Point p) = fst p

pointY :: Point -> Double
pointY (Point p) = snd p

instance Eq Point where
    x == y = (abs(pointX x - pointX y) < 0.01) && (abs (pointY x - pointY y) < 0.01)
    
-- We define a Curve as a tuple of a point, s0, and an ordered list of subsequent points
data Curve = Curve (Point, [Point]) deriving (Show)

instance Eq Curve where
    (Curve x) == (Curve y) = (fst x == fst y) && curveEq (snd x) (snd y)

curveEq :: [Point] -> [Point] -> Bool
curveEq [] [] = True
curveEq [] _ = False
curveEq _ [] = False
curveEq (x:xs) (y:ys) = x == y && curveEq xs ys     

curve :: Point -> [Point] -> Curve
curve p px = Curve (p, px)

connect :: Curve -> Curve -> Curve
connect (Curve c1) (Curve c2) = Curve (fst c1, snd c1 ++ uncurry (:) c2)

rotate :: Curve -> Double -> Curve
rotate (Curve c) i = let rads = i * pi/180 -- <- Conversion of degrees to radians
                         points = snd c
                         x0 = pointX (fst c)
                         y0 = pointY (fst c)
                         res = map (\x -> Point(pointX x * cos rads - pointY x * sin rads, pointX x * sin rads + pointY x * cos rads)) points
                     in curve (point (x0 * cos rads - y0 * sin rads, x0 * sin rads + y0 * cos rads)) res

translate :: Curve -> Point -> Curve
translate (Curve c) p = let delta_x = pointX p - pointX (fst c)
                            delta_y = pointY p - pointY (fst c)
                            res = map (\x -> point (pointX x + delta_x, pointY x  + delta_y)) (snd c)
                        in curve p res

data Line = Vertical Double | Horizontal Double

reflect :: Curve -> Line -> Curve
reflect (Curve c) (Vertical d) = let points = map (\x -> Point(pointX x - 2*(pointX x - d), pointY x)) (snd c)
                                     x1 = pointX(fst c)
                                 in curve (point (x1 - 2*(x1 - d), pointY (fst c))) points

reflect (Curve c) (Horizontal d) = let points = map (\x -> Point(pointX x, pointY x - 2*(pointY x - d))) (snd c)
                                       y = pointY(fst c)
                                   in curve (Point(pointX(fst c), y - 2*(y - d))) points

bbox :: Curve -> (Point, Point)
bbox cs = let list = toList cs
              xlist = map pointX list
              ylist = map pointY list
              xmin = minimum xlist
              ymin = minimum ylist
              xmax = maximum xlist
              ymax = maximum ylist
          in (point (xmin, ymin), point (xmax, ymax))
                 
width :: Curve -> Double
width cs = let box = bbox cs
               xmin = pointX $ fst box
               xmax = pointX $ snd box
           in xmax - xmin
                 
height :: Curve -> Double
height cs = let box = bbox cs
                ymin = pointY $ fst box
                ymax = pointY $ snd box
            in ymax - ymin                 
                 
toList :: Curve -> [Point]
toList (Curve c) = uncurry (:) c  

normalize :: Curve -> Curve
normalize cs = let box = bbox cs
                   deltaX = pointX $ fst box
                   deltaY = pointY $ fst box
                   Curve (s0, list) = cs
                   news0 = point (pointX s0 - deltaX, pointY s0 - deltaY) 
                   newlist = map (\p -> point(pointX p - deltaX, pointY p - deltaY)) list
               in Curve(news0, newlist)

toSVG :: Curve -> String
toSVG cs = let pointList = toList cs
               h = height cs
               w = width cs
               intro = "<svg xmlns=\"http://www.w3.org/2000/svg\"\n      width=\"" ++ printf "%.0f" w ++ "px\" height=\"" ++ printf "%.0f" h ++ "px\" version=\"1.1\">\n<g>"
           in intro ++ toSVGrecursion pointList ++ "</g>\n</svg>"
                 
toSVGrecursion :: [Point] -> String
toSVGrecursion [] = ""
toSVGrecursion [_] = "" 
toSVGrecursion (x:y:xs) = "<line style=\"stroke-width: 2px; stroke: black; fill:white\"\n            "
                             ++ "x1=\"" ++ printf "%.2f" (pointX x) ++ "\" x2=\"" ++ printf "%.2f" (pointX y) 
                          ++ "\" y1=\"" ++ printf "%.2f" (pointY x) ++ "\" y2=\"" ++ printf "%.2f" (pointY y) ++ "\" />\n" ++ toSVGrecursion (y:xs)
                 
toFile :: Curve -> FilePath -> IO ()
toFile cs path = writeFile path $ toSVG cs               
                 
hilbert :: Curve -> Curve
hilbert c = c0 `connect` c1 `connect` c2 `connect` c3
   where  w = width c
          h = height c
          p = 6

          ch = reflect c $ Vertical 0

          c0 = ch `rotate` (-90) `translate` point (w+p+w, h+p+h)
          c1 = c `translate` point (w+p+w, h)
          c2 = c
          c3 = ch `rotate` 90 `translate` point (0, h+p)                 
                 