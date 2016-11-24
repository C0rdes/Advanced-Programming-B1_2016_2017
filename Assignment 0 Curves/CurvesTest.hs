import Curves

pointTest :: IO ()
pointTest = print $ "pointTest: " ++ show (point(2.0, 2.0) == Point(2.0, 2.0))

pointXTest :: IO ()    
pointXTest = print $ "pointXTest: " ++ show ((pointX $ point(0.5, 1.5)) == 0.5)

pointYTest :: IO ()
pointYTest = print $ "pointYTest: " ++ show ((pointY $ point(0.5, 1.5)) == 1.5)

curveEqTest1 :: IO ()
curveEqTest1 = print $ "curveEqTest1: " ++ show (Curve(Point (1.0, 2.0), [Point(3.0, 4.0)]) == Curve(Point (1.0, 2.0), [Point(3.0, 4.0)]))
curveEqTest2 :: IO ()
curveEqTest2 = print $ "curveEqTest2: " ++ show ((Curve(Point (1.0, 1.0), [Point(3.0, 4.0)]) == Curve(Point (1.0, 2.0), [Point(3.0, 4.0)])) == False)

curveTest :: IO ()
curveTest = print $ "curveTest: " ++ show ((curve (point(1.0, 2.0)) ([point(2.0, 3.0)])) == Curve (point(1.0, 2.0), [point(2.0, 3.0)]))

connectTest :: IO ()
connectTest = let c1 = Curve(Point (1.0, 2.0), [Point(3.0, 4.0)])
                  c2 = Curve(Point (4.0, 5.0), [Point(6.0, 7.0)])
                  ans = Curve(Point(1.0, 2.0), [Point(3.0, 4.0), Point(4.0, 5.0), Point(6.0, 7.0)])
              in print $ "connectTest: " ++ show (connect c1 c2 == ans)
              
rotateTest :: IO ()
rotateTest = let c1 = Curve(Point(1.0, 1.0), [Point(2.0, 2.0)])
                 d = 180
                 ans = Curve(Point(-1.0, -1.0), [Point(-2.0, -2.0)])
             in print $ "rotateTest: " ++ show (rotate c1 d == ans)
             
translateTest :: IO ()
translateTest = let c1 = Curve(Point(1.0, 2.0), [Point(3.0, 4.0), Point(2.0, 3.0)])
                    p = point(0,0)
                    ans = Curve(Point(0,0), [Point(2.0, 2.0), Point(1.0, 1.0)])
                in print $ "translateTest: " ++ show (translate c1 p == ans)
                
reflectTest1 :: IO ()
reflectTest1 = let c1 = Curve(Point(2.0, 2.0), [Point(4.0, 4.0), Point(-1.0, -2.0)])
                   d = Vertical 1.0
                   ans = Curve(Point(0, 2.0), [Point(-2.0, 4.0), Point(3.0, -2.0)])
               in print $ "reflectTest1: " ++ show (reflect c1 d == ans)

reflectTest2 :: IO()
reflectTest2 = let c1 = Curve(Point(2.0, 2.0), [Point(4.0, 4.0), Point(-1.0, -2.0)])
                   d = Horizontal 1.0
                   ans = Curve(Point(2.0, 0), [Point(4.0, -2.0), Point(-1.0, 4.0)])
               in print $ "reflectTest2: " ++ show (reflect c1 d == ans)              
             
bboxTest :: IO ()
bboxTest = let c1 = Curve(Point(1.0, 1.0), [Point(2.0, 3.0), Point(0.0, 2.0)])
               ans = (Point(0.0, 1.0), Point(2.0, 3.0))
           in print $ "bboxTest: " ++ show (bbox c1 == ans)
           
widthTest :: IO ()
widthTest = let c1 = Curve(Point(0.0, 1.0), [Point(2.0, 3.0), Point(5.0, 4.0)])
                ans = 5
            in print $ "widthTest: " ++ show (width c1 == ans)

heightTest :: IO ()
heightTest = let c1 = Curve(Point(0.0, 1.0), [Point(2.0, 3.0), Point(5.0, 4.0)])
                 ans = 3.0
             in print $ "heightTest: " ++ show (height c1 == ans)

toListTest :: IO ()
toListTest = let c1 = Curve(Point(0.0, 1.0), [Point(2.0, 3.0), Point(5.0, 4.0)])
                 ans = [Point(0.0, 1.0), Point(2.0, 3.0), Point(5.0, 4.0)]
             in print $ "toListTest: " ++ show (toList c1 == ans) 
             
normalizeTest :: IO ()
normalizeTest = let c1 = Curve(Point(-5.0, -4.0), [Point(-2.0, 4.0)])
                    ans = Curve(Point(0.0, 0.0), [Point(3.0, 8.0)])
                in print $ "normalizeTest: " ++ show (normalize c1 == ans)
                
testAll :: IO ()
testAll = do pointTest
             pointXTest
             pointYTest
             curveEqTest1
             curveEqTest2
             curveTest
             connectTest
             rotateTest
             translateTest
             reflectTest1
             reflectTest2
             bboxTest
             widthTest
             heightTest
             toListTest
             normalizeTest
             
hilbertTest :: IO ()
hilbertTest = toFile (hilbert $ hilbert $ hilbert $ hilbert $ curve(Point(0.0, 0.0)) ([])) "output.svg"
