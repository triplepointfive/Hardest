module Levels where

type Pos = (Double, Double)

data Direction = ClockWise
                 |CounterClockWise
                 deriving ( Eq, Show, Read )

data Character = Static
                 |Perpet    { characterNodes   :: [Pos]
                            , characterCurrent :: Int
                            , characterSpeed   :: Double
                            }
                 |Circle    { characterRadius   :: Double
                            , characterSpeed    :: Double
                            , characterAngle    :: Double
                            , characterCenter   :: Pos
                            }
                 |FrontBack { characterNodes    :: [Pos]
                            , characterCurrent  :: Int
                            , characterDir      :: Direction
                            , characterSpeed    :: Double
                            }
                 deriving ( Eq, Show, Read )

data Beast = Player { beastPosition   :: Pos
                    , beastDeathCount :: Int
                    , beastVelocity   :: Double
                    }
             |Enemy { beastPosition :: Pos
                    , beastMovement :: Character
                    }
             deriving ( Eq, Show, Read )

data Level = Level  { levelStartPos :: Pos
                    , levelEnemies  :: [Beast]
                    , levelZones    :: [Zone]
                    , levelGolds    :: [Gold]
                    }
             |Saver { levelText     :: String
                    , levelLength   :: Double
                    , levelStartPos :: Pos
                    }
             deriving ( Eq, Show, Read )

data Gold = Gold { goldPos :: Pos
                 , goldTaked   :: Bool
                 } deriving ( Eq, Show, Read )


data Zone = ZoneForbid { upper  :: Pos
                       , bottom :: Pos
                       }
            |ZoneWin   { upper  :: Pos
                       , bottom :: Pos
                       }
            |ZoneAllow { upper  :: Pos
                       , bottom :: Pos
                       }
            deriving ( Eq, Show, Read )

startLevels :: [Level]
startLevels = [
        (Saver "Let's rock123123!" 300 (0, 0)),
        (Level (80, 240) -- 1
         ((Enemy (170, 187) (Perpet [(170, 187), (455, 187)]  1 500)) :
          (Enemy (455, 217) (Perpet [(170, 217), (455, 217)]  0 500)) :
          (Enemy (170, 247) (Perpet [(170, 247), (455, 247)]  1 500)) :
          (Enemy (455, 277) (Perpet [(170, 277), (455, 277)]  0 500)) : [])
           ((ZoneAllow (50, 150) (590, 330)) :
            (ZoneForbid (140, 150) (440, 180)) :
            (ZoneForbid (470, 180) (500, 300)) :
            (ZoneForbid (200, 300) (500, 330)) :
            (ZoneForbid (140, 180) (170, 300)) :
            (ZoneWin (500, 150) (590, 330)) : [])
             ([])),
        (Saver "Tax this!" 300 (0, 0)),
        (Level (135, 240) -- 2
         ((Enemy (177, 150) (Perpet [(177, 150), (177, 315)]  1 300)) :
          (Enemy (207, 315) (Perpet [(207, 150), (207, 315)]  0 300)) :
          (Enemy (237, 150) (Perpet [(237, 150), (237, 315)]  1 300)) :
          (Enemy (267, 315) (Perpet [(267, 150), (267, 315)]  0 300)) :
          (Enemy (297, 150) (Perpet [(297, 150), (297, 315)]  1 300)) :
          (Enemy (327, 315) (Perpet [(327, 150), (327, 315)]  0 300)) :
          (Enemy (357, 150) (Perpet [(357, 150), (357, 315)]  1 300)) :
          (Enemy (387, 315) (Perpet [(387, 150), (387, 315)]  0 300)) :
          (Enemy (417, 150) (Perpet [(417, 150), (417, 315)]  1 300)) :
          (Enemy (447, 315) (Perpet [(447, 150), (447, 315)]  0 300)) : [])
           ((ZoneAllow (80, 150) (560, 330)) :
            (ZoneForbid (80, 150) (170, 210)) :
            (ZoneForbid (80, 270) (170, 330)) :
            (ZoneForbid (470, 150) (560, 210)) :
            (ZoneForbid (470, 270) (560, 330)) :
            (ZoneWin (500, 210) (560, 270)) : [])
             ((Gold (313, 233) False) : [])),
        (Saver "You are good!" 300 (0, 0)),
        (Level (313, 233) -- 3
         ((Enemy (267, 187) (Perpet [(267, 187), (357, 187), (357, 277), (267, 277)]  1 150)) :
          (Enemy (297, 187) (Perpet [(267, 187), (357, 187), (357, 277), (267, 277)]  1 150)) :
          (Enemy (327, 187) (Perpet [(267, 187), (357, 187), (357, 277), (267, 277)]  1 150)) :
          (Enemy (267, 217) (Perpet [(267, 187), (357, 187), (357, 277), (267, 277)]  0 150)) :
          (Enemy (267, 247) (Perpet [(267, 187), (357, 187), (357, 277), (267, 277)]  0 150)) :
          (Enemy (357, 217) (Perpet [(267, 187), (357, 187), (357, 277), (267, 277)]  2 150)) :
          (Enemy (357, 247) (Perpet [(267, 187), (357, 187), (357, 277), (267, 277)]  2 150)) :
          (Enemy (267, 277) (Perpet [(267, 187), (357, 187), (357, 277), (267, 277)]  3 150)) :
          (Enemy (297, 277) (Perpet [(267, 187), (357, 187), (357, 277), (267, 277)]  3 150)) :
          (Enemy (327, 277) (Perpet [(267, 187), (357, 187), (357, 277), (267, 277)]  3 150)) :
          (Enemy (357, 277) (Perpet [(267, 187), (357, 187), (357, 277), (267, 277)]  3 150)) : [])
           ((ZoneAllow (260, 150) (380, 300)) :
            (ZoneForbid (290, 150) (380, 180)) :
            (ZoneWin (290, 210) (350, 270)) : [])
             ((Gold (267, 157) False) : [])),
        (Saver "Peace" 300 (0, 0)),
        (Level (357, 90) -- 4
         ((Enemy (357, 272) (Static)) :
          (Enemy (357, 272) (Circle 24 (-2.6) (pi/2) (357, 272))) :
          (Enemy (357, 272) (Circle 48 (-2.6) (pi/2) (357, 272))) :
          (Enemy (357, 272) (Circle 72 (-2.6) (pi/2) (357, 272))) :
          (Enemy (357, 272) (Circle 96 (-2.6) (pi/2) (357, 272))) :
          (Enemy (357, 272) (Circle 120 (-2.6) (pi/2) (357, 272))) :
          (Enemy (357, 272) (Circle 24 (-2.6) (-pi/2) (357, 272))) :
          (Enemy (357, 272) (Circle 48 (-2.6) (-pi/2) (357, 272))) :
          (Enemy (357, 272) (Circle 72 (-2.6) (-pi/2) (357, 272))) :
          (Enemy (357, 272) (Circle 96 (-2.6) (-pi/2) (357, 272))) :
          (Enemy (357, 272) (Circle 120 (-2.6) (-pi/2) (357, 272))) :
          (Enemy (357, 272) (Circle 24 (-2.6) pi (357, 272))) :
          (Enemy (357, 272) (Circle 48 (-2.6) pi (357, 272))) :
          (Enemy (357, 272) (Circle 72 (-2.6) pi (357, 272))) :
          (Enemy (357, 272) (Circle 96 (-2.6) pi (357, 272))) :
          (Enemy (357, 272) (Circle 120 (-2.6) pi (357, 272))) :
          (Enemy (357, 272) (Circle 24 (-2.6) 0 (357, 272))) :
          (Enemy (357, 272) (Circle 48 (-2.6) 0 (357, 272))) :
          (Enemy (357, 272) (Circle 72 (-2.6) 0 (357, 272))) :
          (Enemy (357, 272) (Circle 96 (-2.6) 0 (357, 272))) :
          (Enemy (357, 272) (Circle 120 (-2.6) 0 (357, 272))) : [])
           ((ZoneAllow (155, 75) (485, 405)) :
            (ZoneForbid (155, 75) (335, 165)) :
            (ZoneForbid (395, 75) (485, 165)) :
            (ZoneForbid (155, 165) (305, 195)) :
            (ZoneForbid (425, 165) (485, 195)) :
            (ZoneForbid (155, 195) (275, 225)) :
            (ZoneForbid (455, 195) (485, 225)) :
            (ZoneForbid (155, 225) (245, 255)) :
            (ZoneForbid (155, 315) (245, 345)) :
            (ZoneForbid (455, 345) (485, 375)) :
            (ZoneForbid (155, 345) (275, 375)) :
            (ZoneForbid (425, 375) (485, 405)) :
            (ZoneForbid (155, 375) (305, 405)) :
            (ZoneWin (155, 255) (245, 315)) : [])
             ((Gold (357, 187) False) :
              (Gold (357, 357) False) :
              (Gold (447, 277) False) : [])),
        (Level (323, 353) --13
         ((Enemy (177, 150) (Perpet [(177, 150), (177, 315)]  1 300)) :
          (Enemy (207, 315) (Perpet [(207, 150), (207, 315)]  0 300)) :
          (Enemy (237, 150) (Perpet [(237, 150), (237, 315)]  1 300)) :
          (Enemy (267, 315) (Perpet [(267, 150), (267, 315)]  0 300)) :
          (Enemy (297, 150) (Perpet [(297, 150), (297, 315)]  1 300)) :
          (Enemy (327, 315) (Perpet [(327, 150), (327, 315)]  0 300)) :
          (Enemy (357, 150) (Perpet [(357, 150), (357, 315)]  1 300)) :
          (Enemy (387, 315) (Perpet [(387, 150), (387, 315)]  0 300)) :
          (Enemy (417, 150) (Perpet [(417, 150), (417, 315)]  1 300)) :
          (Enemy (447, 315) (Perpet [(447, 150), (447, 315)]  0 300)) :
          (Enemy (170, 217) (Perpet [(170, 217), (455, 217)]  1 500)) :
          (Enemy (455, 247) (Perpet [(170, 247), (455, 247)]  0 500)) :[])
           ((ZoneAllow (170, 90) (470, 390)) :
            (ZoneForbid (170, 90) (290, 150)) :
            (ZoneForbid (170, 330) (290, 390)) :
            (ZoneForbid (350, 90) (470, 150)) :
            (ZoneForbid (350, 330) (470, 390)) :
            (ZoneWin (290, 90) (350, 150)) : [])
             ((Gold (313, 233) False) : [])),
        (Level (67, 136) -- 21
         ((Enemy (117, 120) (Perpet [(117, 120), (117, 344)]  1 400)) :
          (Enemy (147, 344) (Perpet [(147, 120), (147, 344)]  0 300)) :
          (Enemy (177, 120) (Perpet [(177, 120), (177, 344)]  1 200)) :
          (Enemy (207, 344) (Perpet [(207, 120), (207, 344)]  0 400)) :
          (Enemy (237, 120) (Perpet [(237, 120), (237, 344)]  1 300)) :
          (Enemy (267, 344) (Perpet [(267, 120), (267, 344)]  0 200)) :
          (Enemy (297, 120) (Perpet [(297, 120), (297, 344)]  1 400)) :
          (Enemy (327, 344) (Perpet [(327, 120), (327, 344)]  0 300)) :
          (Enemy (357, 120) (Perpet [(357, 120), (357, 344)]  1 200)) :
          (Enemy (387, 344) (Perpet [(387, 120), (387, 344)]  0 400)) :
          (Enemy (417, 120) (Perpet [(417, 120), (417, 344)]  1 300)) :
          (Enemy (447, 344) (Perpet [(447, 120), (447, 344)]  0 200)) :
          (Enemy (477, 120) (Perpet [(477, 120), (477, 344)]  1 400)) :
          (Enemy (507, 344) (Perpet [(507, 120), (507, 344)]  0 300)) : [])
           ((ZoneAllow (50, 120) (590, 360)) :
            (ZoneForbid (50, 180) (110, 360)) :
            (ZoneForbid (530, 120) (590, 300)) :
            (ZoneWin (530, 300) (590, 360)) : [])
             ((Gold (117, 337) False) :
              (Gold (507, 127) False) : [])),
        (Level (40, 40)
         ((Enemy (200, 300) (Circle 100 0.001 0 (200, 200))) :
          (Enemy (400, 300) (Circle 100 (-0.003) 0 (400, 200))) :
          (Enemy (200, 200) (Perpet [(200, 200), (300, 300), (200, 400), (100, 300)]  0 0.1)) :
          (Enemy (100, 100) (Perpet [(100, 100), (100, 364), (524, 364), (524, 100)]  0 0.4)) : [])
           ((ZoneAllow (0, 0) (640, 480)) :
            (ZoneForbid (100, 100) (540, 380)) :
            (ZoneForbid (150, 0) (490, 80)) :
            (ZoneWin (540, 380) (640, 450)) : []) [])]

