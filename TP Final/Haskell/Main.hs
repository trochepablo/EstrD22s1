import Img
import qualified Matrix as M
import Color as C

img1 :: Img
img1 = createImg source1
source1 :: [[Color]]
source1 = 
 [ [red, red]
 , [red, blue]      
 ]

c :: Color
c = cyan
p :: Color
p = green4 

img2 :: Img
img2 = createImg source2
source2 :: [[Color]]
source2 =
 [ [ c     , c     , c     , c     , c     , c     , c     , c      ]
 , [ c     , c     , green1, green3, green3, green3, c     , c      ]
 , [ c     , green1, green1, green2, green2, green3, green3, c      ]
 , [ c     , green1, green2, green2, green2, green2, green3, c      ]
 , [ p     , p     , green1, green2, green2, green3, p     , p      ]
 , [ p     , p     , p     , brown1, brown2, p     , p     , p      ]
 , [ p     , p     , p     , brown1, brown2, p     , p     , p      ]
 , [ p     , p     , p     , p     , p     , p     , p     , p      ]
 ]

img3 :: Img
img3 = createImg source3
source3 :: [[Color]]
source3 = 
 [ [green1, green2, yellow, red    ]
 , [green3, green4, yellow, red    ]      
 , [cyan  , cyan  , yellow, red    ]      
 , [brown1, cyan  , yellow, red    ]      
 ]