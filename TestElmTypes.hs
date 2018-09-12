module TestElmTypes where

import Types
import Generate.Types
import Data.Map as M

testRGB :: ElmCustom
testRGB = ElmCustom "Colour" [("RGB", 
									[ (ElmInt, "red", "defines the red value of the RGB colour")
									, (ElmInt, "green", "defines the green value of the RGB colour")
									, (ElmInt, "blue", "defines the blue value of the RGB colour")
						 			]
						  	)
						  , ("HSL", 
									[ (ElmFloat, "hue", "defines the hue value of the HSL colour")
									, (ElmFloat, "saturation", "defines the saturation value of the HSL colour")
									, (ElmFloat, "light", "defines the light value of the HSL colour")
						 			]
						  	)
						  , ("RGBA", 
									[ (ElmInt, "red", "defines the red value of the RGBA colour")
									, (ElmInt, "green", "defines the green value of the RGBA colour")
									, (ElmInt, "blue", "defines the blue value of the RGBA colour")
									, (ElmFloat, "alpha", "defines the alpha value of the RGBA colour")
						 			]
						  	)
						  , ("HSLA", 
									[ (ElmFloat, "hue", "defines the hue value of the HSL colour")
									, (ElmFloat, "saturation", "defines the saturation value of the HSL colour")
									, (ElmFloat, "light", "defines the light value of the HSL colour")
									, (ElmFloat, "alpha", "defines the alpha value of the HSLA colour")
						 			]
						  	)
						  ]

testBinTree :: ElmCustom
testBinTree = ElmCustom "BinTree" [("Branch", 
									[ (ElmType "BinTree", "left", "the left subtree of the node")
									, (ElmType "Colour", "leafColour", "the colour of the leaf")
									, (ElmType "BinTree", "right", "the right subtree of the node")
						 			]
						  	)
						  , ("Leaf", 
									[ (ElmType "Colour", "leafColour", "the colour of the leaf")
						 			]
						  	)
						  ]

test = M.fromList [("Colour", testRGB), ("BinTree", testBinTree)]