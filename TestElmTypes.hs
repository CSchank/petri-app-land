module TestElmTypes where

import Types
import Generate.Types

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
						  ]

testBinTree :: ElmCustom
testBinTree = ElmCustom "BinTree" [("Branch", 
									[ (ElmType $ ElmCustom "BinTree" [], "left", "the left subtree of the node")
									, (ElmInt, "n", "defines the green value of the RGB colour")
									, (ElmType $ ElmCustom "BinTree" [], "right", "the right subtree of the node")
						 			]
						  	)
						  , ("Leaf", 
									[ (ElmType $ testRGB, "n", "the value of the leaf")
						 			]
						  	)
						  ]