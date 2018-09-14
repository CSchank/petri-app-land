module TestElmTypes where

import Types
import Generate.Types
import Generate.Codec
import Data.Map as M

testRGB :: ElmCustom
testRGB = ElmCustom "Colour" [("RGB", 
									[ (ElmIntRange 0 255, "red", "defines the red value of the RGB colour")
									, (ElmIntRange 0 255, "green", "defines the green value of the RGB colour")
									, (ElmIntRange 0 255, "blue", "defines the blue value of the RGB colour")
						 			]
						  	)
						  , ("HSL", 
									[ (ElmFloatRange 0 1 7, "hue", "defines the hue value of the HSL colour")
									, (ElmFloatRange 0 1 7, "saturation", "defines the saturation value of the HSL colour")
									, (ElmFloatRange 0 1 7, "light", "defines the light value of the HSL colour")
						 			]
						  	)
						  , ("RGBA", 
									[ (ElmIntRange 0 255, "red", "defines the red value of the RGBA colour")
									, (ElmIntRange 0 255, "green", "defines the green value of the RGBA colour")
									, (ElmIntRange 0 255, "blue", "defines the blue value of the RGBA colour")
									, (ElmFloatRange 0 1 7, "alpha", "defines the alpha value of the RGBA colour")
						 			]
						  	)
						  , ("HSLA", 
									[ (ElmFloatRange 0 1 7, "hue", "defines the hue value of the HSL colour")
									, (ElmFloatRange 0 1 7, "saturation", "defines the saturation value of the HSL colour")
									, (ElmFloatRange 0 1 7, "light", "defines the light value of the HSL colour")
									, (ElmFloatRange 0 1 7, "alpha", "defines the alpha value of the HSLA colour")
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

testPoint :: ElmCustom
testPoint = ElmCustom "Point" [("Point",
										[(ElmPair (ElmFloatRange (-1) 1 7,"x","") (ElmFloatRange (-1) 1 7,"y",""),"point","")
										]
							  )]

testPoint3D :: ElmCustom
testPoint3D = ElmCustom "Point3D" [("Point3D",
										[(ElmTriple (ElmFloatRange (-1) 1 7,"x","") (ElmFloatRange (-1) 1 7,"y","") (ElmFloatRange (-1) 1 7,"z",""),"point","")
										]
							  )]

test = M.fromList [("Colour", testRGB), ("BinTree", testBinTree)]