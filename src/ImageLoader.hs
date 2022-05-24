module ImageLoader where

import Codec.Picture (readImage, pixelAt, decodeImage, convertRGB8)
import Codec.Picture.Types
import qualified Data.Vector as Vec
import Data.Vector ((!))
import PietTypes

imgToColorGrid :: Image PixelRGB8 -> ImageGrid
imgToColorGrid img@(Image w h _) = Vec.fromList [
  Vec.fromList [pixelToColor $ pixelAt img x y | x <- [0..w-1]] | y <- [0..h-1]
]

configureProgram :: ImageGrid -> CodelSize -> PietProgram
configureProgram img cs | Vec.length img < 1 = error "Image is empty"
configureProgram img cs | Vec.length (img ! 0) < 1 = error "Image is empty"
configureProgram img cs = Prog {
  _grid = img, 
  _height = Vec.length img,
  _width = Vec.length (img ! 0),
  _cs = cs
} 

imageToProgram :: FilePath -> CodelSize -> IO (Either String PietProgram)
imageToProgram path cs = do
  res <- readImage path
  case res of
    Left err -> return $ Left err
    Right img -> let conv = imgToColorGrid $ convertRGB8 img in
        return $ Right (configureProgram conv cs)

pixelToColor :: PixelRGB8 -> Lightness
pixelToColor (PixelRGB8 r g b) = 
  case (r, g, b) of 
      (0, 0, 0) -> Black
      (255, 255, 255) -> White
      (255, 192, 192) -> Light Red
      (255, 255, 192) -> Light Yellow
      (192, 255, 192) -> Light Green
      (192, 255, 255) -> Light Cyan
      (192, 192, 255) -> Light Blue
      (255, 192, 255) -> Light Magenta
      (255, 0, 0)     -> Reg Red
      (255, 255, 0)   -> Reg Yellow
      (0, 255, 0)     -> Reg Green
      (0, 255, 255)   -> Reg Cyan
      (0, 0, 255)     -> Reg Blue
      (255, 0, 255)   -> Reg Magenta
      (192, 0, 0)     -> Dark Red
      (192, 192, 0)   -> Dark Yellow
      (0, 192, 0)     -> Dark Green
      (0, 192, 192)   -> Dark Cyan
      (0, 0, 192)     -> Dark Blue
      (192, 0, 192)   -> Dark Magenta


decodeInstr :: Lightness -> Lightness -> PietInstr
decodeInstr (Light x) (Light y) = 
  case (fromEnum y - fromEnum x) `mod` 6 of
        0 -> Nop
        1 -> Add
        2 -> Div
        3 -> Grt
        4 -> Dup
        5 -> CharIn
decodeInstr (Light x) (Reg y) = 
    case (fromEnum y - fromEnum x) `mod` 6 of
        0 -> Push
        1 -> Sub
        2 -> Mod
        3 -> Ptr
        4 -> Roll
        5 -> IntOut
decodeInstr (Light x) (Dark y) = 
    case (fromEnum y - fromEnum x) `mod` 6 of
        0 -> Pop
        1 -> Mul
        2 -> Not
        3 -> Swi
        4 -> IntIn
        5 -> CharOut
  
decodeInstr (Reg x) (Reg y) = decodeInstr (Light x) (Light y)
decodeInstr (Dark x) (Dark y) = decodeInstr (Light x) (Light y)
decodeInstr (Reg x) (Dark y) = decodeInstr (Light x) (Reg y)
decodeInstr (Dark x) (Light y) = decodeInstr (Light x) (Reg y)
decodeInstr (Reg x) (Light y) = decodeInstr (Light x) (Dark y)
decodeInstr (Dark x) (Reg y) = decodeInstr (Light x) (Dark y) 

decodeInstr l1 l2 
  | l1 == White || l1 == Black = Nop
  | l2 == White || l2 == Black = Nop