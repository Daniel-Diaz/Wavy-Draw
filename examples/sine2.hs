
import Data.Sound
import Data.Sound.Draw

main :: IO ()
main = renderFileSound "sine2.pdf" $ sine 1 1 1 0 <|> sine 1 1 1 (pi/2)