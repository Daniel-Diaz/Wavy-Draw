
import Data.Sound
import Data.Sound.Draw

main :: IO ()
main = renderFileSound "sine.pdf" $ sine 1 1 1 0