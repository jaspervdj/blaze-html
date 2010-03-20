import Criterion
import Criterion.Main
import System.IO
import Data.Text (Text(..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

lipsum = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vestibulum ultricies nulla sapien. Mauris eget dui eros. Vivamus eleifend condimentum leo sit amet tempus. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Vestibulum auctor mattis ante vitae dictum. Praesent sed lacus nisl. In molestie ultricies lorem, non pulvinar felis bibendum a. Donec id urna eget enim tincidunt condimentum. Curabitur id nibh ut risus tincidunt faucibus sit amet ut augue. Mauris aliquet leo elementum dui convallis egestas. Curabitur consequat lobortis dapibus. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas."

tlipsum = (T.pack lipsum)

openDevNull :: IO (Handle)
openDevNull = do
    h <- openFile "/dev/null" WriteMode
    hSetBuffering h (BlockBuffering Nothing)
    return h

print_concat_str :: Int -> String -> IO ()
print_concat_str n str = do
    let tstr = concat $ take n (repeat str)
    h <- openDevNull
    hPutStr h tstr
    hFlush h
    hClose h

print_repeat_str :: Int -> String -> IO ()
print_repeat_str n str = do
    h <- openDevNull
    helper n str h
    hFlush h
    hClose h
  where
    helper 1 str h = hPutStr h str  
    helper n str h = hPutStr h str >> helper (n - 1) str h

print_concat_text :: Int -> Text -> IO ()
print_concat_text n str = do
    let tstr = T.concat $ take n (repeat str)
    h <- openDevNull
    TIO.hPutStr h tstr
    hFlush h
    hClose h

print_repeat_text :: Int -> Text -> IO ()
print_repeat_text n str = do
    h <- openDevNull
    helper n str h
    hFlush h
    hClose h
  where
    helper 1 str h = TIO.hPutStr h str  
    helper n str h = TIO.hPutStr h str >> helper (n - 1) str h


main = tlipsum `seq` defaultMain [
    bgroup "simple"
        [ bench "print_concat_str" $ print_concat_str 1000 lipsum
        , bench "print_repeat_str" $ print_repeat_str 1000 lipsum
        , bench "print_concat_text" $ print_concat_text 1000 tlipsum
        , bench "print_repeat_text" $ print_repeat_text 1000 tlipsum 
        ]
    ]
