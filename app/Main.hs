module Main where
import ActionFunction (addCredit, chooseMode, gameLottoPlayC, gameLottoPlayR, playLottoC,
                       startCredit, welcome, welcomeChooseMode, welcomeCustomMode,
                       welcomeRandomMode)
import Control.Monad.Trans.State
import Type

main :: IO ()
main = do
    welcome
    >> mode

mode :: IO ()
mode = do
    way <- welcomeChooseMode >> chooseMode
    if way then welcomeCustomMode >> customMode
    else welcomeRandomMode >> randomMode

customMode :: IO ()
customMode = do
                gameLottoPlayC startCredit >> mode

randomMode :: IO ()
randomMode = do
                gameLottoPlayR startCredit >> mode


