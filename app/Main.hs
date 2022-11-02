module Main where
import ActionFunction (chooseMode, gameLottoPlayC, gameLottoPlayR, startCredit, welcome,
                       welcomeChooseMode, welcomeCustomMode, welcomeRandomMode)

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


