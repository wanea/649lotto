module Main where
import ActionFunction (chooseMode, gameLottoPlayC, gameLottoPlayR, playLottoC, playLottoR,
                       startCredit, welcome, welcomeChooseMode, welcomeCustomMode,
                       welcomeRandomMode)

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
                let init = playLottoC  startCredit
                let lottoC = gameLottoPlayC init
                lottoC >> mode

randomMode :: IO ()
randomMode = do
                let init = playLottoR  startCredit
                let lottoR = gameLottoPlayR init
                lottoR >> mode


