module Main  where
import A1Lexer
import A1Parser
import Types

main::IO()
main = do
    s<-getContents
    --print (alexScanTokens s)
    let program = calc (alexScanTokens s)
    print program
