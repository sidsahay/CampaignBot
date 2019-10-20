{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Control.Monad (when, forM_, replicateM, liftM)
import Data.Char (toLower)
import Data.List.Split (splitOn)
import Data.Text.Conversions(convertText)
import Data.Maybe
import System.Random
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Regex.TDFA

import Discord
import Discord.Types
import qualified Discord.Requests as R

campaignBotApp :: IO ()
campaignBotApp = do
    token <- TIO.readFile "./key.key"
    output <- runDiscord $ def { discordToken = token
                               , discordOnStart = startHandler
                               , discordOnEnd = putStrLn "Ended"
                               , discordOnEvent = eventHandler
                               , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
                               }
    TIO.putStrLn output

startHandler :: DiscordHandle -> IO ()
startHandler dis = do
    Right partialGuilds <- restCall dis R.GetCurrentUserGuilds
    forM_ partialGuilds $ \pg -> do
        Right guild <- restCall dis $ R.GetGuild (partialGuildId pg)
        Right chans <- restCall dis $ R.GetGuildChannels (guildId guild)
        case filter isTextChannel chans of
            (c:_) -> do 
                --_ <- restCall dis $ R.CreateMessage (channelId c) "Hello, I'm the Campaign helper bot. Use $ to prefix bot commands which can be looked up at https://github.com/sidsahay/CampaignBot#readme"
                return ()
            _ -> return ()

eventHandler :: DiscordHandle -> Event -> IO ()
eventHandler dis event = case event of
    MessageCreate m -> when (not (fromBot m) && isBotCommand m) $ do
        _ <- restCall dis (R.CreateReaction (messageChannel m, messageId m) "eyes")
        output <- processMessage m
        _ <- restCall dis (R.CreateMessage (messageChannel m) (convertText output))
        return ()
    _ -> return ()

isTextChannel :: Channel -> Bool
isTextChannel (ChannelText {}) = True
isTextChannel _ = False

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isBotCommand :: Message -> Bool
isBotCommand = ("$" `T.isPrefixOf`) . T.map toLower . messageText

messageToString :: Message -> String
messageToString = convertText . messageText

rollDice :: Int -> Int -> IO (Int, [Int])
rollDice mult lim = do
    nums <- replicateM mult $ getStdRandom (randomR (1, lim))
    return (foldl (+) 0 nums, nums)

validInputRegex :: String
validInputRegex = "^[1-9][0-9]*d[1-9][0-9]*((\\+|-)[0-9]+)*$"

sumOfTermsRegex :: String
sumOfTermsRegex = "^[1-9][0-9]*((\\+|-)[0-9]+)+$"

prettyPrint :: (Int, [Int]) -> String
prettyPrint (x, xs) = (show x)  ++ " : " ++ (show xs)

getInt = read :: String -> Int

processSumString :: String -> Int
processSumString s = 
    case s of
        [] -> 0
        op:rest -> 
            let pred x = (x /= '+') && (x /= '-') in
            let (number, remaining) = (getInt $ takeWhile pred rest, dropWhile pred rest) in
                case op of
                    '-' -> -1 * number + processSumString remaining
                    _ -> number + processSumString remaining
        
processCommand :: String -> IO String
processCommand commandString = 
    case splitOn "d" commandString of
        [multString, rest] ->
            let mult = getInt multString in
            case rest =~ sumOfTermsRegex of
                False -> prettyPrint <$> rollDice mult (getInt rest)
                True -> 
                    let pred x = (x /= '+') && (x /= '-') in
                    let (limString, sumString) = (takeWhile pred rest, dropWhile pred rest) in do
                        rolled@(totalVal, _) <- rollDice mult (getInt limString)
                        return $ (show (totalVal + processSumString sumString)) ++ " :: " ++ (prettyPrint rolled)
        _ -> return "Invalid input during splitting."

processMessage :: Message -> IO String
processMessage m = 
    let messageString = filter (/=' ') . tail . messageToString $ m in
    case messageString =~ validInputRegex of
        False -> return "Invalid input."
        True -> processCommand messageString