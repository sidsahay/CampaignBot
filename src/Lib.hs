{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib (campaignBotApp) where

import Control.Monad (when, forM_, replicateM, liftM)
import Data.Char (toLower)
import Data.List.Split (splitOn)
import Data.Text.Conversions(convertText)
import System.Random
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

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
                _ <- restCall dis $ R.CreateMessage (channelId c) "Hello, I'm the Campaign helper bot. Use $ to prefix bot commands which can be looked up at https://github.com/sidsahay/CampaignBot#readme"
                return ()
            _ -> return ()

eventHandler :: DiscordHandle -> Event -> IO ()
eventHandler dis event = case event of
    MessageCreate m -> when (not (fromBot m) && isBotCommand m) $ do
        _ <- restCall dis (R.CreateReaction (messageChannel m, messageId m) "eyes")
        rolledVal <- processMessage m
        _ <- restCall dis (R.CreateMessage (messageChannel m) (convertText . show $ rolledVal))
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

processMessage :: Message -> IO Int
processMessage m = let sm = splitOn "d" . tail . messageToString $ m in
    case length sm of
        2 -> let mult :: Int = read (sm !! 0) in
             let lim :: Int = read (sm !! 1) in
                liftM (foldl (+) 0) (replicateM mult $ getStdRandom (randomR (1, lim)))
        _ -> return 0