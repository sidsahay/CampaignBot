{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import qualified Expression as E

import Control.Monad (when, forM_, replicateM, liftM)
import Data.Char (toLower)
import Data.Text.Conversions (convertText)
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
        return ()

eventHandler :: DiscordHandle -> Event -> IO ()
eventHandler dis event = case event of
    MessageCreate m -> when (not (fromBot m) && isBotCommand m) $ do
        _ <- restCall dis (R.CreateReaction (messageChannel m, messageId m) "eyes")
        output <- processMessage m
        let outputWithUserName = (getAuthorName m) ++ "```" ++ output ++ "```"
        let reply = R.CreateMessage (messageChannel m) (convertText outputWithUserName)
        _ <- restCall dis reply
        return ()
    _ -> return ()

isTextChannel :: Channel -> Bool
isTextChannel (ChannelText {}) = True
isTextChannel _ = False

getAuthorName = convertText . userName . messageAuthor

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isBotCommand :: Message -> Bool
isBotCommand = ("$" `T.isPrefixOf`) . T.map toLower . messageText

messageToString :: Message -> String
messageToString = convertText . messageText

processMessage :: Message -> IO String
processMessage m = E.processStr . (++E.eofString) . tail . messageToString $ m