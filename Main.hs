{-# Language ScopedTypeVariables, BlockArguments, DeriveTraversable, OverloadedStrings, ImportQualifiedPost #-}
{-# Options_GHC -Wno-unused-imports #-}
{-# Options_GHC -w #-}
module Main where

import Data.Foldable
import Data.Function (on)
import Data.List (intercalate, intersperse, transpose, nub, groupBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy.IO qualified as Text
import Data.Traversable
import Hledger
import Hledger.Cli qualified as Cli
import Hledger.Utils.Regex qualified as Regex
import Text.Tabular
import Text.Tabular.AsciiWide

mkMap :: [Account] -> Map [Text] MixedAmount
mkMap accts = Map.fromList [(Text.splitOn ":" (aname a), aibalance a) | a <- accts]

cmdMode :: Cli.Mode RawOpts
cmdMode = Cli.addonCommandMode "accountflow"

main :: IO ()
main =
  do copts <- Cli.getHledgerCliOpts cmdMode
     Cli.withJournalDo copts \journal ->
       do let table = report (rsQuery (Cli.reportspec_ copts)) journal
          Text.putStrLn (render True id id (Text.pack . showMixedAmountOneLine) table)


report :: Query -> Journal -> Table Text Text MixedAmount
report query journal
  | Map.null chk_ledger = table
  | otherwise = error ("Erroneous accounts: " ++ show (Map.keys chk_ledger))
  where
    mk q = mkMap (drop 1 (laccounts (ledgerFromJournal q journal)))

    Right flowTag = Tag <$> Regex.toRegex "^flow$"

    chk_ledger = mk (And [query, Not (flowTag Nothing)])

    columns :: [Map [Text] MixedAmount]
    columns =
      [ ledger
        | tag <- tags
        , let Right re = Regex.toRegex ("^" <> tag <> "$")
        , let ledger = mk (And [query, flowTag (Just re)])
      ] ++ [mk query]

    accounts :: [[[Text]]]
    accounts = divideAccounts
             $ Set.toList
             $ Set.unions
             $ map Map.keysSet columns

    tags :: [Text]
    tags = flowTags journal

    table :: Table Text Text MixedAmount
    table = Table
              (rowHeaders accounts)
              (columnHeaders tags)
              [ Map.findWithDefault 0 a <$> columns
              | a <- concat accounts ]

flowTags :: Journal -> [TagValue]
flowTags journal =
  Set.toList $ Set.fromList
  [ val
    | tx <- jtxns journal
    , px <- tpostings tx
    , ("flow", val) <- ptags px
  ]


rowHeaders :: [[[Text]]] -> Header Text
rowHeaders accounts =
  Group SingleLine
    (map (Group NoLine . map (Header . anPad)) accountNames)
  where
    accountNames :: [[Text]]
    accountNames = map (map nestAccount) accounts

    anLen :: Int
    anLen = maximum (map Text.length (concat accountNames))

    anPad :: Text -> Text
    anPad str = str <> Text.replicate (anLen - Text.length str) "·"

nestAccount :: [Text] -> Text
nestAccount [x] = x
nestAccount []  = ""
nestAccount (_:xs) = "· " <> nestAccount xs

columnHeaders :: [Text] -> Header Text
columnHeaders tags =
  Group DoubleLine
    [Group SingleLine (Header <$> tags), Header "bal"]

divideAccounts :: [[Text]] -> [[[Text]]]
divideAccounts = groupBy ((==) `on` length)
