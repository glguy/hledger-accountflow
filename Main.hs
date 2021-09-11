{-# Language ScopedTypeVariables, BlockArguments, DeriveTraversable, OverloadedStrings, ImportQualifiedPost #-}
module Main where

import Data.Function (on)
import Data.List (groupBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy.IO qualified as Text
import Hledger
import Hledger.Cli qualified as Cli
import Hledger.Utils.Regex qualified as Regex
import Text.Tabular
import Text.Tabular.AsciiWide

mkMap :: [Account] -> Map [Text] MixedAmount
mkMap accts = Map.fromList [(Text.splitOn ":" (aname a), aibalance a) | a <- accts]

cmdMode :: Cli.Mode RawOpts
cmdMode = Cli.hledgerCommandMode
  "accountflow\n\
  \Pivot accounts on a flow tag.\n\
  \_FLAGS\n"
  [Cli.flagReq ["tag"] (\x y -> Right (setopt "tag" x y)) "TAG" "Pivot tag (default: \"flow\")"
  ,Cli.flagNone ["total"] (setboolopt "total") "Show account totals"]
  [Cli.generalflagsgroup1]
  []
  ([], Just (Cli.argsFlag "[QUERY]"))

main :: IO ()
main =
  do copts <- Cli.getHledgerCliOpts cmdMode

     let rawopts = Cli.rawopts_ copts
         tag | Cli.inRawOpts "tag" rawopts = Text.pack (Cli.stringopt "tag" rawopts)
             | otherwise = "flow" 
         total = Cli.boolopt "total" rawopts
     Cli.withJournalDo copts \journal ->
       do let table = report tag total (rsQuery (Cli.reportspec_ copts)) journal
          Text.putStrLn (render True id id (Text.pack . showMixedAmountOneLine) table)


report :: TagName -> Bool -> Query -> Journal -> Table Text Text MixedAmount
report tag showTotal query journal
  | Map.null chk_ledger = table
  | otherwise = error ("Erroneous accounts: " ++ show (Map.keys chk_ledger))
  where
    mk q = mkMap (drop 1 (laccounts (ledgerFromJournal q journal)))

    Right tagQuery = Tag <$> Regex.toRegex ("^" <> tag <> "$")

    chk_ledger = mk (And [query, Not (tagQuery Nothing)])

    columns :: [Map [Text] MixedAmount]
    columns =
      [ ledger
        | val <- tags
        , let Right re = Regex.toRegex ("^" <> val <> "$")
        , let ledger = mk (And [query, tagQuery (Just re)])
      ] ++ [mk query | showTotal]

    accounts :: [[[Text]]]
    accounts = divideAccounts
             $ Set.toList
             $ Set.unions
             $ map Map.keysSet columns

    tags :: [Text]
    tags = tagValues tag journal

    table :: Table Text Text MixedAmount
    table = Table
              (rowHeaders accounts)
              (columnHeaders tags showTotal)
              [ Map.findWithDefault 0 a <$> columns
              | a <- concat accounts ]

tagValues :: TagName -> Journal -> [TagValue]
tagValues tag journal =
  Set.toList $ Set.fromList
  [ val
    | tx <- jtxns journal
    , px <- tpostings tx
    , (name, val) <- ptags px
    , name == tag
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

columnHeaders :: [Text] -> Bool -> Header Text
columnHeaders tags showTotal =
  Group DoubleLine $
    [Group SingleLine (Header <$> tags)] ++
    [Header "total" | showTotal]

divideAccounts :: [[Text]] -> [[[Text]]]
divideAccounts = groupBy ((==) `on` length)
