module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, nubBy)
import Data.Maybe (Maybe)


type Entry =
  { firstName :: String
  , lastName :: String
  , address :: Address
  }

type Address =
  { street :: String
  , city :: String
  , state :: String
  }


type AddressBook = List Entry


showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
                  entry.firstName <> ": " <>
                  showAddress entry.address


showAddress :: Address -> String
showAddress addr = addr.street <> ", " <> addr.city <> ", " <> addr.state


emptyBook :: AddressBook
emptyBook = empty


insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons


findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry first last = head <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry e = e.firstName == first && e.lastName == last


findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter streetFilter
  where
    streetFilter :: Entry -> Boolean
    streetFilter e = e.address.street == street


printEntry :: String -> String -> AddressBook -> Maybe String
printEntry first last book = map showEntry (findEntry first last book)


removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy nameEqual
  where
    nameEqual :: Entry -> Entry -> Boolean
    nameEqual e1 e2 = e1.firstName == e2.firstName && e1.lastName == e2.lastName


defaultEntry :: Entry
defaultEntry =
  { firstName: "Eric"
  , lastName: "Stolten"
  , address: { street: "123 foo bar ln"
             , city: "barville"
             , state: "PA"
             }
  }


defaultAB :: AddressBook
defaultAB = insertEntry defaultEntry empty


duplicateAB :: AddressBook
duplicateAB = insertEntry defaultEntry (insertEntry defaultEntry empty)
