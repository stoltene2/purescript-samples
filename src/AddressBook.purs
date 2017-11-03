module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, nubBy)
import Data.Maybe (Maybe)


newtype Entry = Entry
  { firstName :: String
  , lastName :: String
  , address :: Address
  }

newtype Address = Address
  { street :: String
  , city :: String
  , state :: String
  }


type AddressBook = List Entry

address :: String -> String -> String -> Address
address street city state = Address {street, city, state}


instance showEntry :: Show Entry where
  show (Entry entry) = entry.lastName <> ", " <>
                       entry.firstName <> ": " <>
                       show entry.address

instance eqEntry :: Eq Entry where
  eq (Entry e1) (Entry e2) = e1.firstName == e2.firstName && e1.lastName == e2.lastName


instance showAddress :: Show Address where
  show (Address a) = a.street <> ", " <> a.city <> ", " <> a.state


emptyBook :: AddressBook
emptyBook = empty


insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons


findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry first last = head <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry (Entry e) = e.firstName == first && e.lastName == last


findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter streetFilter
  where
    streetFilter :: Entry -> Boolean
    streetFilter (Entry e) =
      let
        (Address addr) = e.address
      in addr.street == street


printEntry :: String -> String -> AddressBook -> Maybe String
printEntry first last book = map show (findEntry first last book)


removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy eq
  -- where
  --   nameEqual :: Entry -> Entry -> Boolean
  --   nameEqual e1 e2 = e1.firstName == e2.firstName && e1.lastName == e2.lastName


defaultEntry :: Entry
defaultEntry =
  Entry { firstName: "Eric"
  , lastName: "Stolten"
  , address: Address { street: "123 foo bar ln"
             , city: "barville"
             , state: "PA"
             }
  }


defaultAB :: AddressBook
defaultAB = insertEntry defaultEntry empty


duplicateAB :: AddressBook
duplicateAB = insertEntry defaultEntry (insertEntry defaultEntry empty)
