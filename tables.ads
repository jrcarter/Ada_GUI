--                                                                    --
--  package Tables                  Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2000       --
--                                                                    --
--                                Last revision :  13:11 14 Sep 2019  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--
--
-- The  package  Tables  provides tables searched using string keys. The
-- binary search is used for names of known length. It is also  possible
-- to search a table for names of unknown length, i.e. to parse a string
-- using the table. In this case the search time is near to  logarthmic,
-- but in worst case can be linear (when table contains tokens like "a",
-- "aa",  "aaa"  and  so  on). The package is generic. The instantiation
-- parameter  is  the type of the data tag associated with a table item.
-- The  table  is  initially  empty. It is automatically enlarged as new
-- items are added. Upon destruction the memory used by the the table is
-- reclaimed. Items in the table can be accessed either by their offsets
-- (in alphabethical order) or by names. Text parsing is also supported.
-- Table assignment makes a deep copy.
--
-- Design  notes.  Unfortunately the type Table needs to be a controlled
-- type.  This  makes  instantiation possible at the library level only.
-- This is surely a design flaw (of controlled types).
--
with Ada.Finalization;

generic
   type Tag is private;
package Tables is
   type Table is new Ada.Finalization.Controlled with private;
--
-- Add -- Insert an item
--
--    Folder   - To be modified
--    Name     - Of the item
--    Data     - The tag associated with the item
--  [ Offset ] - Of the item (output)
--
-- This procedure is used to add an item. It raises an exception  if  an
-- item with the same name is already in the table. The parameter Offset
-- when used, is set the offset  of  the  newly  added  item.  See  also
-- Replace.
--
-- Exceptions :
--
--    Name_Error - An item with the name is already in the table
--
   procedure Add
             (  Folder : in out Table;
                Name   : String;
                Data   : Tag
             );
   procedure Add
             (  Folder : in out Table;
                Name   : String;
                Data   : Tag;
                Offset : out Positive
             );
--
-- Adjust -- Assignment
--
--    Folder - The target
--
   procedure Adjust (Folder : in out Table);
--
-- Delete -- Remove item
--
--    Folder - To be modified
--    Name   - Of the item
--
-- This procedure is used to remove item from the table. Nothing happens
-- when there is no item with the given name.
--
   procedure Delete (Folder : in out Table; Name : String);
--
-- Delete -- Remove item by its offset
--
--    Folder - The table
--    Offset - The offset of the item to be deleted
--
-- Exceptions :
--
--    End_Error - There is no such item
--
   procedure Delete
             (  Folder : in out Table;
                Offset : Positive
             );
--
-- Erase -- Remove all items
--
--    Folder - To be modified
--
-- This procedure is used to remove all items from the table.
--
   procedure Erase (Folder : in out Table);
--
-- Finalize -- Destructor
--
--    Folder - To be destroyed
--
   procedure Finalize (Folder : in out Table);
--
-- Find -- Search table for an item
--
--    Folder - To be searched
--    Name   - Of the item
--
-- Returns :
--
--    Tag of the found item
--
-- Exceptions :
--
--    End_Error - There is no such item
--
   function Find (Folder : Table; Name : String) return Tag;
--
-- Get -- Parse
--
--    Source   - The source string to be parsed
--    Pointer  - The current string position
--    Folder   - To be searched
--    Data     - The tag associated with the found item
--  [ Got_It ] - True an item was matched
--
-- This procedure is used to parse the string specified by the parameter
-- Source using the table Folder. The procedure tries to find  the  item
-- with the longest name that matches the string  Source  starting  from
-- the position defined by the parameter Pointer. On success Pointer  is
-- advanced  to the first position following the name of the found item.
-- I.e.  Source (OldPointer..Pointer - 1) is the name of the found item.
-- Note  that  unlike Find that searches tables in logarithmic time, Get
-- could require linear time in some pathological cases.
--
-- Exceptions :
--
--    End_Error    - There is no such item
--    Layout_Error - Pointer is not in Source'First..Source'Last+1
--
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Folder  : Table;
                Data    : out Tag
             );
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Folder  : Table;
                Data    : out Tag;
                Got_It  : out Boolean
             );
--
-- GetName -- Get item name
--
--    Folder - The table
--    Offset - Of the item
--
-- This  function  is used to get the name of a table item. All items in
-- the table are enumerated in alphabetical order. The  first  item  has
-- the offset 1. See also GetSize.
--
-- Returns :
--
--    The name
--
-- Exceptions :
--
--    End_Error - There is no such item
--
   function GetName (Folder : Table; Offset : Positive) return String;
--
-- GetSize -- Get the number of items in the table
--
--    Folder - The table
--
-- Returns :
--
--    The number of items
--
   function GetSize (Folder : Table) return Natural;
--
-- GetTag -- Get item tag
--
--    Folder - The table
--    Offset - Of the item
--
-- This function is used to get the tag of a table item.  All  items  in
-- the table are enumerated in alphabetical order. The  first  item  has
-- the offset 1. See also GetSize.
--
-- Returns :
--
--    The tag
--
-- Exceptions :
--
--    End_Error - There is no such item
--
   function GetTag (Folder : Table; Offset : Positive) return Tag;
--
-- IsIn -- Membership test
--
--    Folder - To be searched
--    Name   - Of the item
--
-- Returns :
--
--    True if Folder contains an item for Name
--
   function IsIn (Folder : Table; Name : String) return Boolean;
--
-- Locate -- Search table for an item
--
--    Folder - To be searched
--    Name   - Of the item
--
-- Returns :
--
--    Offset to the item or else 0
--
   function Locate (Folder : Table; Name : String) return Natural;
--
-- Locate -- Parse
--
--    Source  - The source string to be parsed
--    Pointer - The current string position
--    Folder  - To be searched
--    Offset  - To the found item or else 0
--
-- This procedure is similar to Get, but returns the found  item  offset
-- instead.
--
-- Exceptions :
--
--    Layout_Error - Pointer is not in Source'First..Source'Last+1
--
   procedure Locate
             (  Source  : String;
                Pointer : in out Integer;
                Folder  : Table;
                Offset  : out Natural
             );
--
-- Replace -- Insert or replace an item
--
--    Folder   - To be modified
--    Name     - Of the item
--    Data     - The tag associated with the item
--  [ Offset ] - Of the item (output)
--
-- This procedure is used to add an item. If the table already  contains
-- an item with the same name  it  is  replaced  by  the  new  one.  The
-- parameter Offset, when present, is set to the offset of the item. See
-- also Add.
--
   procedure Replace
             (  Folder : in out Table;
                Name   : String;
                Data   : Tag
             );
   procedure Replace
             (  Folder : in out Table;
                Name   : String;
                Data   : Tag;
                Offset : out Positive
             );
--
-- Replace -- Replace an item
--
--    Folder - To be modified
--    Offset - Of the item
--    Data   - The tag associated with the item
--
-- This  procedure  is used to replace an item. The item is specified by
-- its offset 1..GetSize.
--
-- Exceptions :
--
--    End_Error - There is no such item
--
   procedure Replace
             (  Folder : in out Table;
                Offset : Positive;
                Data   : Tag
             );

private
   pragma Inline (Find);
   pragma Inline (Get);
   pragma Inline (GetSize);
   pragma Inline (GetTag);
   pragma Inline (IsIn);
   pragma Inline (Replace);

   type Token (Length : Natural) is record
      Data : Tag;
      Name : String (1..Length);
   end record;

   type TokenPtr is access Token;
   type TokenList is array (Positive range <>) of TokenPtr;
   type TokenListPtr is access TokenList;

   type Table is new Ada.Finalization.Controlled with record
      Size : Natural      := 0;
      List : TokenListPtr := null;
   end record;

   type Equality is (Less, Equal, Greater);
--
-- Insert -- Insert the item
--
--    Folder - The table
--    Place  - The offset of the new item
--    Name   - Its name
--    Data   - Its tag
--
-- This procedure is used internally to  insert  a  new  item  into  the
-- table. No checks made!
--
   procedure Insert
             (  Folder : in out Table;
                Place  : Positive;
                Name   : String;
                Data   : Tag
             );
--
-- Search -- The table
--
--    Folder - The table to be searched
--    Name   - To be searched for
--
-- Returns :
--
--    [+]  The offset of the found item
--    [-]  The offset where the item could be
--
   function Search (Folder : Table; Name : String) return Integer;

end Tables;
