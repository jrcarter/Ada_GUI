--                                                                    --
--  package Tables                  Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
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

with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

with Ada.Unchecked_Deallocation;

package body Tables is
   Increment : constant := 256;

   procedure Free is
      new Ada.Unchecked_Deallocation (Token, TokenPtr);

   procedure Free is
      new Ada.Unchecked_Deallocation (TokenList, TokenListPtr);

   function Search
            (  Folder : Table;
               Name   : String
            )  return Integer is
      function Compare (Item : String) return Equality;
      pragma Inline (Compare);

      function Compare (Item : String) return Equality is
         Index1  : Integer := Name'First;
         Index2  : Integer := Item'First;
         Symbol1 : Character;
         Symbol2 : Character;
      begin
         loop
            if Index1 > Name'Last then
               if Index2 > Item'Last then
                  return Equal;
               else
                  return Less;
               end if;
            elsif Index2 > Item'Last then
               return Greater;
            end if;
            Symbol1 := Name (Index1);
            Symbol2 := Item (Index2);
            if Symbol1 /= Symbol2 then
               if Symbol1 > Symbol2 then
                  return Greater;
               else
                  return Less;
               end if;
            end if;
            Index1 := Index1 + 1;
            Index2 := Index2 + 1;
         end loop;
      end Compare;

      Low  : Integer := 0;
      High : Integer := Folder.Size + 1;
      This : Integer;
   begin
      if High = 1 then
         return -1;
      end if;
      loop
         This := (Low + High) / 2;
         case Compare (Folder.List (This).Name) is
            when Less =>
               High := This;
               if High - Low = 1 then
                  return -This;
               end if;
            when Equal =>
               return This;
            when Greater =>
               Low := This;
               if High - Low = 1 then
                  return -(This + 1);
               end if;
         end case;
      end loop;
   end Search;

   procedure Insert
             (  Folder : in out Table;
                Place  : Positive;
                Name   : String;
                Data   : Tag
             )  is
      List : TokenListPtr;
      Item : constant TokenPtr := new Token'(Name'Length, Data, Name);
   begin
      if Folder.List = null then
         Folder.List := new TokenList (1..Increment);
         Folder.Size := 1;
         Folder.List (1) := Item;
      elsif Folder.List'Length = Folder.Size then
         List := new TokenList (1..Folder.Size + Increment);
         if Place > 1 then
            List (1..Place - 1) := Folder.List (1..Place - 1);
         end if;
         List (Place) := Item;
         if Place <= Folder.Size then
            List (Place + 1..Folder.Size + 1) :=
               Folder.List (Place..Folder.Size);
         end if;
         Free (Folder.List);
         Folder.List := List;
         Folder.Size := Folder.Size + 1;
      else
         for Index in reverse Place..Folder.Size loop
            Folder.List (Index + 1) := Folder.List (Index);
         end loop;
         Folder.List (Place) := Item;
         Folder.Size := Folder.Size + 1;
      end if;
   end Insert;

   procedure Add (Folder : in out Table; Name : String; Data : Tag) is
      Index : constant Integer := Search (Folder, Name);
   begin
      if Index > 0 then
         raise Name_Error;
      else
         Insert (Folder, -Index, Name, Data);
      end if;
   end Add;

   procedure Add
             (  Folder : in out Table;
                Name   : String;
                Data   : Tag;
                Offset : out Positive
             )  is
      Index : Integer := Search (Folder, Name);
   begin
      if Index > 0 then
         raise Name_Error;
      else
         Index := -Index;
         Insert (Folder, Index, Name, Data);
         Offset := Index;
      end if;
   end Add;

   procedure Adjust (Folder : in out Table) is
      List : TokenListPtr;
   begin
      if Folder.Size > 0 then
         List := new TokenList (1..Folder.Size);
         for Index in List'range loop
            List (Index) := new Token'(Folder.List (Index).all);
         end loop;
         Folder.List := List;
      end if;
   end Adjust;

   procedure Delete
             (  Folder : in out Table;
                Offset : Positive
             )  is
   begin
      if Offset > Folder.Size then
         raise End_Error;
      end if;
      Free (Folder.List (Offset));
      if Offset /= Folder.Size then
         Folder.List (Offset .. Folder.Size - 1) :=
            Folder.List (Offset + 1 .. Folder.Size);
      end if;
      Folder.Size := Folder.Size - 1;
   end Delete;

   procedure Delete (Folder : in out Table; Name : String) is
      Index : constant Integer := Search (Folder, Name);
   begin
      if Index > 0 then
         Delete (Folder, Index);
      end if;
   end Delete;

   procedure Erase (Folder : in out Table) is
   begin
      for Item in 1..Folder.Size loop
         Free (Folder.List (Item));
      end loop;
      Folder.Size := 0;
   end Erase;

   procedure Finalize (Folder : in out Table) is
   begin
      for Index in 1..Folder.Size loop
         Free (Folder.List (Index));
      end loop;
      if Folder.List /= null then
         Free (Folder.List);
      end if;
   end Finalize;

   function Find (Folder : Table; Name : String) return Tag is
      Index : constant Integer := Search (Folder, Name);
   begin
      if Index > 0 then
         return Folder.List (Index).Data;
      else
         raise End_Error;
      end if;
   end Find;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Folder  : Table;
                Data    : out Tag
             )  is
      Index : Natural;
   begin
      Locate (Source, Pointer, Folder, Index);
      if Index = 0 then
         raise End_Error;
      else
         Data := Folder.List (Index).Data;
      end if;
   end Get;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Folder  : Table;
                Data    : out Tag;
                Got_It  : out Boolean
             )  is
      Index : Natural;
   begin
      Locate (Source, Pointer, Folder, Index);
      if Index = 0 then
         Got_It := False;
      else
         Got_It := True;
         Data   := Folder.List (Index).Data;
      end if;
   end Get;

   function GetName
            (  Folder : Table;
               Offset : Positive
            )  return String is
   begin
      if Offset > Folder.Size then
         raise End_Error;
      else
         return Folder.List (Offset).Name;
      end if;
   end GetName;

   function GetTag (Folder : Table; Offset : Positive) return Tag is
   begin
      if Offset > Folder.Size then
         raise End_Error;
      else
         return Folder.List (Offset).Data;
      end if;
   end GetTag;

   function GetSize (Folder : Table) return Natural is
   begin
      return Folder.Size;
   end GetSize;

   function IsIn (Folder : Table; Name : String) return Boolean is
   begin
      return Search (Folder, Name) > 0;
   end IsIn;

   function Locate (Folder : Table; Name : String) return Natural is
      Index : constant Integer := Search (Folder, Name);
   begin
      if Index > 0 then
         return Index;
      else
         return 0;
      end if;
   end Locate;

   procedure Locate
             (  Source  : String;
                Pointer : in out Integer;
                Folder  : Table;
                Offset  : out Natural
             )  is
      function Compare (Item : String) return Equality;
      pragma Inline (Compare);

      function Compare (Item : String) return Equality is
         Index1 : Integer := Pointer;
         Index2 : Integer := Item'First;
      begin
         loop
            if Index2 > Item'Last then
               return Equal;
            end if;
            if Index1 > Source'Last then
               return Less;
            end if;
            if Source (Index1) /= Item (Index2) then
               if Source (Index1) > Item (Index2) then
                  return Greater;
               else
                  return Less;
               end if;
            end if;
            Index1 := Index1 + 1;
            Index2 := Index2 + 1;
         end loop;
      end Compare;
      Found : Integer := 0;
      Low   : Integer := 0;
      High  : Integer := Folder.Size + 1;
      This  : Integer;
   begin
      if (  Pointer < Source'First
         or else
            (  Pointer > Source'Last
            and then
               Pointer - 1 > Source'Last
         )  )
      then
         raise Layout_Error;
      end if;
      while High - Low /= 1 loop
         This := (Low + High) / 2;
         case Compare (Folder.List (This).Name) is
            when Less =>
               High := This;
            when Equal =>
               Found := This;
               Low := This;
            when Greater =>
               for Lower in reverse Low + 1 .. This - 1 loop
                  exit when
                     (  Found /= 0
                     and then
                        (  Folder.List (Found).Name'Length
                        >  Folder.List (Lower).Name'Length
                     )  );
                  case Compare (Folder.List (Lower).Name) is
                     when Less =>       -- Rest items could be only
                        exit;           -- lesser than this, exit
                     when Equal =>
                        Found := Lower; -- Here we are. Ignore the rest
                        exit;           -- lesser, i.e. shorter, items
                     when Greater =>
                        null;           -- Undecided, continue
                  end case;
               end loop;
               Low := This;
         end case;
      end loop;
      Offset := Found;
      if Found /= 0 then
         Pointer := Pointer + Folder.List (Found).Name'Length;
      end if;
   end Locate;

   procedure Replace
             (  Folder : in out Table;
                Name   : String;
                Data   : Tag
             )  is
      Index : constant Integer := Search (Folder, Name);
   begin
      if Index > 0 then
         Folder.List (Index).Data := Data;
      else
         Insert (Folder, -Index, Name, Data);
      end if;
   end Replace;

   procedure Replace
             (  Folder : in out Table;
                Name   : String;
                Data   : Tag;
                Offset : out Positive
             )  is
      Index : Integer := Search (Folder, Name);
   begin
      if Index > 0 then
         Folder.List (Index).Data := Data;
      else
         Index := -Index;
         Insert (Folder, Index, Name, Data);
         Offset := Index;
      end if;
   end Replace;

   procedure Replace
             (  Folder : in out Table;
                Offset : Positive;
                Data   : Tag
             )  is
   begin
      if Offset > Folder.Size then
         raise End_Error;
      else
         Folder.List (Offset).Data := Data;
      end if;
   end Replace;
end Tables;
