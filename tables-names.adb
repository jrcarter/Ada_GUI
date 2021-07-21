--                                                                    --
--  package Tables.Names            Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2003       --
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

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Ada.Strings.Maps;         use Ada.Strings.Maps;

package body Tables.Names is

   type Extended_Equality is (Less, Equal, Prefix, Greater);
--
-- Compare -- String with a pattern
--
--    Source  - The string
--    Pointer - The position in it
--    Item    - The pattern
--
-- Pointer is advanced if the outcome is Equal or Prefix
--
-- Returns :
--
--    Comparison result
--
   function Compare
            (  Source  : String;
               Pointer : access Integer;
               Item    : String
            )  return Extended_Equality is
      pragma Inline (Compare);
      Index1  : Integer := Pointer.all;
      Index2  : Integer := Item'First;
      Symbol1 : Character;
      Symbol2 : Character;
   begin
      loop
         if Index2 > Item'Last then
            if Index1 > Source'Last then
               Pointer.all := Index1;
               return Equal;
            else
               Pointer.all := Index1;
               return Prefix;
            end if;
         else
            if Index1 > Source'Last then
               return Less;
            end if;
         end if;
         Symbol1 := Source (Index1);
         Symbol2 := Item   (Index2);
         if Is_In (Symbol1, Blanks) then
            if Is_In (Symbol2, Blanks) then
               Index1 := Index1 + 1;
               Index2 := Index2 + 1;
               while Index1 <= Source'Last loop
                  exit when not Is_In (Source (Index1), Blanks);
                  Index1 := Index1 + 1;
               end loop;
               while Index2 <= Item'Last loop
                  exit when not Is_In (Item (Index2), Blanks);
                  Index2 := Index2 + 1;
               end loop;
            else
               return Less;
            end if;
         else
            if Is_In (Symbol2, Blanks) then
               return Greater;
            else
               Symbol1 := To_Lower (Symbol1);
               Symbol2 := To_Lower (Symbol2);
               if Symbol1 = Symbol2 then
                  Index1 := Index1 + 1;
                  Index2 := Index2 + 1;
               else
                  if Symbol1 > Symbol2 then
                     return Greater;
                  else
                     return Less;
                  end if;
               end if;
            end if;
         end if;
      end loop;
   end Compare;

   function Search
            (  Folder : Dictionary;
               Name   : String
            )  return Integer is
      Low   : Integer := 0;
      High  : Integer := Folder.Size + 1;
      This  : Integer;
      Index : aliased Integer;
   begin
      if High = 1 then
         return -1;
      end if;
      loop
         This := (Low + High) / 2;
         Index := Name'First;
         case Compare (Name, Index'Access, Folder.List (This).Name) is
            when Less =>
               High := This;
               if High - Low = 1 then
                  return -This;
               end if;
            when Equal =>
               return This;
            when Prefix | Greater =>
               Low := This;
               if High - Low = 1 then
                  return -(This + 1);
               end if;
         end case;
      end loop;
   end Search;

   procedure Add
             (  Folder : in out Dictionary;
                Name   : String;
                Data   : Tag
             )  is
   begin
      Check_Spelling (Name);
      declare
         Index : constant Integer := Search (Folder, Name);
      begin
         if Index > 0 then
            raise Name_Error;
         else
            Insert (Folder, -Index, Name, Data);
         end if;
      end;
   end Add;

   procedure Add
             (  Folder : in out Dictionary;
                Name   : String;
                Data   : Tag;
                Offset : out Positive
             )  is
   begin
      Check_Spelling (Name);
      declare
         Index : Integer := Search (Folder, Name);
      begin
         if Index > 0 then
            raise Name_Error;
         else
            Index := -Index;
            Insert (Folder, Index, Name, Data);
            Offset := Index;
         end if;
      end;
   end Add;

   procedure Delete (Folder : in out Dictionary; Name : String) is
      Index : constant Integer := Search (Folder, Name);
   begin
      if Index > 0 then
         Delete (Folder, Index);
      end if;
   end Delete;

   function Find (Folder : Dictionary; Name : String) return Tag is
      Index : constant Integer := Search (Folder, Name);
   begin
      if Index > 0 then
         return Folder.List (Index).Data;
      else
         raise End_Error;
      end if;
   end Find;

   function IsIn (Folder : Dictionary; Name : String) return Boolean is
   begin
      return Search (Folder, Name) > 0;
   end IsIn;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Folder  : Dictionary;
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
                Folder  : Dictionary;
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

   function Locate (Folder : Dictionary; Name : String)
      return Natural is
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
                Folder  : Dictionary;
                Offset  : out Natural
             )  is
      Found : Integer := 0;
      Low   : Integer := 0;
      High  : Integer := Folder.Size + 1;
      This  : Integer;
      Next  : Integer;
      Index : aliased Integer;
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
         Index := Pointer;
         case Compare (Source, Index'Access, Folder.List (This).Name) is
            when Less =>
               High := This;
            when Equal | Prefix =>
               Found := This;
               Next  := Index;
               Low   := This;
            when Greater =>
               for Lower in reverse Low + 1 .. This - 1 loop
                  exit when
                     (  Found /= 0
                     and then
                        (  Folder.List (Found).Name'Length
                        >  Folder.List (Lower).Name'Length
                     )  );
                  Index := Pointer;
                  case Compare
                       (  Source,
                          Index'Access,
                          Folder.List (Lower).Name
                       )  is
                     when Less =>       -- Rest items could be only
                        exit;           -- lesser than this, exit
                     when Equal | Prefix =>
                        Found := Lower; -- Here we are. Ignore the rest
                        Next  := Index; -- lesser, i.e. shorter, items
                        exit;
                     when Greater =>
                        null;           -- Undecided, continue
                  end case;
               end loop;
               Low := This;
         end case;
      end loop;
      if (  Found = 0
         or else
            (  Next <= Source'Last
            and then
               not Check_Matched (Source, Next)
         )  )
      then
         Offset := 0;
      else
         Offset  := Found;
         Pointer := Next;
      end if;
   end Locate;

   procedure Replace
             (  Folder : in out Dictionary;
                Name   : String;
                Data   : Tag
             )  is
   begin
      Check_Spelling (Name);
      declare
         Index : constant Integer := Search (Folder, Name);
      begin
         if Index > 0 then
            Folder.List (Index).Data := Data;
         else
            Insert (Folder, -Index, Name, Data);
         end if;
      end;
   end Replace;

   procedure Replace
             (  Folder : in out Dictionary;
                Name   : String;
                Data   : Tag;
                Offset : out Positive
             )  is
   begin
      Check_Spelling (Name);
      declare
         Index : Integer := Search (Folder, Name);
      begin
         if Index > 0 then
            Folder.List (Index).Data := Data;
         else
            Index := -Index;
            Insert (Folder, Index, Name, Data);
            Offset := Index;
         end if;
      end;
   end Replace;

end Tables.Names;
