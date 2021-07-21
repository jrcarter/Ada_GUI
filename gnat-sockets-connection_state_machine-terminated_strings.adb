--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     Terminated_Strings                          Winter, 2012       --
--  Implementation                                                    --
--                                Last revision :  16:04 08 Jun 2019  --
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

with Ada.Exceptions;         use Ada.Exceptions;
with Ada.IO_Exceptions;      use Ada.IO_Exceptions;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

with Ada.Unchecked_Deallocation;

package body GNAT.Sockets.Connection_State_Machine.Terminated_Strings is

   procedure Free is
      new Ada.Unchecked_Deallocation (String, String_Ptr);

   procedure Feed
             (  Item    : in out String_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
      This : Character;
   begin
      if State = 0 then
         Item.Last := 0;
         State := 1;
      end if;
      while Pointer <= Data'Last loop
         This := Character'Val (Data (Pointer));
         if This = Item.Terminator then
            Pointer := Pointer + 1;
            State := 0;
            return;
         elsif Item.Last = Item.Value'Last then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Terminated string length exceeds "
               &  Image (Item.Size)
               &  " characters"
            )  );
         else
            Item.Last := Item.Last + 1;
            Item.Value (Item.Last) := This;
            Pointer := Pointer + 1;
         end if;
      end loop;
   end Feed;

   procedure Feed
             (  Item    : in out Dynamic_String_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
      This : Character;
   begin
      if Item.Value = null then
         Raise_Exception
         (  Data_Error'Identity,
            "Terminated string length exceeds 0 characters"
         );
      end if;
      if State = 0 then
         Item.Last := 0;
         State     := 1;
      end if;
      while Pointer <= Data'Last loop
         This := Character'Val (Data (Pointer));
         if This = Item.Terminator then
            Pointer := Pointer + 1;
            State := 0;
            return;
         elsif Item.Last = Item.Value'Last then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Terminated string length exceeds "
               &  Image (Integer'(Item.Value'Length))
               &  " characters"
            )  );
         else
            Item.Last := Item.Last + 1;
            Item.Value (Item.Last) := This;
            Pointer := Pointer + 1;
         end if;
      end loop;
   end Feed;

   procedure Finalize (Item : in out Dynamic_String_Data_Item) is
   begin
      Finalize (Data_Item (Item));
      Free (Item.Value);
   end Finalize;

   function Get
            (  Data       : Stream_Element_Array;
               Pointer    : access Stream_Element_Offset;
               Terminator : Character
            )  return String is
      Next : Stream_Element_Offset := Pointer.all;
   begin
      if (  Next < Data'First
         or else
            (  Next > Data'Last
            and then
               Next > Data'Last + 1
         )  )
      then
         Raise_Exception (Layout_Error'Identity, Out_Of_Bounds);
      end if;
      while Next <= Data'Last loop
         if Character'Val (Data (Next)) = Terminator then
            declare
               Result : String (1..Natural (Next - Pointer.all));
            begin
               Next := Pointer.all;
               for Index in Result'Range loop
                  Result (Index) := Character'Val (Data (Next));
                  Next := Next + 1;
               end loop;
               Pointer.all := Next + 1;
               return Result;
            end;
         else
            Next := Next + 1;
         end if;
      end loop;
      Raise_Exception
      (  End_Error'Identity,
         "Missing string terminator"
      );
   end Get;

   function Get_Maximum_Size
            (  Item : Dynamic_String_Data_Item
            )  return Natural is
   begin
      if Item.Value = null then
         return 0;
      else
         return Item.Value'Length;
      end if;
   end Get_Maximum_Size;

   function Get_Value (Item : String_Data_Item) return String is
   begin
      return Item.Value (1..Item.Last);
   end Get_Value;

   function Get_Value
            (  Item : Dynamic_String_Data_Item
            )  return String is
   begin
      if Item.Value = null then
         return "";
      else
         return Item.Value (1..Item.Last);
      end if;
   end Get_Value;

   procedure Put
             (  Data       : in out Stream_Element_Array;
                Pointer    : in out Stream_Element_Offset;
                Value      : String;
                Terminator : Character
             )  is
   begin
      if (  Pointer < Data'First
         or else
            (  Pointer > Data'Last
            and then
               Pointer - 1 > Data'Last
         )  )
      then
         Raise_Exception (Layout_Error'Identity, Out_Of_Bounds);
      elsif Data'Last - Pointer < Value'Length then
         Raise_Exception (End_Error'Identity, No_Room);
      end if;
      for Index in Value'Range loop
         if Value (Index) = Terminator then
            Raise_Exception
            (  Data_Error'Identity,
               "The string body contains the terminator"
            );
         end if;
      end loop;
      for Index in Value'Range loop
         Data (Pointer) := Character'Pos (Value (Index));
         Pointer := Pointer + 1;
      end loop;
      Data (Pointer) := Character'Pos (Terminator);
      Pointer := Pointer + 1;
   end Put;

   procedure Set_Maximum_Size
             (  Item : in out Dynamic_String_Data_Item;
                Size : Positive
             )  is
   begin
      if Item.Value = null then
         Item.Value := new String (1..Size);
         Item.Last := 0;
      elsif Item.Value'Length < Size then
         Free (Item.Value);
         Item.Value := new String (1..Size);
         Item.Last := 0;
      end if;
   end Set_Maximum_Size;

end GNAT.Sockets.Connection_State_Machine.Terminated_Strings;
