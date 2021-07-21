--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     Big_Endian.Unsigneds                        Winter, 2012       --
--  Implementation                                                    --
--                                Last revision :  14:07 11 Nov 2019  --
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

with Ada.Exceptions;     use Ada.Exceptions;
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

package body GNAT.Sockets.Connection_State_Machine.Big_Endian.
             Unsigneds is

   procedure Feed
             (  Item    : in out Unsigned_8_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State > 1 then
         Raise_Exception
         (  Data_Error'Identity,
            "Protocol error reading 8-bit big endian unsigned"
         );
      elsif State = 0 then
         Item.Value := 0;
         State      := 1;
      end if;
      if Pointer <= Data'Last then
         Item.Value := Unsigned_8 (Data (Pointer));
         Pointer := Pointer + 1;
         State := 0;
      end if;
   end Feed;

   procedure Feed
             (  Item    : in out Unsigned_16_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State > 2 then
         Raise_Exception
         (  Data_Error'Identity,
            "Protocol error reading 16-bit big endian unsigned"
         );
      elsif State = 0 then
         Item.Value := 0;
         State      := 2;
      end if;
      while Pointer <= Data'Last and then State > 0 loop
         State := State - 1;
         Item.Value :=
            (  Item.Value
            or Shift_Left
               (  Unsigned_16 (Data (Pointer)),
                  8 * Natural (State)
            )  );
         Pointer := Pointer + 1;
      end loop;
   end Feed;

   procedure Feed
             (  Item    : in out Unsigned_32_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State > 4 then
         Raise_Exception
         (  Data_Error'Identity,
            "Protocol error reading 32-bit big endian unsigned"
         );
      elsif State = 0 then
         Item.Value := 0;
         State      := 4;
      end if;
      while Pointer <= Data'Last and then State > 0 loop
         State := State - 1;
         Item.Value :=
            (  Item.Value
            or Shift_Left
               (  Unsigned_32 (Data (Pointer)),
                  8 * Natural (State)
            )  );
         Pointer := Pointer + 1;
      end loop;
   end Feed;

   procedure Feed
             (  Item    : in out Unsigned_64_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             )  is
   begin
      if State > 8 then
         Raise_Exception
         (  Data_Error'Identity,
            "Protocol error reading 64-bit big endian unsigned"
         );
      elsif State = 0 then
         Item.Value := 0;
         State      := 8;
      end if;
      while Pointer <= Data'Last and then State > 0 loop
         State := State - 1;
         Item.Value :=
            (  Item.Value
            or Shift_Left
               (  Unsigned_64 (Data (Pointer)),
                  8 * Natural (State)
            )  );
         Pointer := Pointer + 1;
      end loop;
   end Feed;

   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : out Unsigned_8
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
      end if;
      Value   := Unsigned_8 (Data (Pointer));
      Pointer := Pointer + 1;
   end Get;

   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : out Unsigned_16
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
      elsif Pointer + 1 > Data'Last then
         Raise_Exception (End_Error'Identity, "End of data");
      end if;
      Value :=
         (  Shift_Left (Unsigned_16 (Data (Pointer)), 8)
         or             Unsigned_16 (Data (Pointer + 1))
         );
      Pointer := Pointer + 2;
   end Get;

   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : out Unsigned_32
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
      elsif Pointer + 3 > Data'Last then
         Raise_Exception (End_Error'Identity, "End of data");
      end if;
      Value :=
         (  Shift_Left (Unsigned_32 (Data (Pointer    )), 24)
         or Shift_Left (Unsigned_32 (Data (Pointer + 1)), 16)
         or Shift_Left (Unsigned_32 (Data (Pointer + 2)), 8)
         or             Unsigned_32 (Data (Pointer + 3))
         );
      Pointer := Pointer + 4;
   end Get;

   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : out Unsigned_64
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
      elsif Pointer + 7 > Data'Last then
         Raise_Exception (End_Error'Identity, "End of data");
      end if;
      Value :=
         (  Shift_Left (Unsigned_64 (Data (Pointer)),     56)
         or Shift_Left (Unsigned_64 (Data (Pointer + 1)), 48)
         or Shift_Left (Unsigned_64 (Data (Pointer + 2)), 40)
         or Shift_Left (Unsigned_64 (Data (Pointer + 3)), 32)
         or Shift_Left (Unsigned_64 (Data (Pointer + 4)), 24)
         or Shift_Left (Unsigned_64 (Data (Pointer + 5)), 16)
         or Shift_Left (Unsigned_64 (Data (Pointer + 6)), 8)
         or             Unsigned_64 (Data (Pointer + 7))
         );
      Pointer := Pointer + 8;
   end Get;

   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Unsigned_8
             )  is
   begin
      if Pointer < Data'First or else Data'Last - Pointer < 0 then
         if Pointer >= Data'First and then Pointer - 1 <= Data'Last then
            Raise_Exception (End_Error'Identity, No_Room);
         else
            Raise_Exception (Layout_Error'Identity, Out_Of_Bounds);
         end if;
      end if;
      Data (Pointer) := Stream_Element (Value);
      Pointer := Pointer + 1;
   end Put;

   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Unsigned_16
             )  is
   begin
      if Pointer < Data'First or else Data'Last - Pointer < 1 then
         if Pointer >= Data'First and then Pointer - 1 <= Data'Last then
            Raise_Exception (End_Error'Identity, No_Room);
         else
            Raise_Exception (Layout_Error'Identity, Out_Of_Bounds);
         end if;
      end if;
      Data (Pointer)     := Stream_Element (Shift_Right (Value, 8));
      Data (Pointer + 1) := Stream_Element (Value and 16#FF#);
      Pointer := Pointer + 2;
   end Put;

   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Unsigned_32
             )  is
      Item : Unsigned_32 := Value;
   begin
      if Pointer < Data'First or else Data'Last - Pointer < 3 then
         if Pointer >= Data'First and then Pointer - 1 <= Data'Last then
            Raise_Exception (End_Error'Identity, No_Room);
         else
            Raise_Exception (Layout_Error'Identity, Out_Of_Bounds);
         end if;
      end if;
      for Byte in reverse Stream_Element_Offset range 0..3 loop
         Data (Pointer + Byte) := Stream_Element (Item and 16#FF#);
         Item := Shift_Right (Item, 8);
      end loop;
      Pointer := Pointer + 4;
   end Put;

   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Unsigned_64
             )  is
      Item : Unsigned_64 := Value;
   begin
      if Pointer < Data'First or else Data'Last - Pointer < 7 then
         if Pointer >= Data'First and then Pointer - 1 <= Data'Last then
            Raise_Exception (End_Error'Identity, No_Room);
         else
            Raise_Exception (Layout_Error'Identity, Out_Of_Bounds);
         end if;
      end if;
      for Byte in reverse Stream_Element_Offset range 0..7 loop
         Data (Pointer + Byte) := Stream_Element (Item and 16#FF#);
         Item := Shift_Right (Item, 8);
      end loop;
      Pointer := Pointer + 8;
   end Put;

end GNAT.Sockets.Connection_State_Machine.Big_Endian.Unsigneds;
