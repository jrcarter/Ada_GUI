--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.Base64                         Luebeck            --
--  Implementation                                 Autumn, 2014       --
--                                                                    --
--                                Last revision :  18:40 01 Aug 2019  --
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

package body Strings_Edit.Base64 is

   Stream_Is_Full      : constant String := "Stream buffer is full";
   Invalid_Character   : constant String := "Non-Base64 character '";
   Unexpected_Equality : constant String := "Unexpected character '='";

   type Base64_Encoding is
      array (Unsigned_16 range 0..63) of Character;
   Base64 : constant Base64_Encoding := "ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
                                        "abcdefghijklmnopqrstuvwxyz" &
                                        "0123456789+/";

   procedure Not_Available
             (  Stream : Base64_Stream'Class;
                Length : Stream_Element_Count
             )  is
   begin
      Raise_Exception
      (  Use_Error'Identity,
         (  "Not enough space to write"
         &  Stream_Element_Offset'Image (Length)
         &  " elements (only"
         &  Stream_Element_Offset'Image (Free (Stream))
         &  " available)"
      )  );
   end Not_Available;

   procedure Too_Large
             (  Stream : Base64_Stream'Class;
                Length : Stream_Element_Count
             )  is
   begin
      Raise_Exception
      (  Use_Error'Identity,
         (  "Writing"
         &  Stream_Element_Offset'Image (Length)
         &  " elements into stream capable of only"
         &  Stream_Element_Offset'Image (Size (Stream))
      )  );
   end Too_Large;

   procedure Flush (Stream : in out Base64_Encoder) is
      Accum : Unsigned_16 renames Stream.Accum;
      Bits  : Natural     renames Stream.Bits;
   begin
      case Bits is
         when 2 =>
            if Free (Stream) < 3 then
               if Size (Stream) < 3 then
                  Too_Large (Stream, 3);
               else
                  Not_Available (Stream, 3);
               end if;
            end if;
            Store (Stream, Character'Pos (Base64 (Accum * 2**4)));
            Store (Stream, Character'Pos ('='));
            Store (Stream, Character'Pos ('='));
         when 4 =>
            if Free (Stream) < 1 then
               Not_Available (Stream, 1);
            end if;
            Store (Stream, Character'Pos (Base64 (Accum * 2**2)));
            Store (Stream, Character'Pos ('='));
         when others =>
            null;
      end case;
   end Flush;

   function From_Base64 (Text : String) return String is
      Result  : String (1..(Text'Length * 3 + 3) / 4);
      This    : Unsigned_16;
      Accum   : Unsigned_16 := 0;
      Bits    : Natural     := 0;
      Pointer : Integer     := 1;
   begin
      for Index in Text'Range loop
         This := Character'Pos (Text (Index));
         case This is
            when Character'Pos ('A')..Character'Pos ('Z') =>
               Accum := Accum * 64 + This - Character'Pos ('A');
            when Character'Pos ('a')..Character'Pos ('z') =>
               Accum := Accum * 64 + This - Character'Pos ('a') + 26;
            when Character'Pos ('0')..Character'Pos ('9') =>
               Accum := Accum * 64 + This - Character'Pos ('0') + 52;
            when Character'Pos ('+') =>
               Accum := Accum * 64 + 62;
            when Character'Pos ('/') =>
               Accum := Accum * 64 + 63;
            when Character'Pos ('=') =>
               if Index = Text'Last then -- 2 bytes accumulated
                  exit when Bits = 2;
               elsif Index + 1 = Text'Last then -- 1 byte accumulated
                  exit when Text (Index + 1) = '=' and then Bits = 4;
               end if;
               Raise_Exception
               (  Data_Error'Identity,
                  Unexpected_Equality
               );
            when others =>
               Raise_Exception
               (  Data_Error'Identity,
                  Invalid_Character & Text (Index) & '''
               );
         end case;
         Bits := Bits + 6;
         if Bits >= 8 then
            Bits := Bits - 8;
            This := Accum / 2**Bits;
            Result (Pointer) :=
               Character'Val ((Accum / 2**Bits) and 16#FF#);
            Pointer := Pointer + 1;
            Accum   := Accum mod 2**Bits;
         end if;
      end loop;
      return Result (1..Pointer - 1);
   end From_Base64;

   function Free (Stream : Base64_Decoder)
      return Stream_Element_Count is
   begin
      return ((Stream.Size - 1 - Used (Stream)) * 4) / 3;
   end Free;

   function Free (Stream : Base64_Encoder)
      return Stream_Element_Count is
   begin
      return ((Stream.Size - 1 - Used (Stream)) * 3) / 4;
   end Free;

   function Is_Empty (Stream : Base64_Decoder) return Boolean is
   begin
      return Stream.Item_In = Stream.Item_Out;
   end Is_Empty;

   function Is_Empty (Stream : Base64_Encoder) return Boolean is
   begin
      return Stream.Item_In = Stream.Item_Out;
   end Is_Empty;

   function Is_Full (Stream : Base64_Decoder) return Boolean is
   begin
      return Free (Stream) = 0;
   end Is_Full;

   function Is_Full (Stream : Base64_Encoder) return Boolean is
   begin
      return Free (Stream) = 0;
   end Is_Full;

   procedure Read
             (  Stream : in out Base64_Stream;
                Data   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             )  is
      Item_In  : Stream_Element_Count renames Stream.Item_In;
      Item_Out : Stream_Element_Count renames Stream.Item_Out;
      FIFO     : Stream_Element_Array renames Stream.FIFO;
   begin
      Last := Data'First - 1;
      while Item_In /= Item_Out and then Last < Data'Last loop
         Last := Last + 1;
         Data (Last) := FIFO (Item_Out);
         if Item_Out = FIFO'Last then
            Item_Out := 1;
         else
            Item_Out := Item_Out + 1;
         end if;
      end loop;
   end Read;

   procedure Reset (Stream : in out Base64_Decoder) is
   begin
      Stream.Item_In  := 1;
      Stream.Item_Out := 1;
      Stream.Accum    := 0;
      Stream.Bits     := 0;
      Stream.Equality := False;
   end Reset;

   procedure Reset (Stream : in out Base64_Encoder) is
   begin
      Stream.Item_In  := 1;
      Stream.Item_Out := 1;
      Stream.Accum    := 0;
      Stream.Bits     := 0;
   end Reset;

   function Size (Stream : Base64_Decoder)
      return Stream_Element_Count is
   begin
      return ((Stream.Size - 1) * 4) / 3;
   end Size;

   function Size (Stream : Base64_Encoder)
      return Stream_Element_Count is
   begin
      return ((Stream.Size - 1) * 3) / 4;
   end Size;

   procedure Store
             (  Stream : in out Base64_Stream;
                Item   : Stream_Element
             )  is
   begin
      if Stream.Item_In = Stream.FIFO'Last then
         if Stream.Item_Out = 1 then
            Raise_Exception (Status_Error'Identity, Stream_Is_Full);
         end if;
         Stream.FIFO (Stream.FIFO'Last) := Item;
         Stream.Item_In := 1;
      else
         if Stream.Item_In + 1 = Stream.Item_Out then
            Raise_Exception (Status_Error'Identity, Stream_Is_Full);
         end if;
         Stream.FIFO (Stream.Item_In) := Item;
         Stream.Item_In := Stream.Item_In + 1;
      end if;
   end Store;

   procedure Write
             (  Stream : in out Base64_Decoder;
                Data   : Stream_Element_Array
             )  is
      Accum : Unsigned_16 renames Stream.Accum;
      Bits  : Natural     renames Stream.Bits;
      This  : Unsigned_16;
   begin
      if Stream.Equality then -- Equality seen
         if Data'Length = 0 then
            return;
         elsif Data (Data'First) = Character'Pos ('=') then
            if Data'Length = 1 and then Bits = 4 then
               Bits := 0;
               return;
            else
               Raise_Exception
               (  Data_Error'Identity,
                  Unexpected_Equality
               );
            end if;
         else
            Raise_Exception
            (  Data_Error'Identity,
               (  Invalid_Character
               &  Character'Val (Data (Data'First))
               &  '''
            )  );
         end if;
      elsif Free (Stream) < Data'Length then
         if Size (Stream) < Data'Length then
            Too_Large (Stream, Data'Length);
         else
            Not_Available (Stream, Data'Length);
         end if;
      end if;
      for Index in Data'Range loop
         This := Unsigned_16 (Data (Index));
         case This is
            when Character'Pos ('A')..Character'Pos ('Z') =>
               Accum := Accum * 64 + This - Character'Pos ('A');
            when Character'Pos ('a')..Character'Pos ('z') =>
               Accum := Accum * 64 + This - Character'Pos ('a') + 26;
            when Character'Pos ('0')..Character'Pos ('9') =>
               Accum := Accum * 64 + This - Character'Pos ('0') + 52;
            when Character'Pos ('+') =>
               Accum := Accum * 64 + 62;
            when Character'Pos ('/') =>
               Accum := Accum * 64 + 63;
            when Character'Pos ('=') =>
               Stream.Equality := True;
               if Index = Data'Last then -- 2 bytes accumulated
                  exit when Bits = 2 or else Bits = 4;
               elsif Index + 1 = Data'Last then -- 1 byte accumulated
                  exit when Data (Index + 1) = Character'Pos ('=')
                   and then Bits = 4;
               end if;
               Raise_Exception
               (  Data_Error'Identity,
                  Unexpected_Equality
               );
            when others =>
               Raise_Exception
               (  Data_Error'Identity,
                  Invalid_Character & Character'Val (Data (Index)) & '''
               );
         end case;
         Bits := Bits + 6;
         if Bits >= 8 then
            Bits := Bits - 8;
            This := Accum / 2**Bits;
            Store
            (  Stream,
               Stream_Element ((Accum / 2**Bits) and 16#FF#)
            );
            Accum := Accum mod 2**Bits;
         end if;
      end loop;
   end Write;

   procedure Write
             (  Stream : in out Base64_Encoder;
                Data   : Stream_Element_Array
             )  is
      Accum : Unsigned_16 renames Stream.Accum;
      Bits  : Natural     renames Stream.Bits;
   begin
      if Free (Stream) < Data'Length then
         if Size (Stream) < Data'Length then
            Too_Large (Stream, Data'Length);
         else
            Not_Available (Stream, Data'Length);
         end if;
      end if;
      for Index in Data'Range loop
         Accum := Accum * 256 + Unsigned_16 (Data (Index));
         Bits  := Bits + 8;
         if Bits >= 12 then
            Bits := Bits - 6;
            Store
            (  Stream,
               Character'Pos (Base64 ((Accum / 2**Bits) and 2#11_1111#))
            );
            Accum := Accum mod 2**Bits;
         end if;
         if Bits >= 6 then
            Bits := Bits - 6;
            Store
            (  Stream,
               Character'Pos (Base64 ((Accum / 2**Bits) and 2#11_1111#))
            );
            Accum := Accum mod 2**Bits;
         end if;
      end loop;
   end Write;

   function To_Base64 (Text : String) return String is
      Result  : String (1..(Text'Length + 3 / 3) * 4);
      Pointer : Integer     := 1;
      Bits    : Natural     := 0;
      Accum   : Unsigned_16 := 0;
   begin
      for Index in Text'Range loop
         Accum := Accum * 256 + Character'Pos (Text (Index));
         Bits  := Bits + 8;
         if Bits >= 12 then
            Bits := Bits - 6;
            Result (Pointer) :=
               Base64 ((Accum / 2**Bits) and 2#11_1111#);
            Accum   := Accum mod 2**Bits;
            Pointer := Pointer + 1;
         end if;
         if Bits >= 6 then
            Bits := Bits - 6;
            Result (Pointer) :=
               Base64 ((Accum / 2**Bits) and 2#11_1111#);
            Accum   := Accum mod 2**Bits;
            Pointer := Pointer + 1;
         end if;
      end loop;
      case Bits is
         when 2 =>
            Result (Pointer) := Base64 (Accum * 2**4);
            Pointer := Pointer + 1;
            Result (Pointer) := '=';
            Pointer := Pointer + 1;
            Result (Pointer) := '=';
            Pointer := Pointer + 1;
         when 4 =>
            Result (Pointer) := Base64 (Accum * 2**2);
            Pointer := Pointer + 1;
            Result (Pointer) := '=';
            Pointer := Pointer + 1;
         when others =>
            null;
      end case;
      return Result (1..Pointer - 1);
   end To_Base64;

   function Used (Stream : Base64_Decoder)
      return Stream_Element_Count is
      Diff : constant Stream_Element_Offset :=
                      Stream.Item_In - Stream.Item_Out;
   begin
      if Diff < 0 then
         return Stream.Size + Diff;
      else
         return Diff;
      end if;
   end Used;

   function Used (Stream : Base64_Encoder)
      return Stream_Element_Count is
      Diff : constant Stream_Element_Offset :=
                      Stream.Item_In - Stream.Item_Out;
   begin
      if Diff < 0 then
         return Stream.Size + Diff;
      else
         return Diff;
      end if;
   end Used;

end Strings_Edit.Base64;
