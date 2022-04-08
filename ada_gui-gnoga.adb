-- Ada_GUI implementation based on Gnoga. Adapted 2021
-- String_Replace corrected 2022
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                                G N O G A                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2014 David Botton                      --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file might be   --
--  covered by the  GNU Public License.                                     --
--                                                                          --
--  For more information please go to http://www.gnoga.com                  --
------------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.UTF_Encoding.Strings;
with Ada.Task_Termination;
with Ada.Text_IO;

with Strings_Edit.UTF8.Handling;

package body Ada_GUI.Gnoga is

   Use_File : Boolean := False;
   Automatic_Flush : Boolean := False;
   Log_File : Ada.Text_IO.File_Type;

   -------------------
   -- Escape_Quotes --
   -------------------

   function Escape_Quotes (S : String) return String is

      function Translate_Character (C : Character) return String;

      function Translate_Character (C : Character) return String is
      begin
         if C = ''' then
            return "\x27";
         elsif C = '\' then
            return "\x5C";
         elsif C = Character'Val (10) then
            return "\x0A";
         elsif C = Character'Val (13) then
            return "\x0D";
         else
            return (1 => C);
         end if;
      end Translate_Character;

      R : Ada.Strings.Unbounded.Unbounded_String;
   begin
      for C in S'Range loop
         Ada.Strings.Unbounded.Append (R, Translate_Character (S (C)));
      end loop;

      return Ada.Strings.Unbounded.To_String (R);
   end Escape_Quotes;

   ---------------------
   -- Unescape_Quotes --
   ---------------------

   function Unescape_Quotes (S : String) return String is

      C : Integer := S'First;

      function Translate_Character return String;

      function Translate_Character return String is
      begin
         if C < S'Last - 1 then
            if S (C .. C + 1) = "\\" then
               C := C + 2;
               return "\";
            elsif S (C .. C + 1) = "\x" then
               declare
                  H : constant Integer := Integer'Value
                    ("16#" & S (C + 2 .. C + 3) & "#");
               begin
                  C := C + 4;

                  return (1 => Character'Val (H));
               end;
            end if;
         end if;

         declare
            R : constant String := (1 => S (C));
         begin
            C := C + 1;
            return R;
         end;
      end Translate_Character;

      R : Ada.Strings.Unbounded.Unbounded_String;
   begin
      loop
         Ada.Strings.Unbounded.Append (R, Translate_Character);
         exit when C > S'Last;
      end loop;

      return Ada.Strings.Unbounded.To_String (R);
   end Unescape_Quotes;

   function Escape_Inner_Quotes (S : in String) return String is
      R : Ada.Strings.Unbounded.Unbounded_String;
   begin -- Escape_Inner_Quotes
      All_Chars : for C of S loop
         Ada.Strings.Unbounded.Append (Source => R, New_Item => (if C = ''' then "&apos;" else C & "") );
      end loop All_Chars;

      return Ada.Strings.Unbounded.To_String (R);
   end Escape_Inner_Quotes;

   ----------------
   -- URL_Encode --
   ----------------

   function URL_Encode (S : String; Encoding : String := "") return String is

      function Translate_Character (C : Character) return String;

      function Translate_Character (C : Character) return String is
      begin
         if C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '-' | '*' | '_'
         then
            return (1 => C);
         elsif C = ' ' then
            return "+";
         else
            declare
               V : String (1 .. 6); -- 16#HH#
            begin
               Ada.Integer_Text_IO.Put (V, Character'Pos (C), 16);
               return "%" & V (4 .. 5);
            end;
         end if;
      end Translate_Character;

      R, T : Ada.Strings.Unbounded.Unbounded_String;
   begin
      if Encoding = "UTF-8" then
         T :=
           Ada.Strings.Unbounded.To_Unbounded_String
             (Ada.Strings.UTF_Encoding.Strings.Encode (S));
      else
         T := Ada.Strings.Unbounded.To_Unbounded_String (S);
      end if;

      for C in 1 .. Ada.Strings.Unbounded.Length (T) loop
         Ada.Strings.Unbounded.Append (R, Translate_Character (Ada.Strings.Unbounded.Element (T, C)));
      end loop;

      return Ada.Strings.Unbounded.To_String (R);
   end URL_Encode;

   ----------------
   -- URL_Decode --
   ----------------

   function URL_Decode (S : String; Encoding : String := "") return String is
      C : Integer := S'First;

      function Translate_Character return Character;

      function Translate_Character return Character is
         R : Character := S (C);
      begin
         if R = '+' then
            R := ' ';
         elsif R = '%' and C < S'Last - 1 then
            R := Character'Val (Integer'Value ("16#" & S (C + 1 .. C + 2)
                                & "#"));
            C := C + 2;
         end if;
         C := C + 1;
         return R;
      end Translate_Character;

      R : Ada.Strings.Unbounded.Unbounded_String;
   begin
      while C in S'Range loop
         Ada.Strings.Unbounded.Append (R, Translate_Character);
      end loop;

      if Encoding = "UTF-8" then
         return Strings_Edit.UTF8.Handling.To_String
           (Ada.Strings.Unbounded.To_String (R), Substitution_Character);
      else
         return Ada.Strings.Unbounded.To_String (R);
      end if;
   end URL_Decode;

   ---------------
   -- Left_Trim --
   ---------------

   function Left_Trim (S : String) return String is
   begin
      if S'Length = 0 then
         return S;
      end if;

      if S (S'First) = ' ' or S (S'First) = Character'Val (9) then
         return Left_Trim (S ((S'First + 1) .. S'Last));
      else
         return S;
      end if;
   end Left_Trim;

   ----------------
   -- Right_Trim --
   ----------------

   function Right_Trim (S : String) return String is
   begin
      if S'Length = 0 then
         return S;
      end if;

      if S (S'Last) = ' ' or S (S'Last) = Character'Val (9) then
         return Right_Trim (S (S'First .. (S'Last - 1)));
      else
         return S;
      end if;
   end Right_Trim;

   -----------------------
   -- Left_Trim_Slashes --
   -----------------------

   function Left_Trim_Slashes (S : String) return String is
   begin
      if S'Length = 0 then
         return S;
      end if;

      if
        S (S'First) = ' ' or
        S (S'First) = Character'Val (9) or
        S (S'First) = '/'
      then
         return Left_Trim_Slashes (S ((S'First + 1) .. S'Last));
      else
         return S;
      end if;
   end Left_Trim_Slashes;

   ------------------------
   -- Right_Trim_Slashes --
   ------------------------

   function Right_Trim_Slashes (S : String) return String is
   begin
      if S'Length = 0 then
         return S;
      end if;

      if
        S (S'Last) = ' ' or
        S (S'Last) = Character'Val (9) or
        S (S'Last) = '/'
      then
         return Right_Trim_Slashes (S (S'First .. (S'Last - 1)));
      else
         return S;
      end if;
   end Right_Trim_Slashes;

   --------------------
   -- String_Replace --
   --------------------

   procedure String_Replace
     (Source      : in out Ada.Strings.Unbounded.Unbounded_String;
      Pattern     : in     String;
      Replacement : in     String)
   is
      use Ada.Strings.Unbounded;

      I : Natural;
   begin
      if Pattern = Replacement then
         return;
      end if;

      loop
         I := Index (Source => Source, Pattern => Pattern);

         exit when I = 0;

         Replace_Slice (Source => Source,
                        Low    => I,
                        High   => I + Pattern'Length - 1,
                        By     => Replacement);
      end loop;
   end String_Replace;

   ----------------------
   -- Write_To_Console --
   ----------------------

   procedure Write_To_Console (Message : in String) is
   begin
      Ada.Text_IO.Put_Line (Message);
   end Write_To_Console;

   -----------------
   -- Log_To_File --
   -----------------

   procedure Log_To_File (File_Name : in String;
                          Flush_Auto : in Boolean := False) is
      use Ada.Text_IO;
   begin
      Create (File => Log_File,
              Mode => Append_File,
              Name => File_Name);

      Use_File := True;
      Automatic_Flush := Flush_Auto;
   exception
      when E : others =>
         Log ("Error failed to open log file " & File_Name);
         Log (E);
   end Log_To_File;

   ---------
   -- Log --
   ---------

   procedure Log (Message : in String) is
      T : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Date_Message : constant String := Ada.Calendar.Formatting.Image
        (Date                  => T,
         Include_Time_Fraction => True,
         Time_Zone             =>
           Ada.Calendar.Time_Zones.UTC_Time_Offset (T)) &
        " : " & Message;
   begin
      if Use_File then
         Ada.Text_IO.Put_Line (Log_File, Date_Message);
         if Automatic_Flush then
            Flush_Log;
         end if;
      else
         Write_To_Console (Date_Message);
      end if;
   end Log;

   procedure Log (Occurrence : in Ada.Exceptions.Exception_Occurrence) is
   begin
      Log (Ada.Exceptions.Exception_Information (Occurrence));
   end Log;

   ---------------
   -- Flush_Log --
   ---------------

   procedure Flush_Log  is
   begin
      Ada.Text_IO.Flush (Log_File);
   end Flush_Log;

   --------------------------------
   -- Activate_Exception_Handler --
   --------------------------------

   protected Exception_Handler is
      procedure Log (Cause      : in Ada.Task_Termination.Cause_Of_Termination;
                     Id         : in Ada.Task_Identification.Task_Id;
                     Occurrence : in Ada.Exceptions.Exception_Occurrence);
   end Exception_Handler;

   protected body Exception_Handler is
      procedure Log (Cause      : in Ada.Task_Termination.Cause_Of_Termination;
                     Id         : in Ada.Task_Identification.Task_Id;
                     Occurrence : in Ada.Exceptions.Exception_Occurrence) is
         use all type Ada.Task_Termination.Cause_Of_Termination;
      begin
         case Cause is
            when Normal =>
               Log ("Normal exit of task: " & Ada.Task_Identification.Image (Id));
            when Abnormal =>
               Log ("Abnormal exit of task: " & Ada.Task_Identification.Image (Id));
            when Unhandled_Exception =>
               Log ("Unhandled exception in task: " & Ada.Task_Identification.Image (Id));
               Log (Occurrence);
         end case;
      end Log;
   end Exception_Handler;

   procedure Activate_Exception_Handler  (Id : Ada.Task_Identification.Task_Id) is
   begin
      Ada.Task_Termination.Set_Specific_Handler (Id, Exception_Handler.Log'Access);
   end Activate_Exception_Handler;

   -- From Types:
   function To_RGBA_From_Hex (Value : String) return RGBA_Type;
   function To_RGBA_From_RGB_or_RGBA (Value : String) return RGBA_Type;

   ---------------
   -- To_String --
   ---------------

   function To_String (RGBA : RGBA_Type) return String is
   begin
      return "rgba(" &
        Left_Trim (RGBA.Red'Img) & "," &
        Left_Trim (RGBA.Green'Img) & "," &
        Left_Trim (RGBA.Blue'Img) & "," &
        Left_Trim (RGBA.Alpha'Img) & ")";
   end To_String;

   ------------
   -- To_Hex --
   ------------

   function To_Hex (RGBA : RGBA_Type) return String is
      Hex : constant String := "0123456789ABCDEF";
   begin
      return "0x" &
        Hex (Natural (RGBA.Red) / 16 + 1) & Hex (Natural (RGBA.Red) mod 16 + 1) &
        Hex (Natural (RGBA.Green) / 16 + 1) & Hex (Natural (RGBA.Green) mod 16 + 1) &
        Hex (Natural (RGBA.Blue) / 16 + 1) & Hex (Natural (RGBA.Blue) mod 16 + 1);
   end To_Hex;

   -------------
   -- To_RGBA --
   -------------

   function To_RGBA (Value : String) return RGBA_Type is
   begin
      if Value (Value'First) = '#' then
         return To_RGBA_From_Hex (Value);
      else
         return To_RGBA_From_RGB_or_RGBA (Value);
      end if;
   end To_RGBA;

   ----------------------
   -- To_RGBA_From_Hex --
   ----------------------

   function To_RGBA_From_Hex (Value : String) return RGBA_Type is
      RGBA : RGBA_Type;
      P    : constant Integer := Value'First;
   begin
      if Value'Length = 7 then
         RGBA.Red   := Color_Type'Value
           ("16#" & Value ((P + 1) .. (P + 2)) & '#');
         RGBA.Green := Color_Type'Value
           ("16#" & Value ((P + 3) .. (P + 4)) & '#');
         RGBA.Blue  := Color_Type'Value
           ("16#" & Value ((P + 5) .. (P + 6)) & '#');
      elsif Value'Length = 9 then
         RGBA.Alpha := Alpha_Type
           (Integer'Value ("16#" & Value ((P + 1) .. (P + 2)) & '#')) / 255;
         RGBA.Red   := Color_Type'Value
           ("16#" & Value ((P + 3) .. (P + 4)) & '#');
         RGBA.Green := Color_Type'Value
           ("16#" & Value ((P + 5) .. (P + 6)) & '#');
         RGBA.Blue  := Color_Type'Value
           ("16#" & Value ((P + 7) .. (P + 8)) & '#');
      else
         Log ("Invalid Hex value for rbga value from " & Value);
      end if;

      return RGBA;
   exception
      when E : others =>
         Log ("Error converting to rbga value from " & Value);
         Log (Ada.Exceptions.Exception_Information (E));
         return RGBA;
   end To_RGBA_From_Hex;

   ------------------------------
   -- To_RGBA_From_RGB_or_RGBA --
   ------------------------------

   function To_RGBA_From_RGB_or_RGBA (Value : String) return RGBA_Type is
      use Ada.Strings.Fixed;

      S    : Integer   := Value'First;
      F    : Integer   := Value'First - 1;
      RGBA : RGBA_Type;

      function Split (P : String) return String;
      function Split (P : String) return Color_Type;
      function Split (P : String) return Alpha_Type;
      --  Split string and extract values

      function Split (P : String) return String is
      begin
         S := F + 1;
         F := Index (Source  => Value,
                     Pattern => P,
                     From    => S);
         return Value (S .. (F - 1));
      end Split;

      function Split (P : String) return Color_Type is
      begin
         return Color_Type'Value (Split (P));
      end Split;

      function Split (P : String) return Alpha_Type is
      begin
         return Alpha_Type'Value (Split (P));
      end Split;

      rtype : constant String := Split ("(");
   begin
      RGBA.Red := Split (",");
      RGBA.Green := Split (",");

      if rtype'Length = 3 then
         RGBA.Blue := Split (")");
      else
         RGBA.Blue := Split (",");
         RGBA.Alpha := Split (")");
      end if;

      return RGBA;
   exception
      when others =>
         Log ("Error converting to rbga value from " & Value);
         return RGBA;
   end To_RGBA_From_RGB_or_RGBA;

   -------------
   -- To_RGBA --
   -------------

   function To_RGBA (Value : in Pixel_Type) return RGBA_Type is
   begin
      return (Value.Red, Value.Green, Value.Blue,
              Frational_Range_Type (Float (Value.Alpha) / 255.0));
   end To_RGBA;

   --------------
   -- To_Pixel --
   --------------

   function To_Pixel (Value : in RGBA_Type) return Pixel_Type is
   begin
      return (Value.Red, Value.Green, Value.Blue,
              Color_Type (Value.Alpha * 255.0));
   end To_Pixel;
end Ada_GUI.Gnoga;
