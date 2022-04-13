-- Ada_GUI implementation based on Gnoga. Adapted 2022
-- Implementation hierarchy rooted at Ada_GUI.Gnoga in recognition
-- Clients should never with packages in the Ada_GUI.Gnoga hierarchy
-- Original Gnoga headers have been left on the implemetation packages for reference
--
------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                                G N O G A                                 --
--                                                                          --
--                                 S p e c                                  --
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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Exceptions;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;
with Ada.Task_Identification;

package Ada_GUI.Gnoga is
   Version : constant String := "1.0";

   HTTP_Server_Name : constant String := "gnoga/" & Version;

   function Escape_Quotes (S : String) return String;
   --  Escape quotes for JavaScript.

   function Unescape_Quotes (S : String) return String;
   --  Unescape a string quoted for JavaScript

   function Escape_Inner_Quotes (S : in String) return String;
   -- Escape quotes for HTML attributes in Javascript.

   Substitution_Character : constant Character := '?';
   --  Character replacement if UTF-8 character is not existant in Latin-1

   function URL_Encode (S : String; Encoding : String := "") return String;
   function URL_Decode (S : String; Encoding : String := "") return String;
   --  Encode and decode form URL
   --  Supported encodings are ISO-8859-1 (default)
   --  and UTF-8 (typically from Input_Encoding function)

   function Left_Trim (S : String) return String;
   function Right_Trim (S : String) return String;
   --  Remove extra spaces and tabs

   function Left_Trim_Slashes (S : String) return String;
   function Right_Trim_Slashes (S : String) return String;
   --  Remove extra spaces, tabs and '/'s

   procedure String_Replace
     (Source      : in out Ada.Strings.Unbounded.Unbounded_String;
      Pattern     : in     String;
      Replacement : in     String);
   --  Replace all instances of Pattern with Replacement in Source

   procedure Write_To_Console (Message : in String);
   --  Output message to console

   procedure Log_To_File (File_Name  : in String;
                          Flush_Auto : in Boolean := False);
   --  Redirect logging to File_Name instead of console with flushing if specified

   procedure Log (Message : in String);
   --  Output message to log

   procedure Log (Occurrence : in Ada.Exceptions.Exception_Occurrence);
   --  Output exception occurence to log

   procedure Flush_Log;
   --  Manual flush log file

   procedure Activate_Exception_Handler (Id : Ada.Task_Identification.Task_Id);
   --  Activate exception log for the designated task

   -- From Types:
   package Data_Arrays is
     new Ada.Containers.Indefinite_Vectors (Positive, String);
   subtype Data_Array_Type is Data_Arrays.Vector;

   package Data_Maps is
      new Ada.Containers.Indefinite_Hashed_Maps (String,
                                                 String,
                                                 Ada.Strings.Hash,
                                                 Equivalent_Keys => "=");
   subtype Data_Map_Type is Data_Maps.Map;

   package Maps_of_Data_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps (String,
                                                Data_Maps.Map,
                                                Ada.Strings.Hash,
                                                Equivalent_Keys => "=",
                                                "=" => Data_Maps."=");
   subtype Map_of_Data_Maps_Type is Maps_of_Data_Maps.Map;

   subtype Web_ID is Ada.Strings.Unbounded.Unbounded_String;

   type ID_Enumeration is (No_ID, DOM_ID, Script, Gnoga_ID);

   subtype Connection_ID is Integer;

   No_Connection : constant Connection_ID := -1;

   subtype Unique_ID is Integer;

   No_Unique_ID : constant Unique_ID := -1;

   type Connection_Data_Type is tagged limited null record;
   type Connection_Data_Access is access all Connection_Data_Type;
   type Pointer_to_Connection_Data_Class is
     access all Connection_Data_Type'Class;

   type Frational_Range_Type is delta 0.001 range 0.0 .. 1.0;
   for Frational_Range_Type'Small use 0.001;

   subtype Alpha_Type is Frational_Range_Type;

   type Color_Type is range 0 .. 255;

   type RGBA_Type is
      record
         Red   : Color_Type := 0;
         Green : Color_Type := 0;
         Blue  : Color_Type := 0;
         Alpha : Alpha_Type := 1.0;
      end record;

   function To_String (RGBA : RGBA_Type) return String;
   --  Returns an rgba(r,g,b,a) representation of RGBA

   function To_Hex (RGBA : RGBA_Type) return String;
   --  Returns a 0xRRGGBB representation of RGBA

   function To_RGBA (Value : String) return RGBA_Type;
   --  Will convert rgb(r,g,b) and rgba(r,g,b,a), or
   --  Hex color (include Hex with Alpha) to RGBA_Type

   type Pixel_Type is
      record
         Red   : Color_Type := 0;
         Green : Color_Type := 0;
         Blue  : Color_Type := 0;
         Alpha : Color_Type := 0;
      end record;

   function To_RGBA (Value : in Pixel_Type) return RGBA_Type;

   function To_Pixel (Value : in RGBA_Type) return Pixel_Type;

   type Pixel_Data_Type is
     array (Positive range <>, Positive range <>) of Pixel_Type;
   type Pixel_Data_Access is access Pixel_Data_Type;

   type Point_Type is
      record
         X, Y : Integer;
      end record;

   type Point_Array_Type is
     array (Positive range <>) of Point_Type;

   type Rectangle_Type is
      record
         X, Y, Width, Height : Integer;
      end record;

   type Size_Type is
      record
         Width, Height : Integer;
      end record;
end Ada_GUI.Gnoga;
