-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--          G N O G A . S E R V E R . T E M P L A T E _ P A R S E R         --
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

with Ada.Text_IO;
with Ada.Strings.Fixed;

with GNAT.OS_Lib;

package body Ada_GUI.Gnoga.Server.Template_Parser is

   Template_Directory : Ada.Strings.Unbounded.Unbounded_String :=
                          Ada.Strings.Unbounded.To_Unbounded_String
                            (Gnoga.Server.Templates_Directory);

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Object : in out View_Data) is
   begin
      Clear (Object);
   end Finalize;

   -------------------
   -- Variable_Name --
   -------------------

   procedure Variable_Name (Data : in out View_Data;
                            Name : String)
   is
   begin
      Data.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Variable_Name;

   ------------
   -- Insert --
   ------------

   procedure Insert (Data  : in out View_Data;
                     Key   : String;
                     Value : String)
   is
   begin
      Data.String_Values.Insert (Key, Value);
   end Insert;

   procedure Insert (Data  : in out View_Data;
                     Key   : String;
                     Value : Integer)
   is
      String_Value : constant String := Integer'Image (Value);
   begin
      Insert (Data, Key,
              String_Value (String_Value'First + 1 .. String_Value'Last));
   end Insert;

   ------------------
   -- Insert_Array --
   ------------------

   procedure Insert_Array (Data   : in out View_Data;
                           Vector : Gnoga.Data_Array_Type)
   is
   begin
      for I in Vector.First_Index .. Vector.Last_Index loop
         declare
            K : constant String := I'Img;
         begin
            Data.Insert (K (K'First + 1 .. K'Last), Vector.Element (I));
         end;
      end loop;
   end Insert_Array;

   -----------------------
   -- Insert_Array_Item --
   -----------------------

   procedure Insert_Array_Item (Data   : in out View_Data;
                                Key    : String;
                                Vector : Gnoga.Data_Array_Type)
   is
      Map : Gnoga.Data_Map_Type;
   begin
      for I in Vector.First_Index .. Vector.Last_Index loop
         declare
            K : constant String := I'Img;
         begin
            Map.Insert (K (K'First + 1 .. K'Last), Vector.Element (I));
         end;
      end loop;

      Data.Insert_Map_Item (Key, Map);
   end Insert_Array_Item;

   ----------------
   -- Insert_Map --
   ----------------

   procedure Insert_Map (Data : in out View_Data;
                         Map  : Gnoga.Data_Map_Type)
   is
      procedure foreach (Cursor : Gnoga.Data_Maps.Cursor);
      --  Iterate through required members

      procedure foreach (Cursor : Gnoga.Data_Maps.Cursor) is
      begin
         Data.Insert (Gnoga.Data_Maps.Key (Cursor),
                      Gnoga.Data_Maps.Element (Cursor));
      end foreach;
   begin
      Map.Iterate (foreach'Access);
   end Insert_Map;

   ---------------------
   -- Insert_Map_Item --
   ---------------------

   procedure Insert_Map_Item (Data : in out View_Data;
                              Key  : String;
                              Map  : Gnoga.Data_Map_Type)
   is
   begin
      Data.Map_Values.Insert (Key, Map);
   end Insert_Map_Item;

   ----------------------
   -- Insert_Recordset --
   ----------------------

   procedure Insert_Recordset
     (Data : in out View_Data;
      RS   : in out Gnoga.Server.Database.Recordset'Class)
   is
      I : Integer := 1;
   begin
      while RS.Next loop
         declare
            Key : constant String := I'Img;
         begin
            I := I + 1;
            Data.Insert_Map_Item
              (Key (Key'First + 1 .. Key'Last), RS.Field_Values);
         end;
      end loop;
   end Insert_Recordset;

   -------------------
   -- Insert_Record --
   -------------------

   procedure Insert_Record_Item (Data   : in out View_Data;
                            Key    : String;
                            Row    : Gnoga.Server.Model.Active_Record'Class)
   is
   begin
      Data.Insert_Map_Item (Key, Row.Values);
   end Insert_Record_Item;

   ----------------
   -- Inser_Rows --
   ----------------

   procedure Insert_Rows
     (Data   : in out View_Data;
      Row    : Gnoga.Server.Model.Queries.Active_Record_Array.Vector)
   is
   begin
      for I in 1 .. Natural (Row.Length) loop
         declare
            Key : constant String := I'Img;
         begin
            Data.Insert_Record_Item (Key => Key (Key'First + 1 .. Key'Last),
                                     Row => Row.Element (I));
         end;
      end loop;
   end Insert_Rows;

   -------------------
   --  Insert_Query --
   -------------------

   procedure Insert_Query (Data  : in out View_Data;
                           C     : Gnoga.Server.Database.Connection'Class;
                           Query : String)
   is
      RS : Gnoga.Server.Database.Recordset'Class := C.Query (Query);
   begin
      Insert_Recordset (Data, RS);
   end Insert_Query;

   -----------
   -- Clear --
   -----------

   procedure Clear (Data : in out View_Data) is
   begin
      Data.String_Values.Clear;
      Data.Map_Values.Clear;
   end Clear;

   ----------------
   -- Parse_Name --
   ----------------

   function Parse_Name (Name : String) return String is
      use Ada.Strings.Fixed;

      Templates_Dir : constant String :=
                        Ada.Strings.Unbounded.To_String (Template_Directory);
   begin
      if Index (Name, ":") > 0 or
        Name (Name'First) = GNAT.OS_Lib.Directory_Separator
      then
         return Name;
      end if;

      if Templates_Dir = "" then
         return Name;
      else
         return Templates_Dir & GNAT.OS_Lib.Directory_Separator & Name;
      end if;
   end Parse_Name;

   ----------------------------
   -- Set_Template_Directory --
   ----------------------------

   procedure Set_Template_Directory (Directory : String) is
   begin
      Template_Directory :=
        Ada.Strings.Unbounded.To_Unbounded_String (Directory);
   end Set_Template_Directory;

   -----------------------
   -- Add_Error_Message --
   -----------------------

   procedure Add_Error_Message (Message : String) is
   begin
      Error_Queue.Append (Message);
   end Add_Error_Message;

   -----------------------
   -- Clear_Error_Queue --
   -----------------------

   procedure Clear_Error_Queue is
   begin
      Error_Queue.Clear;
   end Clear_Error_Queue;

   ----------------------
   -- Add_Info_Message --
   ----------------------

   procedure Add_Info_Message (Message : String) is
   begin
      Info_Queue.Append (Message);
   end Add_Info_Message;

   ----------------------
   -- Clear_Info_Queue --
   ----------------------

   procedure Clear_Info_Queue is
   begin
      Info_Queue.Clear;
   end Clear_Info_Queue;

   --------------------------
   -- Write_String_To_File --
   --------------------------

   procedure Write_String_To_File (File_Name : String; Value : String) is
      use Ada.Text_IO;

      F : File_Type;
   begin
      Create (File => F,
              Mode => Out_File,
              Name => File_Name);

      Put (F, Value);

      Close (F);
   end Write_String_To_File;

end Ada_GUI.Gnoga.Server.Template_Parser;
