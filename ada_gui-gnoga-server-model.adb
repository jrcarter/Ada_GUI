-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                    G N O G A . S E R V E R . M O D E L                   --
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

with Ada.Calendar.Formatting;
with Ada.Exceptions;

package body Ada_GUI.Gnoga.Server.Model is

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize (Object : in out Active_Record) is
      use Ada_GUI.Gnoga.Server.Database;
   begin
      if Object.Connection = null then
         raise Connection_Error;
      end if;

      Object.Fields :=
        Object.Connection.List_Fields_Of_Table
          (Object.Table_Name.all);
   end Initialize;

   ----------------
   -- Values --
   ----------------

   procedure Values
     (A                   : in out Active_Record;
      Map                 : in     Gnoga.Data_Maps.Map)
   is
      procedure foreach (Position : in Gnoga.Data_Arrays.Cursor);

      procedure foreach (Position : in Gnoga.Data_Arrays.Cursor) is
      begin
         if Map.Contains (Gnoga.Data_Arrays.Element (Position)) then
            A.Value
              (Gnoga.Data_Arrays.Element (Position),
               Map.Element (Gnoga.Data_Arrays.Element (Position)));
         end if;
      end foreach;
   begin
      A.Fields.Iterate (foreach'Access);
   end Values;

   -----------
   -- Value --
   -----------

   procedure Value (A          : in out Active_Record;
                    Field_Name : in     String;
                    Value      : in     String)
   is
   begin
      A.Values.Include (Field_Name, Value);
   end Value;

   procedure Value (A             : in out Active_Record;
                    Field_Name    : in     String;
                    Integer_Value : in     Integer)
   is
   begin
      Value (A, Field_Name, Gnoga.Left_Trim (Integer_Value'Img));
   end Value;

   procedure Value (A          : in out Active_Record;
                    Field_Name : in     String;
                    Date_Value : in     Ada.Calendar.Time)
   is
      V : constant String := Ada.Calendar.Formatting.Image (Date_Value);
   begin
      Value (A, Field_Name, V);
   end Value;

   -----------------
   -- Field_Names --
   -----------------

   function Field_Names (A : Active_Record)
                         return Gnoga.Data_Array_Type
   is
   begin
      return A.Fields;
   end Field_Names;

   ------------
   -- Values --
   ------------

   function Values (A : Active_Record) return Gnoga.Data_Maps.Map is
   begin
      return A.Values;
   end Values;

   -----------
   -- Value --
   -----------

   function Value (A : Active_Record; Field_Name : String) return String is
   begin
      return A.Values.Element (Field_Name);
   end Value;

   ------------
   -- Exists --
   ------------

   function Exists (A : Active_Record; Field_Name : String) return Boolean is
   begin
      return A.Values.Contains (Field_Name);
   end Exists;

   ----------
   -- Save --
   ----------

   procedure Save (A : in out Active_Record) is
      use Ada.Strings.Unbounded;

      fields : Unbounded_String;
      values : Unbounded_String;

      procedure foreach (Position : in Gnoga.Data_Maps.Cursor);

      procedure foreach (Position : in Gnoga.Data_Maps.Cursor) is
      begin
         if (Gnoga.Data_Maps.Key (Position) /= "id") then
            if (A.Is_New) then
               fields := fields & "`" &
                 Gnoga.Data_Maps.Key (Position) & "` ,";
               values := values & "'" &
               A.Connection.Escape_String
                 (Gnoga.Data_Maps.Element (Position)) &
               "',";
            else
               fields := fields & "`" &
               Gnoga.Data_Maps.Key (Position) & "`=" &
               "'" &
               A.Connection.Escape_String
                 (Gnoga.Data_Maps.Element (Position)) &
               "',";
            end if;
         end if;
      end foreach;

   begin
      A.Values.Iterate (foreach'Access);

      if A.Is_New then
         declare
            f : constant String := To_String (fields);
            v : constant String := To_String (values);

            Insert_String : constant String := "insert into " &
            A.Table_Name.all &
            " (" & f (f'First .. f'Last - 1) & ") VALUES (" &
            v (v'First .. v'Last - 1) & ")";
         begin
            A.Connection.Execute_Query (Insert_String);
            declare
               New_ID : constant String := A.Connection.Insert_ID'Img;
            begin
               A.Value ("id", New_ID (New_ID'First + 1 .. New_ID'Last));
               A.Is_New := False;
            end;
         end;
      else
         declare
            f : constant String := To_String (fields);

            Update_String : constant String := "update " & A.Table_Name.all &
            " set " & f (f'First .. f'Last - 1) &
            " where id=" & A.Values.Element ("id");
         begin
            A.Connection.Execute_Query (Update_String);
         end;
      end if;
   end Save;

   ------------
   -- Delete --
   ------------

   procedure Delete (A : in out Active_Record) is

      SQL : constant String := "delete from " & A.Table_Name.all &
         " where id=" & A.Value ("id");
   begin
      A.Connection.Execute_Query (SQL);
      A.Clear;
   end Delete;

   -----------
   -- Clear --
   -----------

   procedure Clear (A : in out Active_Record) is
   begin
      A.Is_New := True;
      A.Values.Clear;
   end Clear;

   ----------
   -- Find --
   ----------

   procedure Find (A : in out Active_Record; ID : in Positive) is

      Key : constant String := ID'Img;
      RS  : Gnoga.Server.Database.Recordset'Class :=
        A.Connection.Query ("select * from " & A.Table_Name.all &
                            " where id=" & Key (Key'First + 1 .. Key'Last));
   begin
      RS.Next;
      A.Is_New := False; -- If no exception is raised then this is not new
      A.Values := RS.Field_Values;
      RS.Close;
   end Find;

   procedure Find (A : in out Active_Record; ID : in String) is
   begin
      Find (A, Positive'Value (ID));
   end Find;

   ----------------
   -- Find_Where --
   ----------------

   procedure Find_Where (A          : in out Active_Record;
                         Where      : in     String;
                         Create_New : in     Boolean := True)
   is

      RS : Gnoga.Server.Database.Recordset'Class :=
        A.Connection.Query ("select * from " & A.Table_Name.all &
                            " where " & Where & " LIMIT 1");
   begin
      RS.Next;
      A.Is_New := False;  -- If no exception is raised then this is not new
      A.Values := RS.Field_Values;
      RS.Close;
   exception
      when E : Gnoga.Server.Database.End_Of_Recordset =>
         Log ("Error End_Of_Recordset.");
         Log (Ada.Exceptions.Exception_Information (E));
         if Create_New then
            A.Value ("id", "");
            RS.Close;
         else
            raise Gnoga.Server.Database.End_Of_Recordset;
         end if;
   end Find_Where;

   ---------------
   -- Find_Item --
   ---------------

   procedure Find_Item (A          : in out Active_Record;
                        Parent     : in     Active_Record'Class;
                        Create_New : in     Boolean := True)
   is
      Remove_s : constant String := Parent.Table_Name.all;

      Where_Clause : constant String :=
        Remove_s (Remove_s'First .. Remove_s'Last - 1)
        & "_id = " & Parent.Value ("id");
   begin
      A.Find_Where (Where_Clause, Create_New);
   end Find_Item;

end Ada_GUI.Gnoga.Server.Model;
