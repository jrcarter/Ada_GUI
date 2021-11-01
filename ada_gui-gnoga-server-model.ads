-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                    G N O G A . S E R V E R . M O D E L                   --
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

--  An Active Record implementation for Ada similar to that used in Rails

with Ada.Finalization;
with Ada.Calendar;

with Ada_GUI.Gnoga.Server.Database;

package Ada_GUI.Gnoga.Server.Model is
   type Active_Record
     (Table_Name : access constant String;
      Connection : access          Gnoga.Server.Database.Connection'Class)
     is new Ada.Finalization.Controlled with private;

   overriding procedure Initialize (Object : in out Active_Record);

   function Field_Names (A : Active_Record)
                         return Gnoga.Data_Array_Type;
   --  Returns the Field Names supported by this Active_Record

   procedure Values (A   : in out Active_Record;
                     Map : in     Gnoga.Data_Map_Type);
   --  Set table values from Map ignoring any extra key/value pairs in Map
   --  not found in record schema

   function Values (A : Active_Record) return Gnoga.Data_Map_Type;
   --  Return all values in record

   procedure Value (A          : in out Active_Record;
                    Field_Name : in     String;
                    Value      : in     String);
   --  Set value for Field_Name

   procedure Value (A             : in out Active_Record;
                    Field_Name    : in     String;
                    Integer_Value : in     Integer);
   --  Set value for Field_Name

   procedure Value (A          : in out Active_Record;
                    Field_Name : in     String;
                    Date_Value : in     Ada.Calendar.Time);
   --  Set value for Field_Name

   function Value (A : Active_Record; Field_Name : String) return String;
   --  Return value for field_name

   function Exists (A : Active_Record; Field_Name : String) return Boolean;
   --  Checks for the presence of Field_Name in record

   procedure Save (A : in out Active_Record);
   --  Save values to row, if not the result of a find create new record

   procedure Delete (A : in out Active_Record);
   --  Delete this row

   procedure Clear (A : in out Active_Record);
   --  Clears the Active Record and mark as a new row

   procedure Find (A : in out Active_Record; ID : in Positive);
   --  Load row with ID

   procedure Find (A : in out Active_Record; ID : in String);
   --  Load row with ID, this version of Find accepts the ID as a string
   --  ID must be a valid integer

   procedure Find_Where (A          : in out Active_Record;
                         Where      : in     String;
                         Create_New : in     Boolean := True);
   --  Load row based on where clause, note that only the first record
   --  returned will be loaded. Use Model.Queries.Find_All for
   --  return of multiple records. If Create_New is true then no exception
   --  is thrown if the record is not found. Checking if Value ("id")
   --  is empty, i.e. is a new record would confirm no results found.

   procedure Find_Item (A          : in out Active_Record;
                        Parent     : in     Active_Record'Class;
                        Create_New : in     Boolean := True);
   --  Return first matching record in Child Table A where:
   --  Child_Table.PARENT_TABLE_NAME(with out s)_id = Child_Table.id
private

   type Active_Record
     (Table_Name : access constant String;
      Connection : access          Gnoga.Server.Database.Connection'Class)
     is new Ada.Finalization.Controlled with
      record
         Is_New     : Boolean                               := True;
         Fields     : Gnoga.Data_Array_Type;
         Values     : Gnoga.Data_Map_Type;
      end record;
end Ada_GUI.Gnoga.Server.Model;
