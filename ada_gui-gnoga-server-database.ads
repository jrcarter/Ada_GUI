-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                  G N O G A . S E R V E R . D A T A B A S E               --
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

--  Abstract class for database access. Use one of the specific implementations
--  for MySQL, SQLLite, etc.

with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Vectors;

package Ada_GUI.Gnoga.Server.Database is

   type Connection is limited interface;
   type Connection_Access is access all Connection'Class;

   procedure Disconnect (C : in out Connection) is abstract;
   --  Disconnect from server

   procedure Execute_Query (C : in out Connection; SQL : String) is abstract;
   --  Execute an SQL Query with no result set

   function Execute_Update (C : in out Connection; SQL : String)
                            return Natural is abstract;
   --  Executes and SQL Query and returns the number of affected rows

   function Affected_Rows (C : Connection) return Natural is abstract;
   --  Returns the number of rows affected by an Execute_Query

   function Insert_ID (C : Connection) return Natural is abstract;
   --  Returns the last value assigned to an auto increment field upon insert

   function Error_Message (C : Connection) return String is abstract;
   --  Returns the last error message that has occurred on this connection

   function List_Of_Tables (C : Connection)
                            return Gnoga.Data_Array_Type is abstract;
   --  Return an array of table names

   function List_Fields_Of_Table
     (C          : Connection;
      Table_Name : String)
      return Gnoga.Data_Array_Type is abstract;
   --  Return an array of field names for table

   type Field_Description is record
      Column_Name   : Ada.Strings.Unbounded.Unbounded_String;
      Data_Type     : Ada.Strings.Unbounded.Unbounded_String;
      Can_Be_Null   : Boolean;
      Default_Value : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Field_Description_Arrays is
     new Ada.Containers.Indefinite_Vectors (Natural, Field_Description);
   subtype Field_Description_Array_Type is Field_Description_Arrays.Vector;

   function Field_Descriptions
     (C : Connection; Table_Name : String)
     return Field_Description_Array_Type is abstract;
   --  Return an array of Field_Description records describe the fields of
   --  a table

   function Field_Type (Field : Field_Description) return String;
   --  Returns the field type portion of a data type, for example:
   --  If the Field.Data_Type = Varchar(80) then will return Varchar

   function Field_Size (Field : Field_Description) return Natural;
   --  Returns the field size portion of a data type, for example:
   --  If the Field.Data_Type = varchar(80) then will return 80
   --  If the Data_Type does not have a size portion will return 0
   --  If the Data_Type is a numeric with decimals, e.g. decimal(10,2)
   --  then it will return the non-decimal portion.

   function Field_Decimals (Field : Field_Description) return Natural;
   --  Returns the decimal portion of a field size if it exists or 0
   --  for example: if the Data_Type = float(10,2) it will return 2

   function Field_Options (Field : Field_Description) return String;
   --  Returns the field options portion of a data type, for example:
   --  If the Field.Data_Type = enum('N','Y') then will return 'N','Y'
   --  as this is described in the database in the same way as field
   --  size, this may be used for a string representation of the size
   --  as well. For example varchar(80) will return the string 80
   --  This is also used for descriptions like decimal(10,2), etc.

   function ID_Field_String (C : Connection) return String is abstract;
   --  Returns the propper type format for the ID field that should be part
   --  of every table used by GRAW.
   --  e.g. for SQLlite = "id INTEGER PRIMARY KEY AUTOINCREMENT"
   --       for MySQL   = "id INTEGER PRIMARY KEY AUTO_INCREMENT"

   type Recordset is interface;
   type Recordset_Access is access all Recordset'Class;

   function Query (C : Connection; SQL : String)
                   return Recordset'Class
                   is abstract;
   --  Execute query that returns Recordset

   procedure Close (RS : in out Recordset) is abstract;
   --  Close current recordset and free resources

   procedure Next (RS : in out Recordset) is abstract;
   --  Go to next row

   function Next (RS : in out Recordset) return Boolean is abstract;
   --  Go to next row and return true if not End of Recordset

   procedure Iterate
     (C       : in out Connection;
      SQL     : in String;
      Process : not null access procedure (RS : Recordset'Class))
      is abstract;
   --  Iterate through all rows in the result set of the query

   procedure Iterate
     (RS      : in out Recordset;
      Process : not null access procedure (RS : Recordset'Class))
      is abstract;
   --  Iterate through all rows in the recordset

   procedure Iterate
     (C     : in out Connection;
      SQL   : String;
      Process : not null access procedure (Row : Gnoga.Data_Map_Type))
      is abstract;
   --  Iterate through all rows in the result set of the query

   procedure Iterate
     (RS      : in out Recordset;
      Process : not null access procedure (Row : Gnoga.Data_Map_Type))
      is abstract;
   --  Iterate through all rows in the recordset

   function Number_Of_Rows (RS : Recordset) return Natural is abstract;
   --  Return number of rows in recordset
   --  This function is not available in many implementations, check the
   --  database specific package before considering use.

   function Number_Of_Fields (RS : Recordset) return Natural is abstract;
   --  Return number of fields in recordset

   function Field_Name (RS : Recordset; Field_Number : Natural) return String
                        is abstract;
   --  Return name of field

   function Is_Null (RS : Recordset; Field_Number : Natural) return Boolean
                     is abstract;
   function Is_Null (RS : Recordset; Field_Name : String) return Boolean
                     is abstract;
   --  return True if value of field is null

   function Field_Value (RS           : Recordset;
                         Field_Number : Natural;
                         Handle_Nulls : Boolean := True)
                         return String
                         is abstract;
   function Field_Value (RS           : Recordset;
                         Field_Name   : String;
                         Handle_Nulls : Boolean := True)
                         return String
                         is abstract;
   --  return value of field, if Handle_Nulls is true, Null values will
   --  return as empty Strings

   function Field_Values (RS : Recordset) return Gnoga.Data_Map_Type
                          is abstract;
   --  return map of all values for current row, NULL values are set to
   --  an empty String

   function Escape_String (C : Connection; S : String) return String
                           is abstract;
   --  prepares a string for safe storage in a query

   Connection_Error : exception;
   --  Unable to connect to MYSQL Server or Not connected to Server

   Database_Error : exception;
   --  Unable to switch to specified Database

   Table_Error : exception;
   --  Unable to locate table or table has no fields

   Query_Error : exception;
   --  Unable to execute query

   Empty_Recordset_Error : exception;
   --  The recordset is currently empty

   Empty_Row_Error : exception;
   --  Attempt to read value from Row before calling Next

   End_Of_Recordset : exception;
   --  Attempt to go pass the last row in recordset

   No_Such_Field    : exception;
   --  The value for a field name was requested that does not exits

   Null_Field : exception;
   --  The value for a Null field was requested

   Not_Implemented : exception;
   --  If a database method is called that is not implemented by the specific
   --  database engined used this exception will be raised.
end Ada_GUI.Gnoga.Server.Database;
