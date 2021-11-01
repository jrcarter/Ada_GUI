-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--          G N O G A . S E R V E R . T E M P L A T E _ P A R S E R         --
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

with Ada.Finalization;
with Ada.Strings.Unbounded;

with Ada_GUI.Gnoga.Server.Database;
with Ada_GUI.Gnoga.Server.Model.Queries;

package Ada_GUI.Gnoga.Server.Template_Parser is

   --  Gnoga.Server.Template_Parser uses PHP, Python or Ada as template parsing
   --  engines. It also makes it possible to leverage and reuse existing
   --  web resources until they can be ported to Gnoga. To use Python instead
   --  of PHP see Gnoga.Server.Template_Parser.Python. If only simple parsing
   --  is needed (find and replace), use Ada_GUI.Gnoga.Server.Template_Parser.Simple
   --  that uses Ada.Text_IO
   --
   --  Note: Template_Parser is not intended to be used for web site / app
   --        development but for tools or for use by apps to manipulate files.
   --        That doesn't mean you couldn't develop an entire site using it
   --        and production sites have been made with it. Keep in mind though
   --        that the security advantages of Gnoga are reduced if you are using
   --        a CGI like approach using a template_parser based on PHP or
   --        Python.

   type View_Data is new Ada.Finalization.Controlled with private;
   type View_Data_Array is array (Positive range <>) of View_Data;

   overriding procedure Finalize (Object : in out View_Data);

   procedure Variable_Name (Data : in out View_Data;
                            Name : String);
   --  Sets the variable name used for the passed data
   --  by default the Name is "data" and so in the views
   --  accessed as - $data[key] (or data['key'] in python, or
   --  @@data.key@@ in the Simple parser).

   procedure Insert (Data  : in out View_Data;
                     Key   : String;
                     Value : Integer);
   --  Add Key/Value pair to View Data
   --  accessed in view as - $data[key], data['key'] or @@data.key@@

   procedure Insert (Data  : in out View_Data;
                     Key   : String;
                     Value : String);
   --  Add Key/Value pair to View Data
   --  accessed in view as - $data[key], data['key'] or @@data.key@@

   procedure Insert_Array (Data   : in out View_Data;
                           Vector : Gnoga.Data_Array_Type);
   --  Add an entire Array from a Data_Vector.Vector to View_Data
   --  access in view as $data[Index], data[Index] or @@data.Index@@

   procedure Insert_Array_Item (Data   : in out View_Data;
                                Key    : String;
                                Vector : Gnoga.Data_Array_Type);
   --  Add an entire Array from a Data_Vector.Vector to View_Data
   --  as a single item.
   --  access in view as $data[Key][Array Index], data[Key][Index] or
   --  @@data.Key.Index@@

   procedure Insert_Map (Data   : in out View_Data;
                         Map    : Gnoga.Data_Map_Type);
   --  Add an entire Map of Key/Value pairs from a Gnoga.Data_Map_Type
   --  to View_Data each item will be accessed as - $data[key] where key is key
   --  in Map, or for Python and Simple parser data['key'] or @@data.key@@

   procedure Insert_Map_Item (Data   : in out View_Data;
                              Key    : String;
                              Map    : Gnoga.Data_Map_Type);
   --  Add an entire Map of Key/Value pairs from a Gnoga.Data_Map_Type
   --  to View_Data as a single item.
   --  access in view as $data[Key][Map's_Key], data['Key']['Map's_Key']
   --  or @@data.Key.Map's_Key@@

   procedure Insert_Record_Item
     (Data   : in out View_Data;
      Key    : String;
      Row    : Gnoga.Server.Model.Active_Record'Class);
   --  Add field values of an Active_Record to View Data as a single item.
   --  access in view as $data[Key][Row's Field Name],
   --  data['Key']['Row's Field Name'] or @@data.Key.Row's Field Name@@

   procedure Insert_Rows
     (Data   : in out View_Data;
      Row    : Gnoga.Server.Model.Queries.Active_Record_Array.Vector);
   --  Add a vector of Active_Records to View_Data.
   --  Each Active_Record is keyed by row number
   --  access in view as $data[Row Number][Row's Field Name]
   --  data[Row Number]['Row's Field Name'] or
   --  @@data.Row Number.Row's Field Name@@

   procedure Insert_Recordset
     (Data   : in out View_Data;
      RS     : in out Gnoga.Server.Database.Recordset'Class);
   --  Add a recordset to View_Data. Each record is keyed by row number
   --  access in view as $data[Row Number][Row's Field Name]
   --  data[Row Number]['Row's Field Name'] or
   --  @@data.Row Number.Row's Field Name@@

   procedure Insert_Query
     (Data  : in out View_Data;
      C     : Gnoga.Server.Database.Connection'Class;
      Query : String);
   --  Inserts the results of a SQL Query. Each record is keyed by row number

   procedure Clear (Data : in out View_Data);
   --  Clear contents of View_Data

   procedure Add_Error_Message (Message : String);
   --  Adds an error message to the error queue. The error queue is accessed
   --  in the view as $gnoga_errors[Row Number]

   procedure Clear_Error_Queue;
   --  Clears any error messages from the error queue

   procedure Add_Info_Message (Message : String);
   --  Adds an information message to the info queue. The info queue is
   --  accessed in the view as $gnoga_infos[Row Number]

   procedure Clear_Info_Queue;
   --  Clears any info messaes from the information queue

   Parser_Execution_Failure : exception;
   --  Raised if failed to execute parser application

   procedure Set_Template_Directory (Directory : String);
   --  Set the default extension for views. By default this is at
   --  Gnoga.Server.Templates_Directory

   procedure Write_String_To_File (File_Name : String; Value : String);
   --  Name is file name (no path is prefixed) to write Value to.
   --  File will be overwritten if exists.

private
   type View_Data is new Ada.Finalization.Controlled with
      record
         Name          : Ada.Strings.Unbounded.Unbounded_String :=
           Ada.Strings.Unbounded.To_Unbounded_String ("data");
         String_Values : Gnoga.Data_Map_Type;
         Map_Values    : Gnoga.Map_of_Data_Maps_Type;
      end record;

   function Parse_Name (Name : String) return String;
   --  prefix path from Set_Template_Directory to Name
   --  unless Name contains a ':' (as in C:) or starts
   --  with the OS separator ('/' or '\' on Windows)

   Error_Queue : Gnoga.Data_Array_Type;
   Info_Queue  : Gnoga.Data_Array_Type;
end Ada_GUI.Gnoga.Server.Template_Parser;
