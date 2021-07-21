-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--            G N O G A . S E R V E R . M O D E L . Q U E R I E S           --
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

with Ada.Containers.Indefinite_Vectors;

package Ada_GUI.Gnoga.Server.Model.Queries is

   package Active_Record_Array is
     new Ada.Containers.Indefinite_Vectors (Positive, Active_Record'Class);

   function Find_All
     (Name       : access String;
      Connection : access Gnoga.Server.Database.Connection'Class;
      Like       : in     String := "";
      Order_By   : in     String := "")
      return Active_Record_Array.Vector;
   --  Return all matching records 'Like'
   --  This version of Find_All returns Active_Records Types
   --  Like is a valid SQL with clause

   function Find_All (Template : Active_Record'Class;
                      Like     : String := "";
                      Order_By : String := "")
                      return Active_Record_Array.Vector;
   --  Return all matching records 'Like'
   --  This version of Find_All duplicates the type of Template
   --  Like is a valid SQL with clause

   function Find_Items (Parent      : in     Active_Record'Class;
                        Child_Table : access String;
                        Like        : in     String := "";
                        Order_By    : in     String := "")
                        return Active_Record_Array.Vector;
   --  Return all matching records in Child Table where:
   --  Child_Table.PARENT_TABLE_NAME(with out s)_id = Child_Table.id

   function Find_Items (Parent         : Active_Record'Class;
                        Child_Template : Active_Record'Class;
                        Like           : String := "";
                        Order_By       : String := "")
                        return Active_Record_Array.Vector;
   --  Return all matching records in Child Table where:
   --  Child_Table.PARENT_TABLE_NAME(with out s)_id = Child_Table.id
end Ada_GUI.Gnoga.Server.Model.Queries;
