-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--            G N O G A . S E R V E R . M O D E L . Q U E R I E S           --
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

with Ada.Strings.Unbounded;

package body Ada_GUI.Gnoga.Server.Model.Queries is

   --------------
   -- Find_All --
   --------------

   function Find_All
     (Name       : access String;
      Connection : access Gnoga.Server.Database.Connection'Class;
      Like       : in     String := "";
      Order_By   : in     String := "")
      return Active_Record_Array.Vector
   is
      R : Active_Record (Name, Connection);
   begin
      return Find_All (R, Like, Order_By);
   end Find_All;

   function Find_All
     (Template : Active_Record'Class;
      Like     : String := "";
      Order_By : String := "")
      return Active_Record_Array.Vector
   is
      use Ada.Strings.Unbounded;
      use Ada.Strings;

      SQL  : Unbounded_String :=
        To_Unbounded_String ("select * from ") & Template.Table_Name.all;
   begin
      if Like /= "" then
         SQL := SQL & " where " & Like;
      end if;

      if Order_By /= "" then
         SQL := SQL & " order by " & Order_By;
      end if;

      declare
         RS   : Gnoga.Server.Database.Recordset'Class :=
           Template.Connection.Query (To_String (SQL));

         Rows : Active_Record_Array.Vector;
         Row  : Active_Record (Template.Table_Name, Template.Connection);
      begin
         Row.Is_New := False;

         while RS.Next loop
            Row.Values := RS.Field_Values;
            Rows.Append (Row);
         end loop;

         RS.Close;
         return Rows;
      end;
   end Find_All;

   ----------------
   -- Find_Items --
   ----------------

   function Find_Items (Parent      : in     Active_Record'Class;
                        Child_Table : access String;
                        Like        : in      String := "";
                        Order_By    : in     String := "")
                        return Active_Record_Array.Vector
   is
      Remove_s : constant String := Parent.Table_Name.all;
      Where_Clause : constant String :=
        Remove_s (Remove_s'First .. Remove_s'Last - 1)
        & "_id = " & Parent.Value ("id");
   begin
      if Like /= "" then
         return Find_All (Child_Table, Parent.Connection,
                          Where_Clause & " and " & Like, Order_By);
      else
         return Find_All (Child_Table, Parent.Connection,
                          Where_Clause, Order_By);
      end if;
   end Find_Items;

   function Find_Items (Parent         : Active_Record'Class;
                        Child_Template : Active_Record'Class;
                        Like           : String := "";
                        Order_By       : String := "")
                        return Active_Record_Array.Vector
   is
      Remove_s : constant String := Parent.Table_Name.all;
      Where_Clause : constant String :=
        Remove_s (Remove_s'First .. Remove_s'Last - 1)
        & "_id = " & Parent.Value ("id");
   begin
      if Like /= "" then
         return Find_All (Child_Template,
                          Where_Clause & " and " & Like, Order_By);
      else
         return Find_All (Child_Template,
                          Where_Clause, Order_By);
      end if;
   end Find_Items;
end Ada_GUI.Gnoga.Server.Model.Queries;
