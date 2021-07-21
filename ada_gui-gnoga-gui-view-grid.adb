-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                    G N O G A . G U I . V I E W . G R I D                 --
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

with Ada_GUI.Gnoga.Server.Connection;
with Ada_GUI.Gnoga.Gui.Window;

package body Ada_GUI.Gnoga.Gui.View.Grid is

   ------------
   -- Create --
   ------------

   procedure Create
     (Grid          : in out Grid_View_Type;
      Parent        : in out Gnoga.Gui.Base_Type'Class;
      Layout        : in     Grid_Rows_Type;
      Fill_Parent   : in     Boolean := True;
      Set_Sizes     : in     Boolean := True;
      ID            : in     String  := "")
   is
      use Ada.Strings.Unbounded;

      C   : Unbounded_String;
      CID : constant String           := Gnoga.Server.Connection.New_GID;

      N         : Natural;
      Span_Size : Natural;
      Column    : Positive;

      P_Height : constant String :=
        Left_Trim (Integer (100 / Layout'Length (1))'Img) & "%";
      P_Width  : constant String :=
        Left_Trim (Integer (100 / Layout'Length (2))'Img) & "%";

      Column_Object : View_Base_Access := null;

      function TD_Width  return String;

      function TD_Width return String is
      begin
         if Set_Sizes then
            return " width:" & P_Width & ";";
         else
            return "";
         end if;
      end TD_Width;

   begin
      if Parent in Gnoga.Gui.Window.Window_Type'Class then
         C := To_Unbounded_String ("<div style='position:relative'>");
      end if;

      C := C & "<table style='" &
        " position:relative;" &
        " border-spacing: 0px; border-collapse: collapse;";

      if Fill_Parent then
         C := C & " width:100%; height:100%;'>";
      else
         C := C & "'>";
      end if;

      N := 0;
      for Row in Layout'Range (1) loop
         C := C & "<tr>";

         Column := Layout'First (2);
         Span_Size := 0;

         loop
            if Layout (Row, Column) = COL then
               N := N + 1;
               C := C & "<td style='" &
                 TD_Width &
                 " position:relative;" &
                 " padding:0; text-align: left; vertical-align: top;" &
                 "' id='" & CID & "_" & Left_Trim (N'Img) & "'";
               Span_Size := 1;
            elsif Layout (Row, Column) = SPN then
               Span_Size := Span_Size + 1;
            end if;

            Column := Column + 1;

            if Column > Layout'Last (2) or else Layout (Row, Column) = COL then
               if Span_Size > 0 then
                  if Span_Size > 1 then
                     C := C & " colspan=" & Left_Trim (Span_Size'Img);
                  end if;

                  C := C & " />";
               end if;
            end if;

            exit when Column > Layout'Last (2);
         end loop;

         C := C & "</tr>";
      end loop;
      C := C & "</table>";

      if Parent in Gnoga.Gui.Window.Window_Type'Class then
         C := C & "</div>";
      end if;

      Grid.Create_From_HTML (Parent, Escape_Quotes (To_String (C)), ID);

      N := 0;
      for Row in Layout'Range (1) loop
         Column := Layout'First (2);
         Span_Size := 0;

         loop
            if Layout (Row, Column) = COL then
               N := N + 1;
               Span_Size := 1;

               Column_Object := new View_Base_Type;
               Column_Object.Auto_Place (False);
               Column_Object.Dynamic (True);
               Column_Object.Attach_Using_Parent
                 (Grid, CID & "_" & Left_Trim (N'Img));
               Column_Object.Parent (Grid);

               if Column = Layout'First (2) and Row = Layout'First (1) then
                  Column_Object.Box_Height (P_Height);
               end if;
            elsif Layout (Row, Column) = SPN then
               Span_Size := Span_Size + 1;
            end if;

            declare
               Address : constant String := Left_Trim (Row'Img) & "_" &
                 Left_Trim (Column'Img);
            begin
               Grid.Add_Element (Address, Gnoga.Gui.Element.Pointer_To_Element_Class (Column_Object));
            end;

            Column := Column + 1;
            exit when Column > Layout'Last (2);
         end loop;
      end loop;
   end Create;

   -----------
   -- Panel --
   -----------

   function Panel (Grid : Grid_View_Type; Row, Column : Positive)
                   return Pointer_To_View_Base_Class
   is
      Address : constant String :=
        Left_Trim (Row'Img) & "_" & Left_Trim (Column'Img);
   begin
      return Pointer_To_View_Base_Class (Grid.Element (Address));
   end Panel;

end Ada_GUI.Gnoga.Gui.View.Grid;
