-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                    G N O G A . G U I . V I E W . G R I D                 --
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

package Ada_GUI.Gnoga.Gui.View.Grid is

   -------------------------------------------------------------------------
   --  Grid_View_Types
   -------------------------------------------------------------------------

   type Grid_View_Type is new View_Base_Type with private;
   type Grid_View_Access is access all Grid_View_Type;
   type Pointer_To_Grid_View_Class is access all Grid_View_Type'Class;

   -------------------------------------------------------------------------
   --  Grid_View_Type - Creation Methods
   -------------------------------------------------------------------------

   type Grid_Element_Type is (COL, SPN);
   --  COL = A single column
   --  SPN = Span previous column through this column

   type Grid_Rows_Type is
     array (Positive range <>, Positive range <>) of Grid_Element_Type;

   Vertical_Split   : constant Grid_Rows_Type := ((1 => COL), (1 => COL));
   Horizontal_Split : constant Grid_Rows_Type := (1 => (COL, COL));

   procedure Create
     (Grid          : in out Grid_View_Type;
      Parent        : in out Gnoga.Gui.Base_Type'Class;
      Layout        : in     Grid_Rows_Type;
      Fill_Parent   : in     Boolean := True;
      Set_Sizes     : in     Boolean := True;
      ID            : in     String  := "");
   --  Create a grid of views using Layout. If Set_Sizes is true then default
   --  equal size columns and rows will be created and widths set as
   --  percentages.
   --
   --  Example:
   --     Grid.Create (Main_Window,
   --                  ((COL, SPN),
   --                   (COL, COL),
   --                   (COL, SPN)),
   --                  Fill_Parent => True);
   --  This will create a top view two columns wide, left and right middle
   --  views and a bottom view two columns wide. Provided Main_Window is a
   --  window type, since Fill_Parent is True the grid will fill the entire browser
   --  window.
   --
   --  Note: If Set_Sizes is used any change in size of panels should be
   --        done using percentages, e.g. Box_Width ("20%"). In all cases
   --        changing the size of individual panels but not others may not
   --        always give the expected results.

   -------------------------------------------------------------------------
   --  Grid_View_Type - Properties
   -------------------------------------------------------------------------

   function Panel (Grid : Grid_View_Type; Row, Column : Positive)
                   return Pointer_To_View_Base_Class;
   --  Return the Panel view at Row, Column. Every member of a column span
   --  will return a pointer to the same view.

private
   type Grid_View_Type is new View_Type with null record;
end Ada_GUI.Gnoga.Gui.View.Grid;
