-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                       G N O G A . G U I . V I E W                        --
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

with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;

with Ada_GUI.Gnoga.Gui.Document;
with Ada_GUI.Gnoga.Gui.Element.Common;

with Ada_GUI.Gnoga.Server.Connection;
with Ada_GUI.Gnoga.Server.Template_Parser.Simple;

package body Ada_GUI.Gnoga.Gui.View is

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (Object : in out View_Base_Type) is
   begin
      if not Gnoga.Server.Connection.Shutting_Down then
         for i in
           Object.Child_Array.First_Index .. Object.Child_Array.Last_Index
         loop
            if Object.Child_Array.Element (i).Dynamic then
               Object.Child_Array.Element (i).Free;
            end if;
         end loop;
      end if;

      Gnoga.Gui.Element.Element_Type (Object).Finalize;
   end Finalize;

   ------------
   -- Create --
   ------------

   procedure Create
     (View    : in out View_Type;
      Parent  : in out Gnoga.Gui.Base_Type'Class;
      ID      : in     String := "")
   is
   begin
      View.Create_From_HTML (Parent, "<div />", ID);
   end Create;

   --------------------
   -- On_Child_Added --
   --------------------

   overriding
   procedure On_Child_Added (View  : in out View_Base_Type;
                             Child : in out Gnoga.Gui.Base_Type'Class)
   is
      use Gnoga.Gui.Element;
   begin
      if Child in Element_Type'Class then
         if Element_Type (Child).Auto_Place then
            Element_Type (Child).Place_Inside_Bottom_Of (View);
         end if;
      end if;

      if Child.Dynamic then
         View.Child_Array.Append (Child'Unchecked_Access);
      end if;
   end On_Child_Added;

   ------------------
   --  Fill_Parent --
   ------------------

   procedure Fill_Parent (View : in out View_Base_Type) is
      use Gnoga.Gui.Element;
   begin
      View.Position (Absolute);
      View.Box_Height ("100%");
      View.Box_Width ("100%");
      if View.Height = 0 then
         View.Position (Relative);
      end if;
   end Fill_Parent;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (View    : in out View_Base_Type;
                       Message : in     String;
                       Class   : in     String := "";
                       ID      : in     String := "")
   is
      D : Gnoga.Gui.Element.Common.DIV_Type;
   begin
      D.Create (View, Message, ID);
      if Class /= "" then
         D.Class_Name (Class);
      end if;
   end Put_Line;

   ---------
   -- Put --
   ---------

   procedure Put (View    : in out View_Base_Type;
                  Message : in     String;
                  Class   : in     String := "";
                  ID      : in     String := "")
   is
      S : Gnoga.Gui.Element.Common.Span_Type;
   begin
      S.Create (View, Message, ID);
      if Class /= "" then
         S.Class_Name (Class);
      end if;
   end Put;

   --------------
   -- Put_HTML --
   --------------

   procedure Put_HTML (View  : in out View_Base_Type;
                       HTML  : in     String;
                       Class : in     String := "";
                       ID    : in     String := "")
   is
      D : Gnoga.Gui.Element.Element_Type;
   begin
      D.Create_From_HTML (View, Escape_Quotes (HTML), ID);
      if Class /= "" then
         D.Class_Name (Class);
      end if;
   end Put_HTML;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (View : in out View_Base_Type) is
   begin
      View.Put_HTML ("<br />");
   end New_Line;

   ---------------------
   -- Horizontal_Rule --
   ---------------------

   procedure Horizontal_Rule (View : in out View_Base_Type) is
   begin
      View.Put_HTML ("<hr />");
   end Horizontal_Rule;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File (View      : in out View_Base_Type;
                        File_Name : in     String;
                        Class     : in     String := "";
                        ID        : in     String := "")
   is
      S : constant String :=
        Gnoga.Server.Template_Parser.Simple.Load_View (File_Name);
   begin
      View.Put_Line (S, Class, ID);
   end Load_File;

   ---------------
   -- Load_HTML --
   ---------------

   procedure Load_HTML (View      : in out View_Base_Type;
                        File_Name : in     String;
                        Class     : in     String := "";
                        ID        : in     String := "")
   is
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps.Constants;

      S : constant String  :=
        Gnoga.Server.Template_Parser.Simple.Load_View (File_Name);
      B : constant Natural := Index (Source  => S,
                            Pattern => "<body",
                            Mapping => Lower_Case_Map);
      T : constant Natural := Index (Source  => S,
                            Pattern => ">",
                            From    => B);
      E : constant Natural := Index (Source  => S,
                            Pattern => "</body",
                            Mapping => Lower_Case_Map);
   begin
      if B > 0 and E > 0 then
         View.Put_HTML (S (T + 1 .. E - 1), Class, ID);
      end if;
   end Load_HTML;

   --------------
   -- Load_CSS --
   --------------

   procedure Load_CSS (View : in out View_Base_Type;
                       URL  : in     String)
   is
      Document : Gnoga.Gui.Document.Document_Type;
   begin
      Document.Attach (View.Connection_ID);
      Document.Head_Element.jQuery_Execute
         ("append ('" & Escape_Quotes ("<link rel='stylesheet' href='" &
          Escape_Inner_Quotes (URL) & "' />") & "')");
   end Load_CSS;

   -------------------
   -- Load_CSS_File --
   -------------------

   procedure Load_CSS_File (View      : in out View_Base_Type;
                            File_Name : in     String)
   is
      S : constant String :=
        Gnoga.Server.Template_Parser.Simple.Load_View (File_Name);

      Document : Gnoga.Gui.Document.Document_Type;
   begin
      Document.Attach (View.Connection_ID);
      Document.Head_Element.jQuery_Execute
        ("append ('<style>" & Escape_Quotes (S) & "</style>'");
   end Load_CSS_File;

   -----------------
   -- Add_Element --
   -----------------

   procedure Add_Element
     (View    : in out View_Base_Type;
      Name    : in     String;
      Element : Gnoga.Gui.Element.Pointer_To_Element_Class)
   is
   begin
      View.Element_Map.Include (Key      => Name,
                                New_Item => Element);
   end Add_Element;

   -----------------
   -- New_Element --
   -----------------

   function New_Element
     (View    : access View_Base_Type;
      Name    : String;
      Element : Gnoga.Gui.Element.Pointer_To_Element_Class)
      return Gnoga.Gui.Element.Pointer_To_Element_Class
   is
   begin
      View.Add_Element (Name, Element);
      Element.Dynamic;
      return Element.all'Unrestricted_Access;
   end New_Element;

   ---------
   -- Add --
   ---------

   function Add
     (View    : access View_Base_Type;
      Element : access Gnoga.Gui.Element.Element_Type'Class)
      return Gnoga.Gui.Element.Pointer_To_Element_Class
   is
      pragma Unreferenced (View);
   begin
      Element.Dynamic;
      return Element.all'Unrestricted_Access;
   end Add;

   -------------
   -- Element --
   -------------

   function Element (View : View_Base_Type; Name : String)
                     return Gnoga.Gui.Element.Pointer_To_Element_Class
   is
   begin
      if View.Element_Map.Contains (Name) then
         return View.Element_Map.Element (Name);
      else
         return null;
      end if;
   end Element;

   -------------------
   -- Element_Names --
   -------------------

   function Element_Names (View : View_Base_Type)
                           return Gnoga.Data_Array_Type
   is
      Names : Gnoga.Data_Array_Type;

      procedure Add_Name (C : in Gnoga.Gui.Element.Element_Type_Maps.Cursor);

      procedure Add_Name (C : in Gnoga.Gui.Element.Element_Type_Maps.Cursor) is
      begin
         Names.Append (String'(Gnoga.Gui.Element.Element_Type_Maps.Key (C)));
      end Add_Name;
   begin
      View.Element_Map.Iterate (Add_Name'Access);

      return Names;
   end Element_Names;

end Ada_GUI.Gnoga.Gui.View;
