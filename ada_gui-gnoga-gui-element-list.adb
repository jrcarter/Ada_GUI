-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                G N O G A . G U I . E L E M E N T . L I S T S             --
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

package body Ada_GUI.Gnoga.Gui.Element.List is

   ------------
   -- Create --
   ------------

   procedure Create
     (List    : in out Ordered_List_Type;
      Parent  : in out Gnoga.Gui.Base_Type'Class;
      ID      : in     String  := "")
   is
   begin
      List.Create_From_HTML (Parent, "<ol />", ID);
   end Create;

   ---------------
   -- List_Kind --
   ---------------

   procedure List_Kind
     (List  : in out Ordered_List_Type;
      Value : in     List_Kind_Type)
   is
      function Adjusted_Image (S : String) return String;

      function Adjusted_Image (S : String) return String is
         P : constant Integer := Ada.Strings.Fixed.Index (S, "_");
      begin
         if P = 0 then
            return S;
         else
            return Adjusted_Image (S (S'First .. (P - 1)) &
                                     "-" & S ((P + 1) .. S'Last));
         end if;
      end Adjusted_Image;
   begin
      List.Style ("list-style-type", Adjusted_Image (Value'Img));
   end List_Kind;

   -------------------
   -- List_Location --
   -------------------

   procedure List_Location
     (List  : in out Ordered_List_Type;
      Value : in     List_Location_Type)
   is
   begin
      List.Style ("list-style-position", Value'Img);
   end List_Location;

   ------------
   -- Create --
   ------------

   overriding
   procedure Create
     (List    : in out Unordered_List_Type;
      Parent  : in out Gnoga.Gui.Base_Type'Class;
      ID      : in     String  := "")
   is
   begin
      List.Create_From_HTML (Parent, "<ul />", ID);
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item   : in out List_Item_Type;
      Parent : in out Ordered_List_Type'Class;
      Text   : in     String := "";
      ID     : in     String := "")
   is
   begin
      Item.Create_From_HTML
        (Parent, "<li>" & Escape_Quotes (Text) & "</li>", ID);
   end Create;

   -----------
   -- Value --
   -----------

   procedure Value (Element : in out List_Item_Type; Value : in String) is
   begin
      Element.Property ("value", Value);
   end Value;

   function Value (Element : List_Item_Type) return String is
   begin
      return Element.Property ("value");
   end Value;

   ------------
   -- Create --
   ------------

   procedure Create
     (List    : in out Definition_List_Type;
      Parent  : in out Gnoga.Gui.Base_Type'Class;
      ID      : in     String  := "")
   is
   begin
      List.Create_From_HTML (Parent, "<dl />", ID);
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item   : in out Term_Type;
      Parent : in out Definition_List_Type'Class;
      Text   : in     String := "";
      ID     : in     String := "")
   is
   begin
      Item.Create_From_HTML
        (Parent, "<dt>" & Escape_Quotes (Text) & "</dt>", ID);
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item   : in out Description_Type;
      Parent : in out Definition_List_Type'Class;
      Text   : in     String := "";
      ID     : in     String := "")
   is
   begin
      Item.Create_From_HTML
        (Parent, "<dd>" & Escape_Quotes (Text) & "</dd>", ID);
   end Create;

end Ada_GUI.Gnoga.Gui.Element.List;
