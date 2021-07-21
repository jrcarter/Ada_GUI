-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                G N O G A . G U I . E L E M E N T . L I S T S             --
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

with Ada_GUI.Gnoga.Gui.View;

package Ada_GUI.Gnoga.Gui.Element.List is
   --  Lists are elements, implemented as views that comprise sub element
   --  parts. Each list type has a different default display style.
   --
   --  To add elements just Item.Create the sub types.
   --  To remove from list use Item.Remove
   --  To place in a specific location use the standard Element.Place_*
   --  methods.

   -------------------------------------------------------------------------
   --  Ordered_List_Types
   -------------------------------------------------------------------------

   type Ordered_List_Type is new Gnoga.Gui.View.View_Base_Type with private;
   type Ordered_List_Access is access all Ordered_List_Type;
   type Pointer_To_Ordered_List_Class is
     access all Ordered_List_Type'Class;

   -------------------------------------------------------------------------
   --  Ordered_List_Types - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (List    : in out Ordered_List_Type;
      Parent  : in out Gnoga.Gui.Base_Type'Class;
      ID      : in     String  := "");
   --  Create an ordered (by default 1,2,3,4..) list

   -------------------------------------------------------------------------
   --  Ordered_List_Types - Properties
   -------------------------------------------------------------------------

   type List_Kind_Type is
     (Disc, Armenian, Circle, Cjk_Ideographic, Decimal, Decimal_Leading_Zero,
      Georgian, Hebrew, Hiragana, Hiragana_Iroha, Katakana, Katakana_Iroha,
      Lower_Alpha, Lower_Greek, Lower_Latin, Lower_Roman, None, Square,
      Upper_Alpha, Upper_Latin, Upper_Roman);

   procedure List_Kind (List  : in out Ordered_List_Type;
                        Value : in     List_Kind_Type);

   type List_Location_Type is (Inside, Outside);

   procedure List_Location (List  : in out Ordered_List_Type;
                            Value : in     List_Location_Type);
   --  Default is outside

   -------------------------------------------------------------------------
   --  Unordered_List_Types
   -------------------------------------------------------------------------

   type Unordered_List_Type is new Ordered_List_Type with private;
   type Unordered_List_Access is access all Unordered_List_Type;
   type Pointer_To_Unordered_List_Class is
     access all Unordered_List_Type'Class;

   -------------------------------------------------------------------------
   --  Unordered_List_Types - Creation Methods
   -------------------------------------------------------------------------

   overriding
   procedure Create
     (List    : in out Unordered_List_Type;
      Parent  : in out Gnoga.Gui.Base_Type'Class;
      ID      : in     String  := "");
   --  Create an unordered (by default) bullet/disc list

   -------------------------------------------------------------------------
   --  List_Item_Types
   -------------------------------------------------------------------------

   type List_Item_Type is new Gnoga.Gui.Element.Element_Type with private;
   type List_Item_Access is access all List_Item_Type;
   type Pointer_To_List_Item_Class is access all List_Item_Type'Class;

   -------------------------------------------------------------------------
   --  List_Item_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Item   : in out List_Item_Type;
                     Parent : in out Ordered_List_Type'Class;
                     Text   : in     String := "";
                     ID     : in     String := "");
   --  To properly display parent should be an Ordered_List_Type or an
   --  Unordered_List_Type

   -------------------------------------------------------------------------
   --  List_Item_Type - Properties
   -------------------------------------------------------------------------

   procedure Value (Element : in out List_Item_Type; Value : in String);
   function Value (Element : List_Item_Type) return String;
   --  Ordered list value, List_Item_Types added following set of Value will
   --  follow in order.

   -------------------------------------------------------------------------
   --  Definition_List_Types
   -------------------------------------------------------------------------

   type Definition_List_Type is new Gnoga.Gui.View.View_Base_Type with private;
   type Definition_List_Access is access all Definition_List_Type;
   type Pointer_To_Definition_List_Class is
     access all Definition_List_Type'Class;

   -------------------------------------------------------------------------
   --  Definition_List_Types - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (List    : in out Definition_List_Type;
      Parent  : in out Gnoga.Gui.Base_Type'Class;
      ID      : in     String  := "");
   --  Create a definition list of terms and descriptions

   -------------------------------------------------------------------------
   --  Term_Types
   -------------------------------------------------------------------------

   type Term_Type is new Gnoga.Gui.Element.Element_Type with private;
   type Term_Access is access all Term_Type;
   type Pointer_To_Term_Class is access all Term_Type'Class;

   -------------------------------------------------------------------------
   --  Term_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Item   : in out Term_Type;
                     Parent : in out Definition_List_Type'Class;
                     Text   : in     String := "";
                     ID     : in     String := "");

   -------------------------------------------------------------------------
   --  Description_Types
   -------------------------------------------------------------------------

   type Description_Type is new Gnoga.Gui.Element.Element_Type with private;
   type Description_Access is access all Description_Type;
   type Pointer_To_Description_Class is access all Description_Type'Class;

   -------------------------------------------------------------------------
   --  Description_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Item   : in out Description_Type;
                     Parent : in out Definition_List_Type'Class;
                     Text   : in     String := "";
                     ID     : in     String := "");

private
   type Ordered_List_Type is
     new Gnoga.Gui.View.View_Base_Type with null record;
   type Unordered_List_Type is new Ordered_List_Type with null record;
   type List_Item_Type is new Gnoga.Gui.Element.Element_Type with null record;
   type Definition_List_Type is
     new Gnoga.Gui.View.View_Base_Type with null record;
   type Term_Type is new Gnoga.Gui.Element.Element_Type with null record;
   type Description_Type is
     new Gnoga.Gui.Element.Element_Type with null record;
end Ada_GUI.Gnoga.Gui.Element.List;
