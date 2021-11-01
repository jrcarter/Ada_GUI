-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--               G N O G A . G U I . E L E M E N T . C O M M O N            --
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

package Ada_GUI.Gnoga.Gui.Element.Common is
   -------------------------------------------------------------------------
   --  A_Types
   -------------------------------------------------------------------------

   type A_Type is new Gnoga.Gui.View.View_Base_Type with private;
   type A_Access is access all A_Type;
   type Pointer_To_A_Class is access all A_Type'Class;

   -------------------------------------------------------------------------
   --  A_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (A       : in out A_Type;
                     Parent  : in out Gnoga.Gui.Base_Type'Class;
                     Link    : in     String := "";
                     Content : in     String := "";
                     Target  : in     String := "_self";
                     ID      : in     String := "");
   --  Create an Anchor link

   -------------------------------------------------------------------------
   --  A_Type - Properties
   -------------------------------------------------------------------------

   procedure Link (A : in out A_Type; Value : String);
   function Link (A : A_Type) return String;
   --  The HREF link of the Anchor

   procedure Target (A : in out A_Type; Value : String);
   function Target (A : A_Type) return String;
   --  Target of link, name of a frame or:
   --  _blank  = new window
   --  _top    = top most frame (full browser window)
   --  _parent = parent frame or window
   --  _self   = current frame or window

   -------------------------------------------------------------------------
   --  Button_Types
   -------------------------------------------------------------------------

   type Button_Type is new Gnoga.Gui.View.View_Base_Type with private;
   type Button_Access is access all Button_Type;
   type Pointer_To_Button_Class is access all Button_Type'Class;

   -------------------------------------------------------------------------
   --  Button_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Button  : in out Button_Type;
                     Parent  : in out Gnoga.Gui.Base_Type'Class;
                     Content : in     String := "";
                     ID      : in     String := "");
   --  Create an HTML button. The content will be placed inside the button.
   --  For forms use Gnoga.Gui.Element.Form.Button instead.
   --  Button_Type's can contain other elements like images.

   -------------------------------------------------------------------------
   --  Button_Type - Properties
   -------------------------------------------------------------------------

   procedure Disabled (Button : in out Button_Type;
                       Value  : in     Boolean := True);
   function Disabled (Button : Button_Type) return Boolean;

   -------------------------------------------------------------------------
   --  DIV_Types
   -------------------------------------------------------------------------

   type DIV_Type is new Gnoga.Gui.View.View_Base_Type with private;
   type DIV_Access is access all DIV_Type;
   type Pointer_To_DIV_Class is access all DIV_Type'Class;

   -------------------------------------------------------------------------
   --  DIV_Type - Creation Methods
   -------------------------------------------------------------------------
   --  Also note, that View_Base_Type.Put_Line also creates DIVs internally

   procedure Create (DIV     : in out DIV_Type;
                     Parent  : in out Gnoga.Gui.Base_Type'Class;
                     Content : in     String := "";
                     ID      : in     String := "");
   --  Create a div container with optional HTML content

   -------------------------------------------------------------------------
   --  P_Types
   -------------------------------------------------------------------------

   type P_Type is new Gnoga.Gui.View.View_Base_Type with private;
   type P_Access is access all P_Type;
   type Pointer_To_P_Class is access all P_Type'Class;

   -------------------------------------------------------------------------
   --  P_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (P       : in out P_Type;
                     Parent  : in out Gnoga.Gui.Base_Type'Class;
                     Content : in     String := "";
                     ID      : in     String := "");

   -------------------------------------------------------------------------
   --  IMG_Types
   -------------------------------------------------------------------------

   type IMG_Type is new Gnoga.Gui.Element.Element_Type with private;
   type IMG_Access is access all IMG_Type;
   type Pointer_To_IMG_Class is access all IMG_Type'Class;

   -------------------------------------------------------------------------
   --  IMG_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (IMG              : in out IMG_Type;
                     Parent           : in out Gnoga.Gui.Base_Type'Class;
                     URL_Source       : in     String := "";
                     Alternative_Text : in     String := "";
                     ID               : in     String := "");
   --  Create an image element. Use width and height properties before
   --  placing image to constrain image size.

   procedure URL_Source (IMG : in out IMG_Type; Value : in String);
   --  Change URL Source for IMG

   -------------------------------------------------------------------------
   --  HR_Types
   -------------------------------------------------------------------------

   type HR_Type is new Gnoga.Gui.Element.Element_Type with private;
   type HR_Access is access all HR_Type;
   type Pointer_To_HR_Class is access all HR_Type'Class;

   -------------------------------------------------------------------------
   --  HR_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (HR     : in out HR_Type;
                     Parent : in out Gnoga.Gui.Base_Type'Class;
                     ID     : in     String := "");
   --  Create a horizontal rule

   -------------------------------------------------------------------------
   --  BR_Types
   -------------------------------------------------------------------------

   type BR_Type is new Gnoga.Gui.Element.Element_Type with private;
   type BR_Access is access all BR_Type;
   type Pointer_To_BR_Class is access all BR_Type'Class;

   -------------------------------------------------------------------------
   --  BR_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (BR     : in out BR_Type;
                     Parent : in out Gnoga.Gui.Base_Type'Class;
                     ID     : in     String := "");

   -------------------------------------------------------------------------
   --  Meter_Types
   -------------------------------------------------------------------------

   type Meter_Type is new Gnoga.Gui.Element.Element_Type with private;
   type Meter_Access is access all Meter_Type;
   type Pointer_To_Meter_Class is access all Meter_Type'Class;

   -------------------------------------------------------------------------
   --  Meter_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Meter   : in out Meter_Type;
                     Parent  : in out Gnoga.Gui.Base_Type'Class;
                     Value   : in     Integer := 0;
                     High    : in     Integer := 100;
                     Low     : in     Integer := 0;
                     Maximum : in     Integer := 100;
                     Minimum : in     Integer := 0;
                     Optimum : in     Integer := 50;
                     ID      : in     String := "");

   -------------------------------------------------------------------------
   --  Meter_Type - Properties
   -------------------------------------------------------------------------

   procedure Value (Meter : in out Meter_Type; Value : in Integer);
   function Value (Meter : Meter_Type) return Integer;

   procedure High (Meter : in out Meter_Type; Value : in Integer);
   function High (Meter : Meter_Type) return Integer;

   procedure Low (Meter : in out Meter_Type; Value : in Integer);
   function Low (Meter : Meter_Type) return Integer;

   procedure Maximum (Meter : in out Meter_Type; Value : in Integer);
   function Maximum (Meter : Meter_Type) return Integer;

   procedure Minimum (Meter : in out Meter_Type; Value : in Integer);
   function Minimum (Meter : Meter_Type) return Integer;

   procedure Optimum (Meter : in out Meter_Type; Value : in Integer);
   function Optimum (Meter : Meter_Type) return Integer;

   -------------------------------------------------------------------------
   --  Progress_Bar_Types
   -------------------------------------------------------------------------

   type Progress_Bar_Type is new Gnoga.Gui.Element.Element_Type with private;
   type Progress_Bar_Access is access all Progress_Bar_Type;
   type Pointer_To_Progress_Bar_Class is access all Progress_Bar_Type'Class;

   -------------------------------------------------------------------------
   --  Progress_Bar_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Progress_Bar : in out Progress_Bar_Type;
                     Parent       : in out Gnoga.Gui.Base_Type'Class;
                     Value        : in     Integer := 0;
                     Maximum      : in     Integer := 100;
                     ID           : in     String := "");

   -------------------------------------------------------------------------
   --  Progress_Bar_Type - Properties
   -------------------------------------------------------------------------

   procedure Value (Progress_Bar : in out Progress_Bar_Type;
                    Value        : in     Integer);
   function Value (Progress_Bar : Progress_Bar_Type) return Integer;

   procedure Maximum (Progress_Bar : in out Progress_Bar_Type;
                      Value        : in     Integer);
   function Maximum (Progress_Bar : Progress_Bar_Type) return Integer;

   -------------------------------------------------------------------------
   --  Span_Types
   -------------------------------------------------------------------------

   type Span_Type is new Gnoga.Gui.View.View_Base_Type with private;
   type Span_Access is access all Span_Type;
   type Pointer_To_Span_Class is access all Span_Type'Class;

   -------------------------------------------------------------------------
   --  Span_Type - Creation Methods
   -------------------------------------------------------------------------
   --  The Spans are created automatically when using View_Base_Type.Put

   procedure Create (Span    : in out Span_Type;
                     Parent  : in out Gnoga.Gui.Base_Type'Class;
                     Content : in     String := "";
                     ID      : in     String := "");
   --  Create a Span container

private
   type A_Type is new Gnoga.Gui.View.View_Base_Type with null record;
   type Button_Type is new Gnoga.Gui.View.View_Base_Type with null record;
   type DIV_Type is new Gnoga.Gui.View.View_Base_Type with null record;
   type P_Type is new Gnoga.Gui.View.View_Base_Type with null record;
   type IMG_Type is new Gnoga.Gui.Element.Element_Type with null record;
   type HR_Type is new Gnoga.Gui.Element.Element_Type with null record;
   type BR_Type is new Gnoga.Gui.Element.Element_Type with null record;
   type Meter_Type is new Gnoga.Gui.Element.Element_Type with null record;
   type Progress_Bar_Type is
     new Gnoga.Gui.Element.Element_Type with null record;
   type Span_Type is new Gnoga.Gui.View.View_Base_Type with null record;
end Ada_GUI.Gnoga.Gui.Element.Common;
