-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                     G N O G A . G U I . E L E M E N T                    --
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

with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with Ada_GUI.Gnoga.Colors;

package Ada_GUI.Gnoga.Gui.Element is
   -------------------------------------------------------------------------
   --  Element_Type
   -------------------------------------------------------------------------

   type Element_Type is new Gnoga.Gui.Base_Type with private;
   type Element_Access is access all Element_Type;
   type Pointer_To_Element_Class is access all Element_Type'Class;
   --  Element_Type is the parent class of all Gnoga GUI elements.
   --  It is generally used internally to create and bind Gnoga elements to
   --  HTML5 DOM elements.

   package Element_Type_Arrays is
     new Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Pointer_To_Element_Class,
        "="          => Element."=");

   package Element_Type_Maps is
      new Ada.Containers.Indefinite_Hashed_Maps (String,
                                                 Pointer_To_Element_Class,
                                                 Ada.Strings.Hash,
                                                 Equivalent_Keys => "=");

   subtype Element_Type_Array is Element_Type_Arrays.Vector;
   --  Arrays of Base_Types

   subtype Element_Type_Map is Element_Type_Maps.Map;
   --  String to Base_Type associative array

   -------------------------------------------------------------------------
   --  Element_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create_From_HTML (Element : in out Element_Type;
                               Parent  : in out Gnoga.Gui.Base_Type'Class;
                               HTML    : in     String;
                               ID      : in     String := "");
   --  Create a Gnoga element with HTML not attached to the DOM. If ID is blank
   --  Gnoga will generate a unique one for it. The created object will be
   --  stored on the browser but will not be inserted in to the DOM until
   --  Place_Inside_Top_Of, Place_Inside_Bottom_Of, Place_Before, Place_After
   --  is called. This is done automatically if Parent is a child type of
   --  Gnoga_Gui.View.View_Base_Type unless the Auto_Place property is
   --  set to False _before_ any creation method is called.
   --  Quotes in HTML have to be escaped if any.
   --
   --  Note: All IDs _must_ be unique for use in Gnoga.

   HTML_Namespace   : constant String := "http://www.w3.org/1999/xhtml";
   MathML_Namespace : constant String := "http://www.w3.org/1998/Math/MathML";
   SVG_Namespace    : constant String := "http://www.w3.org/2000/svg";
   XLink_Namespace  : constant String := "http://www.w3.org/1999/xlink";
   XMLNS_Namespace  : constant String := "http://www.w3.org/2000/xmlns/";
   XML_Namespace    : constant String :=
                        "http://www.w3.org/XML/1998/namespace";

   procedure Create_XML_Element
     (Element      : in out Element_Type;
      Parent       : in out Gnoga.Gui.Base_Type'Class;
      Namespace    : in     String;
      Element_Type : in     String;
      ID           : in     String := "");
   --  Create an XML element using XML Namespace.

   -------------------------------------------------------------------------
   --  Element_Type - Properties
   -------------------------------------------------------------------------

   --  Element Properties --

   procedure Access_Key (Element : in out Element_Type; Value : String);
   function Access_Key (Element : Element_Type) return String;
   --  Used for hot key access to element. [special key] + Access_Key
   --  The [special key] per browser and platform is:
   --
   --  Browser              Windows       Linux           Mac
   --  -----------------    -------       -----           ---
   --  Internet Explorer     [Alt]         N/A            N/A
   --  Chrome                [Alt]        [Alt]     [Control][Alt]
   --  Firefox           [Alt][Shift] [Alt][Shift]  [Control][Alt]
   --  Safari                [Alt]         N/A      [Control][Alt]
   --  Opera 15+             [Alt]        [Alt]          [Alt]

   procedure Advisory_Title (Element : in out Element_Type; Value : in String);
   function Advisory_Title (Element : Element_Type) return String;
   --  Advisory_Title of Element, usually used for body and image maps

   procedure Class_Name (Element : in out Element_Type; Value : in String);
   function Class_Name (Element : Element_Type) return String;
   --  CSS Class name, can be multiple separated by <space>
   --  See Add_Class, Remove_Class and Toggle_Class Methods for adding and
   --  removing individual or groups of classes in an easier way.

   procedure Editable (Element : in out Element_Type;
                       Value   : in     Boolean := True);
   function Editable (Element : Element_Type) return Boolean;
   --  Note: This will make almost any element with content editable, even
   --        non form types in most browsers.

   procedure Draggable (Element    : in out Element_Type;
                        Value      : in     Boolean := True);
   function Draggable (Element : Element_Type) return Boolean;
   --  In order to make an object draggable in addition to Draggable being true
   --  the On_Drag_Start event _must_ be bound as well to set the Drag_Text.
   --  To receive a drop, you need to bind On_Drop. See Gnoga.Gui

   procedure Inner_HTML (Element : in out Element_Type; Value : in String);
   function Inner_HTML (Element : Element_Type) return String;
   --  This will completely replace the inner html of an element. This will
   --  remove any Elements within Element from the DOM. If those elements
   --  have ID_Types of Gnoga_ID they are still available and can be placed
   --  in the DOM again using the Element.Place_* methods. However if they
   --  were of ID_Type DOM_ID they are lost forever.

   function Outer_HTML (Element : Element_Type) return String;
   --  Returns the HTML for Element and all its contents.

   --  Text Content Properties  --

   --  <tag>Text Content</tag> - Text content is the content contained by the
   --                            tag. This should not be confused with the
   --                            "Value" of a Form Tag.
   --                            (See. Gnoga.Gui.Element.Form.Value)

   procedure Language_Code (Element : in out Element_Type; Value : in String);
   function Language_Code (Element : Element_Type) return String;

   procedure Tab_Index (Element : in out Element_Type; Value : in Natural);
   function Tab_Index (Element : Element_Type) return Natural;

   procedure Spell_Check (Element : in out Element_Type;
                          Value   : in Boolean := True);
   function Spell_Check (Element : Element_Type) return Boolean;
   --  If true Element is subject to browser spell checking if Editable is
   --  also true.

   procedure Text (Element : in out Element_Type; Value : in String);
   function Text (Element : Element_Type) return String;
   --  Text content of element.

   type Text_Direction_Type is (Left_To_Right, Right_To_Left);

   procedure Text_Direction (Element : in out Element_Type;
                              Value  : in Text_Direction_Type);
   function Text_Direction (Element : Element_Type) return Text_Direction_Type;
   --  BiDi text direction

   --  Visibility and Layout Properties --

   procedure Hidden (Element : in out Element_Type;
                     Value   : in     Boolean := True);
   function Hidden (Element : Element_Type) return Boolean;
   --  The hidden property will make an element invisible, however unlike
   --  the property Visible which uses CSS to hide the Element, Hidden implies
   --  the element is semantically not relevant not just visually and will
   --  _also_ remove it from layout similar to setting Element.Display (None).

   procedure Visible (Element : in out Element_Type;
                      Value   : in     Boolean := True);
   function Visible (Element : Element_Type) return Boolean;
   --  This will cause the Element to no longer be visible but it will still
   --  take up space where it was in the layout. Use Element.Hidden to also
   --  remove from layout.
   --  Note: that each property, Visible, Hidden and Display (None) all work
   --  independently and do not reflect the actual client side visual state
   --  but the property state. To check if an object is for sure not visible
   --  would require checking all three properties.

   procedure Display (Element : in out Element_Type;
                      Value   : in     String);
   function Display (Element : Element_Type) return String;
   --  Display sets the CSS Display property that handles how elements are
   --  treated by the browser layout engine.
   --
   --  Common Values:
   --
   --  none         - Remove Element from layout but remain in the DOM this is
   --                 similar to Element.Hidden, but not like Element.Visible
   --                 that makes the element not visible but still take up
   --                 space in layout.
   --
   --  block        - Displays an element starting on a new line and stretches
   --                 out to the left and right as far as it can. e.g. <div> by
   --                 default
   --
   --  inline       - Wraps with text in a paragraph. e.g. <span> by default
   --
   --  inline-block - Flows with paragraph but will always fill from left to
   --                 right.
   --
   --  flex         - Use the "flexbox" model

   --  Box Properties --

   type Clear_Side_Type is (Left, Right, Both);

   procedure Clear_Side (Element : in out Element_Type;
                         Value   : in     Clear_Side_Type);
   --  When using "float" for layout sets if the right or left side
   --  of Block should be clear of any "floated" Element.

   type Float_Type is (None, Left, Right);

   procedure Layout_Float (Element : in out Element_Type;
                           Value   : in     Float_Type);
   --  Sets if Element should "float" to the left or right until touching
   --  the closest element.

   type Overflow_Type is (Visible, Hidden, Scroll, Auto);

   procedure Overflow (Element : in out Element_Type;
                       Value   : in     Overflow_Type);
   function Overflow (Element : Element_Type) return Overflow_Type;
   --  How to handle overflow of contents of an element's box
   --  The default is Visible - no clipping.

   procedure Overflow_X (Element : in out Element_Type;
                         Value   : in     Overflow_Type);
   procedure Overflow_Y (Element : in out Element_Type;
                         Value   : in     Overflow_Type);
   --  How to handle overflow of contents of an element's box for X or Y
   --  The default is Visible - no clipping.

   type Resizable_Type is (None, Both, Horizontal, Vertical);

   procedure Resizable (Element : in out Element_Type;
                        Value   : in     Resizable_Type);
   function Resizable (Element : Element_Type) return Resizable_Type;
   --  If overflow is not set to visible, sets if element can be resized
   --  by user.

   type Position_Type is (Static, Absolute, Fixed, Relative);

   procedure Position (Element : in out Element_Type;
                       Value   : in     Position_Type);
   function Position (Element : Element_Type) return Position_Type;
   --  Determines how the properties left, right, top and bottom are
   --  interpreted.
   --
   --  Static   - According to document flow, position properties have no
   --             affect.
   --  Absolute - Position properties are relative to the first non-static
   --             element in the DOM before Element
   --  Fixed    - Position properties are relative to browser window
   --  Relative - Position properties are relative to where the static position
   --             of the element would in the normal document flow.

   type Vertical_Align_Type is (Baseline, Sub, Super, Top, Middle, Bottom,
                                Text_Top, Text_Bottom);

   procedure Vertical_Align (Element : in out Element_Type;
                             Value   : in     Vertical_Align_Type);
   --  Vertical alignment of Element

   type Box_Sizing_Type is (Content_Box, Border_Box);

   procedure Box_Sizing (Element : in out Element_Type;
                         Value   : in     Box_Sizing_Type);
   function Box_Sizing (Element : Element_Type) return Box_Sizing_Type;
   --  Affects if height and width properties represent just the content or
   --  the border, margin, padding, scroll and content area as a whole.
   --  The default is Content_Box

   procedure Z_Index (Element : in out Element_Type;
                      Value   : in     Integer);
   --  Set stack order of element
   --  Note: Z_Index only works on Elements with Position Type of absolute,
   --        relative and fixed.

   procedure Margin (Element : in out Element_Type;
                     Top     : in     String := "0";
                     Right   : in     String := "0";
                     Bottom  : in     String := "0";
                     Left    : in     String := "0");
   --  Each can be - length|auto|initial|inherit

   procedure Padding (Element : in out Element_Type;
                     Top     : in     String := "0";
                     Right   : in     String := "0";
                     Bottom  : in     String := "0";
                     Left    : in     String := "0");
   --  Each can be - length|initial|inherit

   function Position_Top (Element : Element_Type) return Integer;
   function Position_Left (Element : Element_Type) return Integer;
   --  Position in pixels relative to Element's parent in the DOM

   function Offset_From_Top (Element : Element_Type) return Integer;
   function Offset_From_Left (Element : Element_Type) return Integer;
   --  Position in pixels relative to the document

   procedure Left (Element : in out Element_Type;
                   Value   : in     Integer;
                   Unit    : in     String := "px");
   procedure Left (Element : in out Element_Type;
                   Value   : in     String);
   function Left (Element : Element_Type) return String;

   procedure Right (Element : in out Element_Type;
                    Value   : in     Integer;
                    Unit    : in     String := "px");
   procedure Right (Element : in out Element_Type;
                    Value   : in     String);
   function Right (Element : Element_Type) return String;

   procedure Top (Element : in out Element_Type;
                  Value   : in     Integer;
                  Unit    : in     String := "px");
   procedure Top (Element : in out Element_Type;
                  Value   : in     String);
   function Top (Element : Element_Type) return String;

   procedure Bottom (Element : in out Element_Type;
                     Value   : in     Integer;
                     Unit    : in     String := "px");
   procedure Bottom (Element : in out Element_Type;
                     Value   : in     String);
   function Bottom (Element : Element_Type) return String;

   procedure Box_Height (Element : in out Element_Type;
                         Value   : in     Integer;
                         Unit    : in     String := "px");
   procedure Box_Height (Element : in out Element_Type;
                         Value   : in     String);
   function Box_Height (Element : Element_Type) return String;
   --  Box height based on Box_Sizing

   procedure Box_Width (Element : in out Element_Type;
                        Value   : in     Integer;
                        Unit    : in     String := "px");
   procedure Box_Width (Element : in out Element_Type;
                        Value   : in     String);
   function Box_Width (Element : Element_Type) return String;
   --  Box with based on Box_Sizing

   procedure Minimum_Height (Element : in out Element_Type;
                             Value   : in     Integer;
                             Unit    : in     String := "px");
   procedure Minimum_Height (Element : in out Element_Type;
                             Value   : in     String);
   function Minimum_Height (Element : Element_Type) return String;

   procedure Maximum_Height (Element : in out Element_Type;
                             Value   : in     Integer;
                             Unit    : in     String := "px");
   procedure Maximum_Height (Element : in out Element_Type;
                             Value   : in     String);
   function Maximum_Height (Element : Element_Type) return String;

   procedure Minimum_Width (Element : in out Element_Type;
                             Value   : in     Integer;
                             Unit    : in     String := "px");
   procedure Minimum_Width (Element : in out Element_Type;
                             Value   : in     String);
   function Minimum_Width (Element : Element_Type) return String;

   procedure Maximum_Width (Element : in out Element_Type;
                             Value   : in     Integer;
                             Unit    : in     String := "px");
   procedure Maximum_Width (Element : in out Element_Type;
                             Value   : in     String);
   function Maximum_Width (Element : Element_Type) return String;

   --  For reference:
   --  | Margin | Border | Padding | Scroll | [Element] | Scroll | Padding ...

   --  Height and Width of Element are in Base_Type
   --  All the following have the advantage of the CSS related size properties
   --  in that the results are always pixels and numeric.

   procedure Inner_Height (Element : in out Element_Type; Value : in Integer);
   function Inner_Height (Element : Element_Type) return Integer;
   --  Includes padding but not border

   procedure Inner_Width (Element : in out Element_Type; Value : in Integer);
   function Inner_Width (Element : Element_Type) return Integer;
   --  Includes padding but not border

   function Outer_Height (Element : Element_Type) return Integer;
   --  Includes padding and border but not margin

   function Outer_Width (Element : Element_Type) return Integer;
   --  Includes padding and border but not margin

   function Outer_Height_To_Margin (Element : Element_Type) return Integer;
   --  Includes padding and border and margin

   function Outer_Width_To_Margin
     (Element : Element_Type) return Integer;
   --  Includes padding and border and margin

   function Client_Width (Element : Element_Type) return Natural;
   --  Inner width of an element in pixels.
   --  CSS width + CSS padding - width of vertical scrollbar (if present)
   --  Does not include the border or margin.

   function Client_Height (Element : Element_Type) return Natural;
   --  Inner height of an element in pixels.
   --  CSS height + CSS padding - height of horizontal scrollbar (if present)
   --  Does not include the border or margin.

   function Client_Left (Element : Element_Type) return Natural;
   --  The width of the left border of an element in pixels.
   --. It does not include the margin or padding.

   function Client_Top (Element : Element_Type) return Natural;
   --  The width of the top border of an element in pixels.
   --. It does not include the margin or padding.

   function Offset_Width (Element : Element_Type) return Integer;
   --  CSS width + CSS padding + width of vertical scrollbar (if present) +
   --  Border

   function Offset_Height (Element : Element_Type) return Integer;
   --  CSS height + CSS padding + height of horizontal scrollbar (if present) +
   --  Border

   function Offset_Left (Element : Element_Type) return Integer;
   --  The width from parent element border to child border left

   function Offset_Top (Element : Element_Type) return Integer;
   --  The width from parent element border to child border top

   function Scroll_Width (Element : Element_Type) return Natural;
   --  Either the width in pixels of the content of an element or the width of
   --  the element itself, whichever is greater

   function Scroll_Height (Element : Element_Type) return Natural;
   --  Height of an element's content, including content not visible on the
   --  screen due to overflow.

   procedure Scroll_Left (Element : in out Element_Type; Value : Integer);
   function Scroll_Left (Element : Element_Type) return Integer;
   --  The number of pixels that an element's content is scrolled to the left.
   --  For RTL languages is negative.

   procedure Scroll_Top (Element : in out Element_Type; Value : Integer);
   function Scroll_Top (Element : Element_Type) return Integer;
   --  The number of pixels that an element's content has been scrolled
   --  upward.

   -- Style Properties --

   --  Color  --

   procedure Color (Element : in out Element_Type; Value : String);
   procedure Color (Element : in out Element_Type;
                    RGBA    : in     Gnoga.RGBA_Type);
   procedure Color (Element : in out Element_Type;
                    Enum    : Gnoga.Colors.Color_Enumeration);
   function Color (Element : Element_Type) return Gnoga.RGBA_Type;

   procedure Opacity (Element : in out Element_Type;
                      Alpha   : in     Gnoga.Alpha_Type);
   function Opacity (Element : Element_Type) return Gnoga.Alpha_Type;

   --  Background --

   type Background_Attachment_Type is (Scroll, Fixed, Local);

   procedure Background_Attachment
     (Element : in out Element_Type;
      Value   : in     Background_Attachment_Type);
   function Background_Attachment (Element : Element_Type)
                                   return Background_Attachment_Type;

   procedure Background_Color (Element : in out Element_Type;
                               Value   : in     String);
   procedure Background_Color (Element : in out Element_Type;
                               RGBA    : in     Gnoga.RGBA_Type);
   procedure Background_Color
     (Element : in out Element_Type;
      Enum    : in     Gnoga.Colors.Color_Enumeration);
   function Background_Color (Element : Element_Type)
                              return Gnoga.RGBA_Type;

   procedure Background_Image (Element : in out Element_Type;
                               Value   : in     String);
   function Background_Image (Element : Element_Type) return String;
   --  proper syntax is "url(...)" | "" to clear

   procedure Background_Position (Element : in out Element_Type;
                                  Value   : in     String);
   function Background_Position (Element : Element_Type) return String;
   --  combination of 2 - left/right/center/top/bottom | %x %y | x y

   procedure Background_Origin (Element : in out Element_Type;
                                Value   : in     String);
   function Background_Origin (Element : Element_Type) return String;
   --  Background position property is relative to origin of:
   --  padding-box|border-box|content-box

   procedure Background_Repeat (Element : in out Element_Type;
                                Value   : in     String);
   function Background_Repeat (Element : Element_Type) return String;
   --  repeat|repeat-x|repeat-y|no-repeat

   procedure Background_Clip (Element : in out Element_Type;
                              Value   : in     String);
   function Background_Clip (Element : Element_Type) return String;
   --  border-box|padding-box|content-box

   procedure Background_Size (Element : in out Element_Type;
                              Value   : in     String);
   function Background_Size (Element : Element_Type) return String;
   --  auto| w h | % = cover of parent | contain

   --  Border  --

   type Border_Style is (None, Hidden, Dotted, Dashed, Solid, Double, Groove,
                         Ridge, Inset, Outset);
   procedure Border (Element : in out Element_Type;
                     Width   : in     String       := "medium";
                     Style   : in     Border_Style := Solid;
                     Color   : in     Gnoga.Colors.Color_Enumeration :=
                       Gnoga.Colors.Black);
   --  Width = medium|thin|thick|length|initial|inherit;
   --  If Color is "" then border is same as Element.Color

   procedure Border_Radius (Element : in out Element_Type;
                            Radius  : in     String := "0");
   --  Curve of borders
   --  Radius = length|%|initial|inherit

   procedure Shadow (Element             : in out Element_Type;
                     Horizontal_Position : in     String;
                     Vertical_Position   : in     String;
                     Blur                : in     String := "";
                     Spread              : in     String := "";
                     Color               : in     Gnoga.Colors.Color_Enumeration := Gnoga.Colors.Black;
                     Inset_Shadow        : in     Boolean := False);

   procedure Shadow_None (Element : in out Element_Type);

   type Outline_Style_Type is (None, Hidden, Dotted, Dashed, Solid, Double,
                               Groove, Ridge, Inset, Outset);

   procedure Outline (Element : in out Element_Type;
                      Color   : in     String             := "invert";
                      Style   : in     Outline_Style_Type := None;
                      Width   : in     String             := "medium");

   procedure Cursor (Element : in out Element_Type;
                     Value   : in     String);
   function Cursor (Element : Element_Type) return String;
   --  Sets the cursor to a standard type or an image
   --  if set to url(url_to_image). When using a url is best
   --  to suggest an alternate cursor, e.g. "url(url_to_image),auto"
   --  A list of standard cursor types can be found at:
   --  http://www.w3schools.com/cssref/pr_class_cursor.asp

   -- Text --

   type Font_Style_Type is (Normal, Italic, Oblique);

   type Font_Weight_Type is (Weight_Normal, Weight_Bold,
                             Weight_Bolder, Weight_Lighter,
                             Weight_100, Weight_200, Weight_300,
                             Weight_400, Weight_500, Weight_600,
                             Weight_700, Weight_800, Weight_900);

   function Image (Value : in Gnoga.Gui.Element.Font_Weight_Type) return String;

   function Value (Value : in String) return Gnoga.Gui.Element.Font_Weight_Type;

   type Font_Variant_Type is (Normal, Small_Caps);

   type System_Font_Type is (Caption, Icon, Menu, Message_Box, Small_Caption,
                             Status_Bar);

   procedure Font (Element : in out Element_Type;
                   Family  : in     String            := "sans-serif";
                   Height  : in     String            := "medium";
                   Style   : in     Font_Style_Type   := Normal;
                   Weight  : in     Font_Weight_Type  := Weight_Normal;
                   Variant : in     Font_Variant_Type := Normal);
   procedure Font (Element     : in out Element_Type;
                   System_Font : in     System_Font_Type);
   --  Sets or returns the current font properties for text content

   type Alignment_Type is (Left, Right, Center, At_Start, To_End);

   procedure Text_Alignment (Element : in out Element_Type;
                             Value   : in     Alignment_Type);
   --  Text Alignment, At_Start = Left, and To_End = Right in ltr languages
   --  in rtl languages At_Start = Right, and To_End = Left.

   --  Framework Properties --

   procedure Auto_Place (Element : in out Element_Type; Value : Boolean);
   function Auto_Place (Element : Element_Type) return Boolean;
   --  Elements by default are created outside the DOM and therefore not
   --  visible. If Auto_Place is set to false _before_ Create is called on
   --  an Element, View's will not place the Element in to the DOM as is
   --  the View's default behavior. Custom widgets that have child widgets
   --  should be designed to respect this property. Auto_Place if set to
   --  False will also prevent Auto_Set_View if Element's Parent is a Window

   --  General Access to Element --

   procedure Style (Element : in out Element_Type;
                    Name    : in String;
                    Value   : in String);
   procedure Style (Element : in out Element_Type;
                    Name    : in String;
                    Value   : in Integer);
   function Style (Element : Element_Type; Name : String) return String;
   function Style (Element : Element_Type; Name : String) return Integer;
   --  General access to style Name

   procedure Attribute (Element : in out Element_Type;
                        Name    : in String;
                        Value   : in String);
   function Attribute (Element : Element_Type; Name : String) return String;
   --  General access to attribute Name

   -- Traversal Properties --

   procedure First_Child (Element : in out Element_Type;
                          Child  : in out Element_Type'Class);
   --  If Child does not have an html id than Element_Type will have an
   --  ID of undefined and therefore attached to no actual HTML element.

   procedure Next_Sibling (Element : in out Element_Type;
                           Sibling : in out Element_Type'Class);
   --  If Sibling does not have an html id than Element_Type will have an
   --  ID of undefined and therefore attached to no actual HTML element.

   -- Internal Properties --

   function HTML_Tag (Element : Element_Type) return String;

   -------------------------------------------------------------------------
   --  Element_Type - Methods
   -------------------------------------------------------------------------

   -- Element Methods --

   procedure Click (Element : in out Element_Type);
   --  Simulate click on element

   procedure Add_Class (Element : in out Element_Type; Class_Name : in String);
   --  Adds one or more Class_Name(s) to Element

   procedure Remove_Class (Element    : in out Element_Type;
                           Class_Name : in     String);
   --  Removes one or more Class_Name(s) to Element

   procedure Toggle_Class (Element    : in out Element_Type;
                           Class_Name : in     String);
   --  Toggles on and off one or more Class_Name(s) to Element

   -- DOM Placement Methods --

   procedure Place_Inside_Top_Of (Element : in out Element_Type;
                                  Target  : in out Element_Type'Class);

   procedure Place_Inside_Bottom_Of (Element : in out Element_Type;
                                     Target  : in out Element_Type'Class);

   procedure Place_Before (Element : in out Element_Type;
                           Target  : in out Element_Type'Class);

   procedure Place_After (Element : in out Element_Type;
                          Target  : in out Element_Type'Class);

   procedure Remove (Element : in out Element_Type);
   --  Removes an element from the DOM, if the ID_Type is DOM_ID, the ID
   --  will be changed to a unique Gnoga_ID before removal.

private
   type Element_Type is new Gnoga.Gui.Base_Type with
      record
         Auto_Place : Boolean := True;
      end record;
end Ada_GUI.Gnoga.Gui.Element;
