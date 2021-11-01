-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                       G N O G A . G U I . V I E W                        --
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

--  Views are used to handle auto insertion of objects in to the DOM and
--  placement. They are also the base of creating custom widgets.

with Ada_GUI.Gnoga.Gui.Element;

package Ada_GUI.Gnoga.Gui.View is

   -------------------------------------------------------------------------
   --  View_Base_Types
   -------------------------------------------------------------------------

   type View_Base_Type is new Gnoga.Gui.Element.Element_Type with private;
   type View_Base_Access is access all View_Base_Type;
   type Pointer_To_View_Base_Class is access all View_Base_Type'Class;

   --  Note: In order for views to receive keyboard events the Tab_Index
   --        property should be set to any value.

   overriding
   procedure Finalize (Object : in out View_Base_Type);
   --  Deallocate any child element that was marked Dynamic before
   --  being added to View. Child element's marked as Dynamic by calling
   --  Element_Type.Dynamic then created with a View as the parent should
   --  never be deallocated. If you plan on deallocating a child element in
   --  your code, do not mark as Dynamic.

   -------------------------------------------------------------------------
   --  View_Base_Type - Methods
   -------------------------------------------------------------------------

   procedure Fill_Parent (View : in out View_Base_Type);
   --  Cause View to expand its height and width to fill its parent's client
   --  area. View's parent must have Position set to Absolute, Fixed or
   --  Relative for Fill_Parent to work properly.
   --  Note:
   --     Position will be modified for View to either Absolute or Relative
   --     if Absolute positioning fails (i.e. on IE)

   procedure Put_Line (View    : in out View_Base_Type;
                       Message : in     String;
                       Class   : in     String := "";
                       ID      : in     String := "");
   --  Create a new DIV with Message and append to end of view.
   --  Use View.Overflow (Scroll) to allow scroll bars to view overflow
   --  of data added by Put_Line. Class is an optional CSS class and
   --  ID and option DOM ID.

   procedure Put (View    : in out View_Base_Type;
                  Message : in     String;
                  Class   : in     String := "";
                  ID      : in     String := "");
   --  Create a new SPAN with Message and append to end of view.
   --  Spans are added inline unlike DIVs that are blocks taking up an
   --  an entire row.

   procedure New_Line (View : in out View_Base_Type);
   --  Create a new <br /> and append to end of view

   procedure Horizontal_Rule (View : in out View_Base_Type);
   --  Create a new <hr /> and append to end of View

   procedure Put_HTML (View  : in out View_Base_Type;
                       HTML  : in     String;
                       Class : in     String := "";
                       ID    : in     String := "");
   --  Place HTML directly in to view. The HTML will not be wrapped in a DIV
   --  or Span automatically, therefore HTML must be a complete block.
   --  e.g.  <p>I'm a block</p>  <br>One line<br>Two Lines</br>But not a block
   --  Use Put_Line or Put to wrap HTML.

   procedure Load_File (View      : in out View_Base_Type;
                        File_Name : in     String;
                        Class     : in     String := "";
                        ID        : in     String := "");
   --  Load contents of _local_ File_Name in to a <div>.
   --  Unless path given uses Gnoga.Server.Templates_Directory

   procedure Load_HTML (View      : in out View_Base_Type;
                        File_Name : in     String;
                        Class     : in     String := "";
                        ID        : in     String := "");
   --  Load contents of _local_ HTML file called File_Name
   --  All contents before <body> and after </body> will be discarded.
   --  <body></body> will be replaced with <div></div> and Class and ID
   --  set on them.
   --  Unless path given uses Gnoga.Server.Templates_Directory

   procedure Load_CSS (View : in out View_Base_Type;
                       URL  : in     String);
   --  Appends <style src=URL> to document head
   --  Unless path given uses Gnoga.Server.Templates_Directory

   procedure Load_CSS_File (View      : in out View_Base_Type;
                            File_Name : in     String);
   --  Load contents of local File_Name in to a <style> block and appends it
   --  to document head.
   --  Unless path given uses Gnoga.Server.Templates_Directory

   procedure Add_Element
     (View    : in out View_Base_Type;
      Name    : in     String;
      Element : Gnoga.Gui.Element.Pointer_To_Element_Class);
   --  Add Element to associative array of elements at Name and available using
   --  the View_Base_Type's Element property. This does not re-parent the
   --  Element to View if it was created with a different parent nor does it
   --  add Element to the View's DOM. If Element with Name exists it will be
   --  overwritten.

   function New_Element
     (View    : access View_Base_Type;
      Name    : String;
      Element : Gnoga.Gui.Element.Pointer_To_Element_Class)
      return Gnoga.Gui.Element.Pointer_To_Element_Class;
   --  Only use for dynamic objects.
   --  Performs like Add_Element (View, Name, Element); Element.Dynamic;
   --  It returns Element in order to allow for this idiom:
   --  Common.Button_Access
   --   (View.New_Element ("my button", new Common.Button_Type)).Create (View);

   function Add
     (View    : access View_Base_Type;
      Element : access Gnoga.Gui.Element.Element_Type'Class)
      return Gnoga.Gui.Element.Pointer_To_Element_Class;
   --  Only use for dynamic objects.
   --  Marks Element as Dynamic and returns Element. This is primarily of value
   --  for creating a dynamic element that you will no longer interact with
   --  in the future since all reference is lost. Use New_Element if future
   --  named access desired instead. Use with the following idiom:
   --  Common.Button_Access
   --    (View.Add (new Common.Button_Type)).Create (View);

   -------------------------------------------------------------------------
   --  View_Base_Type - Properties
   -------------------------------------------------------------------------

   function Element (View : View_Base_Type; Name : String)
                     return Gnoga.Gui.Element.Pointer_To_Element_Class;
   --  Access elements added by Add_Element and New_Element
   --  returns null if Name not found.

   function Element_Names (View : View_Base_Type)
                           return Gnoga.Data_Array_Type;
   --  Return an array of all the names of elements in the view's element
   --  array

   -------------------------------------------------------------------------
   --  View_Base_Type - Event Methods
   -------------------------------------------------------------------------

   overriding
   procedure On_Child_Added
     (View  : in out View_Base_Type;
      Child : in out Gnoga.Gui.Base_Type'Class);
   --  All children of views should be Element_Type'Class, if it is not
   --  it will be ignored. Any child professing the View as its parent
   --  will automatically have Element.Place_Inside_Bottom_Of (View) applied
   --  to it.
   --  Note: Only if an element is marked as dynamic before its Create is
   --  called it is slated for garbage collection by View.

   -------------------------------------------------------------------------
   --  View_Types
   -------------------------------------------------------------------------

   type View_Type is new View_Base_Type with private;
   type View_Access is access all View_Type;
   type Pointer_To_View_Class is access all View_Type'Class;

   -------------------------------------------------------------------------
   --  View_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (View   : in out View_Type;
      Parent : in out Gnoga.Gui.Base_Type'Class;
      ID     : in     String  := "");
   --  If Parent is a Window_Type'Class will automatically set itself
   --  as the View on Parent if Attach is True.

private
   type View_Base_Type is new Gnoga.Gui.Element.Element_Type with
      record
         Child_Array : Gnoga.Gui.Base_Type_Array;
         Element_Map : Gnoga.Gui.Element.Element_Type_Map;
      end record;

   type View_Type is new View_Base_Type with null record;
end Ada_GUI.Gnoga.Gui.View;
