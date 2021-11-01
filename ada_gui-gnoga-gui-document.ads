-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                    G N O G A . G U I . D O C U M E N T                   --
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

with Ada_GUI.Gnoga.Gui.Element;

package Ada_GUI.Gnoga.Gui.Document is

   -------------------------------------------------------------------------
   --  Document_Type
   -------------------------------------------------------------------------
   --  Document_Type is the class encapsulating the DOM's root document node
   --  To use, access via Window_Type.Document

   type Document_Type is new Gnoga.Gui.Base_Type with private;
   type Document_Access is access all Document_Type;
   type Pointer_To_Document_Class is access all Document_Type'Class;

   Invalid_ID_Type : exception;

   overriding procedure Attach
     (Document      : in out Document_Type;
      Connection_ID : in     Gnoga.Connection_ID;
      ID            : in     String                     := "window";
      ID_Type       : in     Gnoga.ID_Enumeration := Gnoga.Script);
   --  Attach a Gnoga Document_Type to the document on Connection_ID
   --  ID in this context does not mean as on other objects the ID of this
   --  element, but rather the ID of the parent Window for the document.
   --  ID_Type of DOM_ID will raise Invalid_ID_Type;

   -------------------------------------------------------------------------
   --  Document_Type - Properties
   -------------------------------------------------------------------------

   function Domain (Document : Document_Type) return String;

   function Input_Encoding (Document : Document_Type) return String;

   function Last_Modified (Document : Document_Type) return String;

   function Referrer (Document : Document_Type) return String;

   procedure Title (Document : in out Document_Type; Value : in String);
   function Title (Document : Document_Type) return String;

   function URL (Document : Document_Type) return String;

   function Head_Element (Document : Document_Type)
                          return Gnoga.Gui.Element.Element_Access;

   function Body_Element (Document : Document_Type)
                          return Gnoga.Gui.Element.Element_Access;

   function Document_Element (Document : Document_Type)
                              return Gnoga.Gui.Element.Element_Access;
   --  The document element is the root of the document, example <HTML>
   --  in a text/html document. Using Document_Element.Outer_HTML will
   --  return the entire document as per the current state in the browser.

   type Ready_State_Type is (Uninitialized, Loading, Interactive, Complete);

   function Ready_State (Document : Document_Type)
                         return Ready_State_Type;

   -------------------------------------------------------------------------
   --  Document_Type - Methods
   -------------------------------------------------------------------------

   procedure Load_CSS (Document : in out Document_Type; URL : in String);
   --  Loads a CSS file in to document from URL

   procedure Write (Document : in out Document_Type; Value : in String);
   procedure Write_Line (Document : in out Document_Type; Value : in String);
   --  Write Value (with new line if Write_Line) to the document.
   --  Note that the first use of these procedures can erase elements already
   --  inserted in to the Document.Body_Element. Therefore to use these
   --  Write/Write_Line so that a text node is created before inserting any
   --  elements into the document body.
   --  In general, use View.Put_Line instead with in a view that is attached
   --  to the window.

   procedure Put_Line (Document : in out Document_Type; Value : in String);
   --  Calls Write_Line with Value + "<br />"
   --  Using Put_Line on Firefox will break the WebSocket connection
   --  In general, use View.Put_Line instead with in a view that is attached
   --  to the window.
private
   type Document_Type is new Gnoga.Gui.Base_Type with
      record
         DOM_HTML : aliased Gnoga.Gui.Element.Element_Type;
         DOM_Head : aliased Gnoga.Gui.Element.Element_Type;
         DOM_Body : aliased Gnoga.Gui.Element.Element_Type;
      end record;
end Ada_GUI.Gnoga.Gui.Document;
