-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                      G N O G A . G U I . W I N D O W                     --
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

with Ada_GUI.Gnoga.Gui.Document;
with Ada_GUI.Gnoga.Gui.Location;

package Ada_GUI.Gnoga.Gui.Window is

   -------------------------------------------------------------------------
   --  Window_Type
   -------------------------------------------------------------------------
   --  Window_Type is the class encapsulating an individual Gnoga browser
   --  window or tab.

   type Window_Type is new Gnoga.Gui.Base_Type with private;
   type Window_Access is access all Window_Type;
   type Pointer_To_Window_Class is access all Window_Type'Class;

   overriding
   procedure Finalize (Object : in out Window_Type);
   --  Will deallocate the Connection Data if marked dynamic.
   --  Will deallocate an attached View (see Set_View) if the view is
   --  marked dynamic.
   --  See Base_Type.Dynamic

   Invalid_ID_Type    : exception;

   Not_A_Gnoga_Window : exception;

   overriding procedure Attach
     (Window        : in out Window_Type;
      Connection_ID : in     Gnoga.Connection_ID;
      ID            : in     String                     := "window";
      ID_Type       : in     Gnoga.ID_Enumeration := Gnoga.Script);
   --  Attach a Gnoga Window_Type to Connection_ID.
   --  If ID_Type = DOM_ID it will raise Invalid_ID_Type
   --
   --  This can also be used to attach  a non-Gnoga Window and events bound
   --  will use Connection_ID which must be the parent window in the DOM for
   --  such cases.

   procedure Attach
     (Window  : in out Window_Type;
      Parent  : in out Window_Type'Class;
      ID      : in     String;
      ID_Type : in     Gnoga.ID_Enumeration := Gnoga.Script);
   --  Attach a Window with script ID with in the Parent window.
   --  If ID_Type = DOM_ID it will raise Invalid_ID_Type
   --
   --  Note: This will only work if the window pointed to by ID has
   --        a Gnoga connection in it. Raises Not_A_Gnoga_Window if window
   --        does not contain a gnoga connection.

   procedure Reattach (Window : in out Window_Type;
                       Parent : in out Window_Type'Class);
   --  Attach a Window launched by Parent to its own Gnoga connection.
   --  Will raise Not_A_Gnoga_Window if connection was not established in
   --  Window or is not a Gnoga window (i.e. if has no websocket to app).

   procedure Disable_Auto_Set_View (Window : in out Window_Type;
                                    Value  : in     Boolean := True);
   --  By default if a View is created with Window as the Parent, Set_View
   --  will be called to make it the View for the window.

   procedure Set_View (Window : in out Window_Type;
                       Object : in out Gnoga.Gui.Base_Type'Class;
                       Place  : in     Boolean := True);
   --  Sets Object as the Window's View. Object will be auto resized to fill
   --  the entire client area of Window. By default any View_Base_Type'Class
   --  with Window as parent will automatically be set as the View. To change
   --  this use Disable_Auto_Set_View or to prevent for a single create
   --  set Auto_Place (False) before creating that view.
   --
   --  If Place is True then Object will first be placed using
   --  Object.Place_Inside_Top_Of (Window);
   --
   --  If Object is not a Gnoga.Gui.Element_Type or a child of it (such as
   --  all View_Base_Types) an exception will be raised.
   --
   --  If Object is marked dynamic (Element_Type.Dynamic) it will be
   --  deallocated on finalization of Window if it has not been removed using
   --  Window.Remove_View

   function Get_View (Window : Window_Type)
                      return Gnoga.Gui.Pointer_To_Base_Class;
   --  Returns the current view attached to Window or null

   procedure Remove_View (Window : in out Window_Type);
   --  Remove the current View Object from Window

   -------------------------------------------------------------------------
   --  Window_Type - Creation Methods
   -------------------------------------------------------------------------

   Popup_Blocked : exception;

   procedure Launch (Window   : in out Window_Type;
                     Parent   : in out Window_Type'Class;
                     URL      : in     String;
                     Width    : in     Integer := -1;
                     Height   : in     Integer := -1;
                     Left     : in     Integer := -1;
                     Top      : in     Integer := -1;
                     Location : in     Boolean := False;
                     Menu     : in     Boolean := False;
                     Status   : in     Boolean := False;
                     Tool_Bar : in     Boolean := False;
                     Title    : in     Boolean := False);
   --  Launch a new Window on Parent's connection. If Parent window is closed
   --  events will no longer work on Window. If the launched URL has a Gnoga
   --  connection can run Reattach on it before adding any elements or events.
   --  Before running Reattach you need to be sure that the gnoga websocket
   --  has been established on the launched page.
   --
   --  Popups are highly unreliable and outside of Google Chrome only work in
   --  limited ways beyond just a launch and forget situation.

   -------------------------------------------------------------------------
   --  Window_Type - Properties
   -------------------------------------------------------------------------

   function Document (Window : Window_Type)
                      return Gnoga.Gui.Document.Document_Access;
   --  DOM Document Node

   function Location (Window : Window_Type)
                      return Gnoga.Gui.Location.Location_Access;
   --  Browser location object

   procedure Browser_Status_Bar (Window : in out Window_Type;
                                 Value  : in     String);
   function Browser_Status_Bar (Window : Window_Type) return String;
   --  Status bar on browser window (not supported on all browsers)

   procedure Name (Window : in out Window_Type; Value : in String);
   function Name (Window : Window_Type) return String;
   --  Hyperlink "target" Name for Window

   procedure Inner_Height (Window : in out Window_Type; Value : in Integer);
   function Inner_Height (Window : Window_Type) return Integer;

   procedure Inner_Width (Window : in out Window_Type; Value : in Integer);
   function Inner_Width (Window : Window_Type) return Integer;

   procedure Outer_Height (Window : in out Window_Type; Value : in Integer);
   function Outer_Height (Window : Window_Type) return Integer;

   procedure Outer_Width (Window : in out Window_Type; Value : in Integer);
   function Outer_Width (Window : Window_Type) return Integer;

   function X_Offset (Window : Window_Type) return Integer;

   function Y_Offset (Window : Window_Type) return Integer;

   function Top (Window : Window_Type) return Integer;

   function Left (Window : Window_Type) return Integer;

   function Form_Parameter (Window : Window_Type; Name  : String)
                            return String;
   --  Returns the value of parameters passed in on URL. Returns "undefined"
   --  if Name is not in URL search parameters or received via post.
   --  For example: http://localhost:8080/?page_id=2
   --  Form_Parameter (Window, "page_id") = "2"

   --  Framework Properties  --

   function Gnoga_Session_ID (Window : Window_Type; Name : String := "gid")
                              return String;
   --  If Name exists in Client.Storage.Session_Storage it returns that value,
   --  if not a unique Session ID is generated and stored for future
   --  invocations.

   procedure Connection_Data
     (Window  : in out Window_Type;
      Data    : access Gnoga.Connection_Data_Type'Class;
      Dynamic : in     Boolean := True);
   --  Associates Data with the the connection Window is on.
   --  If Dynamic is true, Data will be unallocated when Window is
   --  finalized. The data can be access from any on same connection as
   --  Window using Gnoga.Gui.Connection_Data

   -------------------------------------------------------------------------
   --  Window_Type - Methods
   -------------------------------------------------------------------------

   procedure Alert (Window : in out Window_Type; Message : String);
   --  Display Alert box on Window with Message

   procedure Log (Window : in out Window_Type; Message : String);
   --  Log message on browser console

   procedure Error (Window : in out Window_Type; Message : String);
   --  Log error message on browser console

   procedure Print (Window : in out Window_Type);
   --  Print Window contents

   procedure Scroll_By (Window : in out Window_Type; X, Y : Integer);
   --  Scroll contents in window by x, y

   procedure Scroll_To (Window : in out Window_Type; X, Y : Integer);
   --  Scroll contents in window to x, y

   procedure Close_Connection (Window : in out Window_Type);
   --  Close connection to browser used by Window

   -- Window Placement Methods --

   --  These methods will only work on child windows that have been launched

   procedure Close (Window : in out Window_Type);

   procedure Resize_By (Window : in out Window_Type; Width, Height : Integer);

   procedure Resize_To (Window : in out Window_Type; Width, Height : Integer);

   procedure Move_By (Window : in out Window_Type; X, Y : Integer);

   procedure Move_To (Window : in out Window_Type; X, Y : Integer);

   -------------------------------------------------------------------------
   --  Window_Type - Event Methods
   -------------------------------------------------------------------------

   procedure On_Resize (Window : in out Window_Type);
   --  Handles resizing the View attached to Window to new
   --  height and width of Window

   overriding
   procedure On_Child_Added (Object : in out Window_Type;
                             Child  : in out Gnoga.Gui.Base_Type'Class);
   --  Handles auto attaching Views to Window.
private
   type Window_Type is new Gnoga.Gui.Base_Type with record
      DOM_Document         : aliased Gnoga.Gui.Document.Document_Type;
      Location             : aliased Gnoga.Gui.Location.Location_Type;
      View                 : Gnoga.Gui.Pointer_To_Base_Class := null;
      View_Is_Dynamic      : Boolean                         := False;
      Free_Connection_Data : Boolean                         := False;
      Auto_Set_View        : Boolean                         := True;
   end record;
end Ada_GUI.Gnoga.Gui.Window;
