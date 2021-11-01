-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                       G N O G A . G U I . B A S E                        --
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

with Ada.Finalization;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Strings.Hash;

package Ada_GUI.Gnoga.Gui is

   -------------------------------------------------------------------------
   --  Base_Type
   -------------------------------------------------------------------------

   type Base_Type is new Ada.Finalization.Limited_Controlled with private;
   type Base_Access is access all Base_Type;
   type Pointer_To_Base_Class is access all Base_Type'Class;
   --  Base_Type is the parent class of all Gnoga GUI Objects.
   --  It is generally used internally to create and bind Gnoga objects to
   --  HTML5 DOM objects.

   Object_Already_Created : exception;
   --  Raised when an attempt is made to perform a create method on an already
   --  created or attached Gnoga object.

   Object_Was_Not_Created : exception;
   --  Raised when an attempt was made to use an object that has not yet been
   --  created on the client.

   overriding
   procedure Initialize (Object : in out Base_Type);

   overriding
   procedure Finalize (Object : in out Base_Type);

   procedure Free (Object : in out Base_Type);
   --  Free a dynamically created Object

   package Base_Type_Arrays is
     new Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Pointer_To_Base_Class,
        "="          => "=");

   package Base_Type_Maps is
      new Ada.Containers.Indefinite_Hashed_Maps (String,
                                                 Pointer_To_Base_Class,
                                                 Ada.Strings.Hash,
                                                 Equivalent_Keys => "=");

   subtype Base_Type_Array is Base_Type_Arrays.Vector;
   --  Arrays of Base_Types

   subtype Base_Type_Map is Base_Type_Maps.Map;
   --  String to Base_Type associative array

   -------------------------------------------------------------------------
   --  Base_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create_With_Script
     (Object        : in out Base_Type;
      Connection_ID : in     Gnoga.Connection_ID;
      ID            : in     String;
      Script        : in     String;
      ID_Type       : in     ID_Enumeration := DOM_ID);
   --  Create a Gnoga object on Connection ID with ID using Script.
   --  The Script must include creating the id attribute equal to ID.
   --  Script is eval'd JavaScript.
   --  Note ID _must_ be unique for use in Gnoga.

   procedure Attach_Using_Parent
     (Object   : in out Base_Type;
      Parent   : in     Base_Type'Class;
      ID       : in     String;
      ID_Type  : in     ID_Enumeration := DOM_ID);
   --  Attach a Gnoga object using Connection ID from Parent to an existing
   --  DOM object with ID. On_Create event is not fired.
   --  Note ID _must_ be unique for use in Gnoga.

   procedure Attach
     (Object        : in out Base_Type;
      Connection_ID : in     Gnoga.Connection_ID;
      ID            : in     String;
      ID_Type       : in     ID_Enumeration := DOM_ID);
   --  Attache a Gnoga object on Connection ID to an existing DOM object
   --  with ID. On_Create event is not fired.
   --  Note ID _must_ be unique for use in Gnoga.

   -------------------------------------------------------------------------
   --  Base_Type - Properties
   -------------------------------------------------------------------------

   --  Object Properties --

   --  For reference:
   --  | Margin | Border | Padding | Scroll | [Element] | Scroll | Padding ...

   procedure Height (Object : in out Base_Type; Value : in Integer);
   function Height (Object : Base_Type) return Integer;
   --  Height of Element, or Window or Document
   --  Results in Pixels and numeric unlike using the CSS size properties
   --  See Element_Type for additional Height and Width properties

   procedure Width (Object : in out Base_Type; Value : in Integer);
   function Width (Object : Base_Type) return Integer;
   --  Width of Element, or Window or Document
   --  Results in Pixels and numeric unlike using the CSS size properties
   --  See Element_Type for additional Height and Width properties

   --  Framework Properties  --

   function Unique_ID (Object : Base_Type) return Unique_ID;
   --  Returns the Unique_ID for Object

   function Connection_ID (Object : Base_Type)
                           return Gnoga.Connection_ID;
   procedure Connection_ID (Object : in out Base_Type;
                            Value  : in Gnoga.Connection_ID);
   --  The Gnoga Connection ID of Object.
   --  It is almost certainly always a mistake to set Connection_ID instead
   --  of using Attach. Only change the Connection ID if you fully understand
   --  what you are doing.

   function Valid (Object : Base_Type) return Boolean;
   --  Returns true if Connection_ID is valid, i.e. Object was created and
   --  the connection is still valid.

   function ID (Object : Base_Type) return String;
   procedure ID (Object  : in out Base_Type;
                 ID      : in     String;
                 ID_Type : in     ID_Enumeration);
   --  The ID for Object. Use Attach for attaching to existing objects in,
   --  setting the ID should only be done with full understanding of the Gnoga
   --  internals.

   function ID_Type (Object : Base_Type) return ID_Enumeration;
   --  Returns the type of ID stored for Object or No_ID if object has not
   --  been created or attached on the client side.

   function DOM_Selector (Object : Base_Type) return String;
   --  Returns the DOM_ID for Object, "#" & Object.ID or ID_Type is Gnoga_ID or
   --  DOM_ID otherwise returns ID.

   function Connection_Data
     (Object : Base_Type)
      return Pointer_To_Connection_Data_Class;
   --  Returns the connection specific Data for the connection Object is on.
   --  This is usually set with Gnoga.Gui.Window.Connection_Data

   function Parent (Object : Base_Type)
                    return Pointer_To_Base_Class;
   procedure Parent (Object : in out Base_Type;
                     Value  : in out Base_Type'Class);
   procedure Parent (Object : in out Base_Type;
                     Value  : in Pointer_To_Base_Class);
   --  Parent of Object. Setting/changing the parent will fire the
   --  On_Parent_Added event on the Parent object and if changing the parent
   --  On_Parent_Removed event on the old Parent
   --
   --  Setting the parent Object does not change the position Object may have
   --  in the DOM by default. That should be done using Element_Type.Place_*

   procedure Dynamic (Object : in out Base_Type; Value : Boolean := True);
   function Dynamic (Object : Base_Type) return Boolean;
   --  Can be used to mark an object as dynamically allocated instead of
   --  on the stack. This in of itself does not do anything, but Views
   --  will deallocate on finalization children that are marked as Dynamic
   --  _before_ being Created with the View as parent.
   --  See Gnoga.Gui.View
   --  If you plan on deallocating a child element in your code, do not mark it
   --  as Dynamic. Marking Dynamic is for the purpose of automatic garbage
   --  collection by Gnoga's framework.

   function Buffer_Connection (Object : Base_Type) return Boolean;
   procedure Buffer_Connection (Object : in out Base_Type;
                                Value  : in     Boolean := True);
   --  Set buffering all output to browser on connection used by Object.

   --  Generic Access  --

   procedure Property (Object : in out Base_Type;
                       Name   : in     String;
                       Value  : in     String);
   function Property (Object : Base_Type; Name : String) return String;
   --  General access to property Name as a String

   procedure Property (Object : in out Base_Type;
                       Name   : in     String;
                       Value  : in     Integer);
   function Property (Object : Base_Type; Name : String) return Integer;
   --  General access to property Name as an Integer
   --  If Property returns a float value it will be converted in to an
   --  Integer.

   procedure Property (Object : in out Base_Type;
                       Name   : in     String;
                       Value  : in     Float);
   function Property (Object : Base_Type; Name : String) return Float;
   --  General access to property Name as a Float

   procedure Property (Object : in out Base_Type;
                       Name   : in     String;
                       Value  : in     Boolean);
   function Property (Object : Base_Type; Name : String) return Boolean;
   --  General access to property Name as a Boolean

   -------------------------------------------------------------------------
   --  Base_Type - Methods
   -------------------------------------------------------------------------

   --  Object Methods --

   procedure Focus (Object : in out Base_Type);
   --  Set focus on Object

   procedure Blur (Object : in out Base_Type);
   --  Remove focus from Object

   -- Framework Methods --

   procedure Flush_Buffer (Object : in out Base_Type);
   --  Flush buffer to browser on connection used by Object

   --  Generic Methods --

   procedure Execute (Object : in out Base_Type; Method : in String);
   function Execute (Object : Base_Type; Method : in String) return String;
   --  General access to execute a Method and access to a Method as a String

   function Execute (Object : Base_Type; Method : String) return Integer;
   --  General access to a Method as an Integer
   --  If Method returns a float value it will be converted in to an
   --  Integer.

   function Execute (Object : Base_Type; Method : String) return Float;
   --  General access to a Method as a Float

   function Execute (Object : Base_Type; Method : String) return Boolean;
   --  General access to a Method as a Boolean

   -------------------------------------------------------------------------
   --  Base_Type - Event Handlers
   -------------------------------------------------------------------------
   --  When an event handler is set on any event, binding code will be sent
   --  to the browser automatically for Gnoga to start receiving notifications
   --  of the event. In theory any event can be set on any object not all
   --  will be fired by every object.

   type Action_Event is access
     procedure (Object : in out Base_Type'Class);

   type Mouse_Message_Type is (Unknown, Click, Double_Click, Right_Click,
                               Mouse_Down, Mouse_Up, Mouse_Move);

   type Mouse_Event_Record is
      record
         Message       : Mouse_Message_Type := Unknown;
         X             : Integer;
         Y             : Integer;
         Screen_X      : Integer;
         Screen_Y      : Integer;
         Left_Button   : Boolean := False;
         Middle_Button : Boolean := False;
         Right_Button  : Boolean := False;
         Alt           : Boolean := False;
         Control       : Boolean := False;
         Shift         : Boolean := False;
         Meta          : Boolean := False;
      end record;

   type Mouse_Event is access
     procedure (Object      : in out Base_Type'Class;
                Mouse_Event : in     Mouse_Event_Record);

   type Keyboard_Message_Type is (Unknown, Key_Down, Key_Up, Key_Press);

   type Keyboard_Event_Record is
      record
         Message  : Keyboard_Message_Type := Unknown;
         Key_Code : Integer;
         Key_Char : Wide_Character;
         Alt      : Boolean := False;
         Control  : Boolean := False;
         Shift    : Boolean := False;
         Meta     : Boolean := False;
      end record;

   type Keyboard_Event is access
     procedure (Object         : in out Base_Type'Class;
                Keyboard_Event : in     Keyboard_Event_Record);

   type Character_Event is access
     procedure (Object : in out Base_Type'Class;
                Key    : in     Character);

   type Wide_Character_Event is access
     procedure (Object : in out Base_Type'Class;
                Key    : in     Wide_Character);

   type Message_Event is access
     procedure (Object   : in out Base_Type'Class;
                Event    : in     String;
                Message  : in     String;
                Continue : out    Boolean);

   type Child_Changed_Event is access
     procedure (Object : in out Base_Type'Class;
                Child  : in out Base_Type'Class);

   type Drop_Event is access
     procedure (Object    : in out Base_Type'Class;
                X, Y      : in     Integer;
                Drag_Text : in     String);

   -------------------------------------------------------------------------
   --  Base_Type - Event Internals
   -------------------------------------------------------------------------
   --  Event binding is usually used internally during On_Create or
   --  when setting a message handler. It can be used though to bind events
   --  not bound by Gnoga or custom events.

   procedure Bind_Event (Object  : in out Base_Type;
                         Event   : in     String;
                         Message : in     String;
                         Eval    : in     String    := "";
                         Script  : in     String    := "";
                         Cancel  : in     Boolean   := False);
   --  On Event occurring to Object Gnoga will fire Object.On_Message with
   --  Event and Message, the result of Script is concatenated to Message.
   --
   --  Eval if set is JavaScript to be run before processing the
   --  return message which is the result of ("Message|" + Script).
   --  The Eval script has access to the event "e" and an optional event
   --  parameter "data". The Eval script must be terminated with a ';' if
   --  not a block statement.
   --
   --  If Cancel is true then JS will cancel the default behavior of Event
   --  from occurring on browser. (e.g. stopping a form submit in onsubmit)

   procedure Bind_Event_Script (Object : in out Base_Type;
                                Event  : in     String;
                                Script : in     String);
   --  On Event occurring to Object, the Script will be executed on browser.

   procedure Unbind_Event (Object : in out Base_Type;
                           Event  : in     String);
   --  Unbind an event.

   procedure Attach_To_Message_Queue (Object : in out Base_Type);
   --  Attach Object to Message Queue

   procedure Detach_From_Message_Queue (Object : in out Base_Type);
   --  Detach Object from Message Queue

   function Script_Accessor (ID : String; ID_Type : ID_Enumeration)
                             return String;
   --  General utility for calculating te Script Accessor of an ID based on
   --  ID_Type

   function Script_Accessor (Object : Base_Type) return String;
   --  Returns the script representation for ID. For DOM_ID '#ID' for
   --  Script 'ID'

   function jQuery (Object : Base_Type) return String;
   --  Returns the jQuery selector for Object

   procedure jQuery_Execute (Object : in out Base_Type; Method : in String);
   function jQuery_Execute (Object : Base_Type; Method : String) return String;
   function jQuery_Execute (Object : Base_Type; Method : String)
                            return Integer;
   function jQuery_Execute (Object : Base_Type; Method : String)
                            return Float;
   --  Execute Method of jQuery wrapper object

   type Event_Info is record
      Event  : Ada.Strings.Unbounded.Unbounded_String;
      Object : Pointer_To_Base_Class;
      Data   : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package EQ_IF is new Ada.Containers.Synchronized_Queue_Interfaces (Element_Type => Event_Info);
   package Event_Queues is new Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interfaces => EQ_IF);

   Event_Queue : Event_Queues.Queue;

   -- Object Events --

   procedure On_Resize_Handler (Object  : in out Base_Type;
                                Handler : in     Action_Event);
   procedure Fire_On_Resize (Object : in out Base_Type);
   --  Handle object size change.

   procedure On_Scroll_Handler (Object  : in out Base_Type;
                                Handler : in     Action_Event);
   procedure Fire_On_Scroll (Object : in out Base_Type);
   --  Handle scroll changes.

   -- Form Events --

   procedure On_Focus_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event);
   procedure Fire_On_Focus (Object : in out Base_Type);
   --  Handle focus on object

   procedure On_Blur_Handler (Object  : in out Base_Type;
                              Handler : in     Action_Event);
   procedure Fire_On_Blur (Object : in out Base_Type);
   --  Handle loss of focus, many browsers poorly support this event.

   procedure On_Change_Handler (Object  : in out Base_Type;
                                Handler : in     Action_Event);
   procedure Fire_On_Change (Object : in out Base_Type);

   procedure On_Focus_In_Handler (Object  : in out Base_Type;
                                  Handler : in     Action_Event);
   procedure Fire_On_Focus_In (Object : in out Base_Type);

   procedure On_Focus_Out_Handler (Object  : in out Base_Type;
                                   Handler : in     Action_Event);
   procedure Fire_On_Focus_Out (Object : in out Base_Type);

   procedure On_Input_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event);
   procedure Fire_On_Input (Object : in out Base_Type);

   procedure On_Reset_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event);
   procedure Fire_On_Reset (Object : in out Base_Type);
   --  If this event is bound it will stop automatic reset of form contents
   --  Gnoga.Element.Form_Type.Reset must be called in the handler for the
   --  form to be reset if desired.

   procedure On_Search_Handler (Object  : in out Base_Type;
                                Handler : in     Action_Event);
   procedure Fire_On_Search (Object : in out Base_Type);

   procedure On_Select_Handler (Object  : in out Base_Type;
                                Handler : in     Action_Event);
   procedure Fire_On_Select (Object : in out Base_Type);

   procedure On_Submit_Handler (Object  : in out Base_Type;
                                Handler : in     Action_Event);
   procedure Fire_On_Submit (Object : in out Base_Type);
   --  If this event is bound it will stop automatic submission of a form
   --  Gnoga.Element.Form_Type.Submit must be called in the handler for
   --  the form to be submitted if desired.

   -- Mouse Events --

   procedure On_Click_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event);
   procedure Fire_On_Click (Object : in out Base_Type);
   --  Handle mouse click events

   procedure On_Mouse_Click_Handler (Object  : in out Base_Type;
                                     Handler : in     Mouse_Event);
   procedure Fire_On_Mouse_Click (Object : in out Base_Type;
                                  Event  : in     Mouse_Event_Record);
   --  Handle mouse click events, but also return mouse event data

   procedure On_Context_Menu_Handler (Object  : in out Base_Type;
                                      Handler : in     Action_Event);
   procedure Fire_On_Context_Menu (Object : in out Base_Type);
   --  Handle right mouse button click events

   procedure On_Mouse_Right_Click_Handler (Object  : in out Base_Type;
                                           Handler : in     Mouse_Event);
   procedure Fire_On_Mouse_Right_Click (Object : in out Base_Type;
                                        Event  : in     Mouse_Event_Record);
   --  Handle mouse right click event, but also return mouse event data

   procedure On_Double_Click_Handler (Object  : in out Base_Type;
                                      Handler : in     Action_Event);
   procedure Fire_On_Double_Click (Object : in out Base_Type);
   --  Handle mouse double click events

   procedure On_Mouse_Double_Click_Handler (Object  : in out Base_Type;
                                            Handler : in     Mouse_Event);
   procedure Fire_On_Mouse_Double_Click (Object : in out Base_Type;
                                         Event  : in     Mouse_Event_Record);
   --  Handle mouse double click events, but also return mouse event data

   procedure On_Mouse_Enter_Handler (Object  : in out Base_Type;
                                     Handler : in     Action_Event);
   procedure Fire_On_Mouse_Enter (Object : in out Base_Type);
   --  Handle mouse enter to object events

   procedure On_Mouse_Leave_Handler (Object  : in out Base_Type;
                                     Handler : in     Action_Event);
   procedure Fire_On_Mouse_Leave (Object : in out Base_Type);
   --  Handle mouse leave object events

   procedure On_Mouse_Over_Handler (Object  : in out Base_Type;
                                     Handler : in     Action_Event);
   procedure Fire_On_Mouse_Over (Object : in out Base_Type);
   --  Handle mouse enter to object events or it's children events

   procedure On_Mouse_Out_Handler (Object  : in out Base_Type;
                                   Handler : in     Action_Event);
   procedure Fire_On_Mouse_Out (Object : in out Base_Type);
   --  Handle mouse leave object events or it's children events

   procedure On_Mouse_Down_Handler (Object  : in out Base_Type;
                                    Handler : in     Mouse_Event);
   procedure Fire_On_Mouse_Down (Object : in out Base_Type;
                                 Event  : in     Mouse_Event_Record);
   --  Handle mouse down events

   procedure On_Mouse_Up_Handler (Object  : in out Base_Type;
                                  Handler : in     Mouse_Event);
   procedure Fire_On_Mouse_Up (Object : in out Base_Type;
                               Event  : in     Mouse_Event_Record);
   --  Handle mouse up events

   procedure On_Mouse_Move_Handler (Object  : in out Base_Type;
                                    Handler : in     Mouse_Event);
   procedure Fire_On_Mouse_Move (Object : in out Base_Type;
                                 Event  : in     Mouse_Event_Record);
   --  Handle mouse move events

   --  Drag and Drop Events --

   procedure On_Drag_Start_Handler (Object    : in out Base_Type;
                                    Handler   : in     Action_Event;
                                    Drag_Text : in     String;
                                    Drag_Type : in     String := "text/plain");
   procedure Fire_On_Drag_Start (Object : in out Base_Type);
   --  When setting the On_Drag_Start_Handler, Drag_Text is set to be the
   --  text that will be delivered in a Drag_Event to an On_Drop event.
   --  The Element_Type.Draggable property should also be true for Object.

   procedure On_Drag_Handler (Object  : in out Base_Type;
                              Handler : in     Action_Event);
   procedure Fire_On_Drag (Object : in out Base_Type);

   procedure On_Drag_End_Handler (Object  : in out Base_Type;
                                  Handler : in     Action_Event);
   procedure Fire_On_Drag_End (Object : in out Base_Type);

   procedure On_Drag_Enter_Handler (Object  : in out Base_Type;
                                  Handler : in     Action_Event);
   procedure Fire_On_Drag_Enter (Object : in out Base_Type);

   procedure On_Drag_Leave_Handler (Object  : in out Base_Type;
                                  Handler : in     Action_Event);
   procedure Fire_On_Drag_Leave (Object : in out Base_Type);

   procedure On_Drop_Handler (Object    : in out Base_Type;
                              Handler   : in     Drop_Event;
                              Drag_Type : in     String := "text/plain");
   procedure Fire_On_Drop (Object    : in out Base_Type;
                           X, Y      : in     Integer;
                           Drag_Text : in     String);
   --  To become a drop target an element only needs to handle On_Drop
   --  On_Drag_Enter and On_Drag_Leave can be used to show indication
   --  of "ready to receive" object.

   --  Keyboard Events --

   procedure On_Character_Handler (Object  : in out Base_Type;
                                   Handler : in     Character_Event);
   procedure Fire_On_Character (Object : in out Base_Type;
                                Key    : in     Character);

   procedure On_Wide_Character_Handler (Object  : in out Base_Type;
                                        Handler : in     Wide_Character_Event);
   procedure Fire_On_Wide_Character (Object : in out Base_Type;
                                     Key    : in     Wide_Character);

   procedure On_Key_Down_Handler (Object  : in out Base_Type;
                                  Handler : in     Keyboard_Event);
   procedure Fire_On_Key_Down (Object : in out Base_Type;
                               Event  : in     Keyboard_Event_Record);

   procedure On_Key_Up_Handler (Object  : in out Base_Type;
                                Handler : in     Keyboard_Event);
   procedure Fire_On_Key_Up (Object : in out Base_Type;
                             Event  : in     Keyboard_Event_Record);

   procedure On_Key_Press_Handler (Object  : in out Base_Type;
                                   Handler : in     Keyboard_Event);
   procedure Fire_On_Key_Press (Object : in out Base_Type;
                                Event  : in     Keyboard_Event_Record);

   --  Clipboard Events  --

   procedure On_Copy_Handler (Object  : in out Base_Type;
                              Handler : in     Action_Event);
   procedure Fire_On_Copy (Object : in out Base_Type);

   procedure On_Cut_Handler (Object  : in out Base_Type;
                             Handler : in     Action_Event);
   procedure Fire_On_Cut (Object : in out Base_Type);

   procedure On_Paste_Handler (Object  : in out Base_Type;
                               Handler : in     Action_Event);
   procedure Fire_On_Paste (Object : in out Base_Type);

   --  Generic Events --

   procedure On_Create_Handler (Object  : in out Base_Type;
                                Handler : in     Action_Event);
   procedure Fire_On_Create (Object : in out Base_Type);
   --  Called on creation of a new Gnoga object after attached to message
   --  queue.

   procedure On_Destroy_Handler (Object  : in out Base_Type;
                                 Handler : in     Action_Event);
   procedure Fire_On_Destroy (Object : in out Base_Type);
   --  Called before detaching Gnoga object from message queue during
   --  finalization of Object. As it is possible the connection has been
   --  broken, and the reason for the event, it is imperative to check if the
   --  connection is still valid before calling any Gnoga methods with in an
   --  On_Destroy event.

   procedure On_Child_Added_Handler (Object  : in out Base_Type;
                                     Handler : in     Child_Changed_Event);
   procedure Fire_On_Child_Added (Object : in out Base_Type;
                                  Child  : in out Base_Type'Class);

   procedure On_Child_Removed_Handler (Object  : in out Base_Type;
                                       Handler : in     Child_Changed_Event);
   procedure Fire_On_Child_Removed (Object : in out Base_Type;
                                    Child  : in out Base_Type'Class);

   procedure On_Message_Handler (Object  : in out Base_Type;
                                 Handler : in     Message_Event);
   procedure Fire_On_Message (Object   : in out Base_Type;
                              Event    : in     String;
                              Message  : in     String;
                              Continue : out    Boolean);
   --  Generic message event handler, if set is called before every event.
   --  If Continue is set to false, no more event processing will occur.

   -------------------------------------------------------------------------
   --  Base_Type - Event Methods
   -------------------------------------------------------------------------
   --  When overriding events, to ensure that the event handlers will still
   --  be executed and internal functionality of the event is handled
   --  properly, always call the base class event method.
   --
   --  Event Methods are always bound on creation of Gnoga object or do not
   --  require event binding.

   procedure On_Create (Object : in out Base_Type);
   --  Called on creation of a new Gnoga object after attached to message
   --  queue.

   procedure On_Destroy (Object : in out Base_Type);
   --  Called before detaching Gnoga object from message queue during
   --  finalization of Object.

   procedure On_Child_Removed (Object : in out Base_Type;
                               Child  : in out Base_Type'Class);
   --  Called when a Child's Parent changed and was claiming Object as its
   --  parent.

   procedure On_Resize (Object : in out Base_Type);
   --  Called by all sizing methods to inform Object it has changed size.

   procedure On_Child_Added (Object : in out Base_Type;
                             Child  : in out Base_Type'Class);
   --  Called when a Child is created claiming Object as its parent.

   procedure On_Message (Object  : in out Base_Type;
                         Event   : in     String;
                         Message : in     String);
   --  Called on receiving any message or event from browser.
private
   type Base_Type is new Ada.Finalization.Limited_Controlled with record
      Unique_ID     : Gnoga.Unique_ID      := No_Unique_ID;
      Web_ID        : Gnoga.Web_ID;
      ID_Type       : ID_Enumeration := No_ID;
      Connection_ID : Gnoga.Connection_ID  := No_Connection;
      Parent_Object : Pointer_To_Base_Class      := null;
      Is_Dynamic    : Boolean                    := False;
      In_Resize     : Boolean                    := False;

         --  Object Events
      On_Resize_Event             : Action_Event         := null;
      On_Scroll_Event             : Action_Event         := null;

         --  Form Events
      On_Focus_Event              : Action_Event         := null;
      On_Blur_Event               : Action_Event         := null;
      On_Change_Event             : Action_Event         := null;
      On_Focus_In_Event           : Action_Event         := null;
      On_Focus_Out_Event          : Action_Event         := null;
      On_Input_Event              : Action_Event         := null;
      On_Reset_Event              : Action_Event         := null;
      On_Search_Event             : Action_Event         := null;
      On_Select_Event             : Action_Event         := null;
      On_Submit_Event             : Action_Event         := null;

         --  Mouse Events
      On_Click_Event              : Action_Event         := null;
      On_Mouse_Click_Event        : Mouse_Event          := null;
      On_Mouse_Right_Click_Event  : Mouse_Event          := null;
      On_Context_Menu_Event       : Action_Event         := null;
      On_Double_Click_Event       : Action_Event         := null;
      On_Mouse_Double_Click_Event : Mouse_Event          := null;
      On_Mouse_Enter_Event        : Action_Event         := null;
      On_Mouse_Leave_Event        : Action_Event         := null;
      On_Mouse_Over_Event         : Action_Event         := null;
      On_Mouse_Out_Event          : Action_Event         := null;
      On_Mouse_Down_Event         : Mouse_Event          := null;
      On_Mouse_Up_Event           : Mouse_Event          := null;
      On_Mouse_Move_Event         : Mouse_Event          := null;

         --  Drag and Drop Events
      On_Drag_Start_Event         : Action_Event         := null;
      On_Drag_Event               : Action_Event         := null;
      On_Drag_End_Event           : Action_Event         := null;
      On_Drag_Enter_Event         : Action_Event         := null;
      On_Drag_Leave_Event         : Action_Event         := null;
      On_Drop_Event               : Drop_Event           := null;

         --  Keyboard Events
      On_Character_Event          : Character_Event      := null;
      On_Wide_Character_Event     : Wide_Character_Event := null;
      On_Key_Down_Event           : Keyboard_Event       := null;
      On_Key_Up_Event             : Keyboard_Event       := null;
      On_Key_Press_Event          : Keyboard_Event       := null;

         --  Clipboard Events
      On_Copy_Event               : Action_Event         := null;
      On_Cut_Event                : Action_Event         := null;
      On_Paste_Event              : Action_Event         := null;

         --  Generic Events
      On_Create_Event             : Action_Event         := null;
      On_Destroy_Event            : Action_Event         := null;
      On_Child_Added_Event        : Child_Changed_Event  := null;
      On_Child_Removed_Event      : Child_Changed_Event  := null;
      On_Message_Event            : Message_Event        := null;
   end record;
end Ada_GUI.Gnoga.Gui;
