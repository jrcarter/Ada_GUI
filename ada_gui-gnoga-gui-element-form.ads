-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                G N O G A . G U I . E L E M E N T . F O R M               --
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

package Ada_GUI.Gnoga.Gui.Element.Form is

   -------------------------------------------------------------------------
   --  Form_Types
   -------------------------------------------------------------------------

   type Form_Type is new Gnoga.Gui.View.View_Base_Type with private;
   type Form_Access is access all Form_Type;
   type Pointer_To_Form_Class is access all Form_Type'Class;

   -------------------------------------------------------------------------
   --  Form_Type - Creation Methods
   -------------------------------------------------------------------------

   type Form_Method_Type is (Get, Post);

   procedure Create (Form    : in out Form_Type;
                     Parent  : in out Gnoga.Gui.Base_Type'Class;
                     Action  : in     String           := "";
                     Method  : in     Form_Method_Type := Get;
                     Target  : in     String           := "_self";
                     ID      : in     String           := "");
   --  Create a Form element. This is used to group and set the action
   --  taken on a submit of the form.
   --
   --  In Gnoga forms in general should be processed by handling the
   --  On_Submit event and accessing each element's value directly.
   --  However it is possible to set an action (URL for form to submit to)
   --  and that page can process the results. For submitted forms access
   --  using Gnoga.Gui.Window.Window_Type.Form_Parameters.

   -------------------------------------------------------------------------
   --  From_Type - Properties
   -------------------------------------------------------------------------

   procedure Action (Form : in out Form_Type; Value : in String);
   function Action (Form : Form_Type) return String;
   --  URL to submit form to using Method

   procedure Method (Form : in out Form_Type; Value : in Form_Method_Type);
   function Method (Form : Form_Type) return Form_Method_Type;

   type Encoding_Type is (URL_Encode, Multi_Part, Text);
   --  application/x-www-form-urlencoded / multipart/form-data / text/plain
   --  The default for forms is URL_Encode

   procedure Encoding (Form : in out Form_Type; Value : in Encoding_Type);
   function Encoding (Form : Form_Type) return  Encoding_Type;
   --  Encoding affects how the form is send via the POST method only.

   procedure Autocomplete (Form  : in out Form_Type;
                            Value : in     Boolean := True);
   function Autocomplete (Form : Form_Type) return Boolean;

   procedure Validate_On_Submit (Form  : in out Form_Type;
                                 Value : in     Boolean := True);
   function Validate_On_Submit (Form : Form_Type) return Boolean;

   function Number_Of_Elements_In_Form (Form : Form_Type) return Integer;

   procedure Target (Form : in out Form_Type; Value : in String);
   function Target (Form : Form_Type) return String;
   --  Target of link, name (set as attribute on a frame or windows) of
   --  a frame or:
   --  _blank  = new window
   --  _top    = top most frame (full browser window)
   --  _parent = parent frame or window
   --  _self   = current frame or window

   -------------------------------------------------------------------------
   --  Form_Type - Methods
   -------------------------------------------------------------------------

   procedure Submit (Form : in out Form_Type);
   --  Submit form

   procedure Reset (Form : in out Form_Type);
   --  Reset form to original defaults

   -------------------------------------------------------------------------
   --  Form_Element_Types
   -------------------------------------------------------------------------
   --  Parent type for all form elements

   type Form_Element_Type is new Gnoga.Gui.Element.Element_Type with private;
   type Form_Element_Access is access all Form_Element_Type;
   type Pointer_To_Form_Element_Class is access all Form_Element_Type'Class;

   type Data_List_Type is new Gnoga.Gui.View.View_Base_Type with private;
   --  Forward declaration of Data_List_Type used for drop down /
   --  autocomplete.

   -------------------------------------------------------------------------
   --  Form_Element_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create_Element (Element    : in out Form_Element_Type;
                             Form       : in out Form_Type'Class;
                             Input_Type : in     String;
                             Value      : in     String := "";
                             Name       : in     String := "";
                             ID         : in     String := "");
   --  Create a form element of Input_Type. Setting Name on form element
   --  is only important if the form will be submitted via a GET or POST.
   --  Value is the initial value for the element.
   --
   --  Valid HTML5 Input_Types are:
   --     button, checkbox, color, date, datetime, datetime-local, email
   --     file, hidden, image, month, number, password, radio, range
   --     reset, search, submit, tel, text, time, url, week

   -------------------------------------------------------------------------
   --  Form_Element_Type - Properties
   -------------------------------------------------------------------------

   procedure Autocomplete (Element : in out Form_Element_Type;
                            Value   : in     Boolean := True);
   function Autocomplete (Element : Form_Element_Type) return Boolean;

   procedure Autofocus (Element : in out Form_Element_Type;
                         Value   : in     Boolean := True);
   function Autofocus (Element : Form_Element_Type) return Boolean;

   procedure Data_List (Element   : in out Form_Element_Type;
                        Data_List : in out Data_List_Type'Class);
   --  Set the data list for Autocomplete

   procedure Disabled (Element : in out Form_Element_Type;
                       Value   : in     Boolean := True);
   function Disabled (Element : Form_Element_Type) return Boolean;

   procedure Read_Only (Element : in out Form_Element_Type;
                        Value   : in     Boolean := True);
   function Read_Only (Element : Form_Element_Type) return Boolean;

   procedure Validate_On_Submit (Element : in out Form_Element_Type;
                                 Value   : in     Boolean := True);
   function Validate_On_Submit (Element : Form_Element_Type) return Boolean;

   procedure Name (Element : in out Form_Element_Type; Value : in String);
   function Name (Element : Form_Element_Type) return String;
   --  Form element name, name is not the id of the element but rather how
   --  the data returned from the element will be named in the submit to the
   --  server. For example if Name is My_Field a GET request could look like
   --  http://localhost:8080?My_Field=xxxx

   procedure Default_Value (Element : in out Form_Element_Type;
                            Value   : in     String);
   procedure Default_Value (Element : in out Form_Element_Type;
                            Value   : in     Integer);
   function Default_Value (Element : Form_Element_Type) return String;
   function Default_Value (Element : Form_Element_Type) return Integer;
   --  If the form is reset the value will be set to default value
   --  If Value is set at time of creation it also sets it as the Default_Value

   procedure Value (Element : in out Form_Element_Type; Value : in String);
   procedure Value (Element : in out Form_Element_Type; Value : in Integer);
   function Value (Element : Form_Element_Type) return String;
   function Value (Element : Form_Element_Type) return Integer;
   --  Form element values are not accessible through the Text property but
   --  instead through the value property.

   --  Text oriented properties

   procedure Place_Holder (Element : in out Form_Element_Type;
                           Value   : in     String);
   function Place_Holder (Element : Form_Element_Type) return String;

   procedure Pattern (Element : in out Form_Element_Type; Value : in String);
   function Pattern (Element : Form_Element_Type) return String;
   --  Form validation pattern. Validate_On_Submit fields with input
   --  will validate against their Pattern if set on submit.
   --  Pattern is included in Form_Element_Type since in cases where a specific
   --  input type is not supported like (date, week, etc.) Pattern can be set
   --  to ensure the expected results. This works since Input type will fall
   --  back to a text input.

   procedure Required (Element : in out Form_Element_Type;
                        Value   : in     Boolean := True);
   function Required (Element : Form_Element_Type) return Boolean;
   --  If Required is true on submit Element must be set/contain a value

   --  Range oriented inputs

   procedure Minimum (Element : in out Form_Element_Type; Value : in String);
   procedure Minimum (Element : in out Form_Element_Type; Value : in Integer);
   function Minimum (Element : Form_Element_Type) return String;
   function Minimum (Element : Form_Element_Type) return Integer;

   procedure Maximum (Element : in out Form_Element_Type; Value : in String);
   procedure Maximum (Element : in out Form_Element_Type; Value : in Integer);
   function Maximum (Element : Form_Element_Type) return String;
   function Maximum (Element : Form_Element_Type) return Integer;

   procedure Step (Element : in out Form_Element_Type; Value : in String);
   procedure Step (Element : in out Form_Element_Type; Value : in Integer);
   function Step (Element : Form_Element_Type) return String;
   function Step (Element : Form_Element_Type) return Integer;

   -------------------------------------------------------------------------
   --  Form_Element_Type - Methods
   -------------------------------------------------------------------------

   procedure Select_Text (Element : in out Form_Element_Type);
   --  Selects and highlights the context of Element

   -------------------------------------------------------------------------
   --  Data_List_Types
   -------------------------------------------------------------------------

   type Data_List_Access is access all Data_List_Type;
   type Pointer_To_Data_List_Class is access all Data_List_Type'Class;

   -------------------------------------------------------------------------
   --  Data_List_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (List          : in out Data_List_Type;
      Parent        : in out Gnoga.Gui.Base_Type'Class;
      ID            : in     String  := "");

   -------------------------------------------------------------------------
   --  Data_List_Type - Methods
   -------------------------------------------------------------------------

   procedure Add_Option (List  : in out Data_List_Type;
                         Value : in     String);
   --  Add option to Data_List

   -------------------------------------------------------------------------
   --  Text_Area_Types
   -------------------------------------------------------------------------

   type Text_Area_Type is new Form_Element_Type with private;
   type Text_Area_Access is access all Text_Area_Type;
   type Pointer_To_Text_Area_Class is access all Text_Area_Type'Class;

   -------------------------------------------------------------------------
   --  Text_Area_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Text_Area_Type;
                     Form       : in out Form_Type'Class;
                     Columns    : in     Natural := 20;
                     Rows       : in     Natural := 2;
                     Value      : in     String  := "";
                     Name       : in     String  := "";
                     ID         : in     String  := "");

   -------------------------------------------------------------------------
   --  Text_Area_Type - Properties
   -------------------------------------------------------------------------

   procedure Word_Wrap (Element : in out Text_Area_Type;
                        Value   : in      Boolean := True);
   function Word_Wrap (Element : Text_Area_Type) return Boolean;

   procedure Columns (Element : in out Text_Area_Type;
                      Value   : in     Integer);
   function Columns (Element : Text_Area_Type) return Integer;

   procedure Rows (Element : in out Text_Area_Type;
                      Value   : in     Integer);
   function Rows (Element : Text_Area_Type) return Integer;

   -------------------------------------------------------------------------
   --  Hidden_Types
   -------------------------------------------------------------------------

   type Hidden_Type is new Form_Element_Type with private;
   type Hidden_Access is access all Hidden_Type;
   type Pointer_To_Hidden_Class is access all Hidden_Type'Class;

   -------------------------------------------------------------------------
   --  Hidden_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Hidden_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "");

   -------------------------------------------------------------------------
   --  Input_Button_Types
   -------------------------------------------------------------------------

   type Input_Button_Type is new Form_Element_Type with private;
   type Input_Button_Access is access all Input_Button_Type;
   type Pointer_To_Input_Button_Class is access all Input_Button_Type'Class;

   -------------------------------------------------------------------------
   --  Input_Button_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Input_Button_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "");

   -------------------------------------------------------------------------
   --  Submit_Button_Types
   -------------------------------------------------------------------------
   --  An Input Button that On_Click will fire On_Submit

   type Submit_Button_Type is new Input_Button_Type with private;
   type Submit_Button_Access is access all Submit_Button_Type;
   type Pointer_To_Submit_Button_Class is access all Submit_Button_Type'Class;

   -------------------------------------------------------------------------
   --  Submit_Button_Type - Creation Methods
   -------------------------------------------------------------------------

   overriding
   procedure Create (Element    : in out Submit_Button_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "");

   -------------------------------------------------------------------------
   --  Reset_Button_Types
   -------------------------------------------------------------------------
   --  An Input Button that On_Click will fire On_Reset

   type Reset_Button_Type is new Input_Button_Type with private;
   type Reset_Button_Access is access all Reset_Button_Type;
   type Pointer_To_Reset_Button_Class is access all Reset_Button_Type'Class;

   -------------------------------------------------------------------------
   --  Reset_Button_Type - Creation Methods
   -------------------------------------------------------------------------

   overriding
   procedure Create (Element    : in out Reset_Button_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "");

   -------------------------------------------------------------------------
   --  Check_Box_Types
   -------------------------------------------------------------------------

   type Check_Box_Type is new Form_Element_Type with private;
   type Check_Box_Access is access all Check_Box_Type;
   type Pointer_To_Check_Box_Class is access all Check_Box_Type'Class;

   -------------------------------------------------------------------------
   --  Check_Box_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Check_Box_Type;
                     Form       : in out Form_Type'Class;
                     Checked    : in     Boolean := False;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "");
   --  Value will be what is submitted if Checked is true for Name.

   -------------------------------------------------------------------------
   --  Check_Box_Type - Properties
   -------------------------------------------------------------------------

   procedure Checked (Element : in out Check_Box_Type;
                        Value   : in     Boolean := True);
   function Checked (Element : Check_Box_Type) return Boolean;

   procedure Indeterminate (Element : in out Check_Box_Type;
                        Value   : in     Boolean := True);
   function Indeterminate (Element : Check_Box_Type) return Boolean;

   -------------------------------------------------------------------------
   --  Radio_Button_Types
   -------------------------------------------------------------------------

   type Radio_Button_Type is new Form_Element_Type with private;
   type Radio_Button_Access is access all Radio_Button_Type;
   type Pointer_To_Radio_Button_Class is access all Radio_Button_Type'Class;

   --  These radio buttons will operate as groups based on having a common
   --  Name attribute.

   -------------------------------------------------------------------------
   --  Radio_Button_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Radio_Button_Type;
                     Form       : in out Form_Type'Class;
                     Checked    : in     Boolean := False;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "");

   ----------------------------------------------------------
   --  Radio_Button_Type - Properties
   -------------------------------------------------------------------------

   procedure Checked (Element : in out Radio_Button_Type;
                        Value   : in     Boolean := True);
   function Checked (Element : Radio_Button_Type) return Boolean;

   -------------------------------------------------------------------------
   --  Input_Image_Types
   -------------------------------------------------------------------------

   type Input_Image_Type is new Form_Element_Type with private;
   type Input_Image_Access is access all Input_Image_Type;
   type Pointer_To_Input_Image_Class is access all Input_Image_Type'Class;

   -------------------------------------------------------------------------
   --  Input_Image_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Input_Image_Type;
                     Form       : in out Form_Type'Class;
                     Source     : in     String    := "";
                     Value      : in     String    := "";
                     Name       : in     String    := "";
                     ID         : in     String    := "");

   -------------------------------------------------------------------------
   --  Input_Image_Type - Properties
   -------------------------------------------------------------------------

   procedure Source (Element : in out Input_Image_Type; Value : String);
   function Source (Element : Input_Image_Type) return String;
   --  URL source for image

   -------------------------------------------------------------------------
   --  Text_Types
   -------------------------------------------------------------------------

   type Text_Type is new Form_Element_Type with private;
   type Text_Access is access all Text_Type;
   type Pointer_To_Text_Class is access all Text_Type'Class;

   -------------------------------------------------------------------------
   --  Text_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Text_Type;
                     Form       : in out Form_Type'Class;
                     Size       : in     Integer   := 20;
                     Value      : in     String    := "";
                     Name       : in     String    := "";
                     ID         : in     String    := "");

   -------------------------------------------------------------------------
   --  Text_Type - Properties
   -------------------------------------------------------------------------

   procedure Size (Element : in out Text_Type; Value : Integer);
   function Size (Element : Text_Type) return Integer;
   --  Length of visible field in characters

   procedure Max_Length (Element : in out Text_Type; Value : Integer);
   function Max_Length (Element : Text_Type) return Integer;
   --  Maximum length of Value

   -------------------------------------------------------------------------
   --  Email_Types
   -------------------------------------------------------------------------

   type Email_Type is new Text_Type with private;
   type Email_Access is access all Email_Type;
   type Pointer_To_Email_Class is access all Email_Type'Class;

   -------------------------------------------------------------------------
   --  Email_Type - Creation Methods
   -------------------------------------------------------------------------

   overriding
   procedure Create (Element    : in out Email_Type;
                     Form       : in out Form_Type'Class;
                     Size       : in     Integer   := 20;
                     Value      : in     String    := "";
                     Name       : in     String    := "";
                     ID         : in     String    := "");

   -------------------------------------------------------------------------
   --  Email_Type - Properties
   -------------------------------------------------------------------------

   procedure Multiple_Emails (Element : in out Email_Type;
                              Value   : in     Boolean := True);
   function Multiple_Emails (Element : Email_Type) return Boolean;

   -------------------------------------------------------------------------
   --  Password_Types
   -------------------------------------------------------------------------

   type Password_Type is new Text_Type with private;
   type Password_Access is access all Password_Type;
   type Pointer_To_Password_Class is access all Password_Type'Class;

   -------------------------------------------------------------------------
   --  Password_Type - Creation Methods
   -------------------------------------------------------------------------

   overriding
   procedure Create (Element    : in out Password_Type;
                     Form       : in out Form_Type'Class;
                     Size       : in     Integer   := 20;
                     Value      : in     String    := "";
                     Name       : in     String    := "";
                     ID         : in     String    := "");

   -------------------------------------------------------------------------
   --  URL_Types
   -------------------------------------------------------------------------

   type URL_Type is new Text_Type with private;
   type URL_Access is access all URL_Type;
   type Pointer_To_URL_Class is access all URL_Type'Class;

   -------------------------------------------------------------------------
   --  URL_Type - Creation Methods
   -------------------------------------------------------------------------

   overriding
   procedure Create (Element    : in out URL_Type;
                     Form       : in out Form_Type'Class;
                     Size       : in     Integer   := 20;
                     Value      : in     String    := "";
                     Name       : in     String    := "";
                     ID         : in     String    := "");

   -------------------------------------------------------------------------
   --  Search_Types
   -------------------------------------------------------------------------

   type Search_Type is new Text_Type with private;
   type Search_Access is access all Search_Type;
   type Pointer_To_Search_Class is access all Search_Type'Class;

   -------------------------------------------------------------------------
   --  Search_Type - Creation Methods
   -------------------------------------------------------------------------

   overriding
   procedure Create (Element    : in out Search_Type;
                     Form       : in out Form_Type'Class;
                     Size       : in     Integer   := 20;
                     Value      : in     String    := "";
                     Name       : in     String    := "";
                     ID         : in     String    := "");

   -------------------------------------------------------------------------
   --  Color_Picker_Types
   -------------------------------------------------------------------------

   type Color_Picker_Type is new Form_Element_Type with private;
   type Color_Picker_Access is access all Color_Picker_Type;
   type Pointer_To_Color_Picker_Class is access all Color_Picker_Type'Class;

   -------------------------------------------------------------------------
   --  Color_Picker_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Color_Picker_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "");
   procedure Create (Element    : in out Color_Picker_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     Gnoga.RGBA_Type;
                     Name       : in     String := "";
                     ID         : in     String := "");
   procedure Create (Element    : in out Color_Picker_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     Gnoga.Colors.Color_Enumeration;
                     Name       : in     String := "";
                     ID         : in     String := "");

   -------------------------------------------------------------------------
   --  Color_Picker_Type - Properties
   -------------------------------------------------------------------------

   overriding
   procedure Color (Element : in out Color_Picker_Type;
                    Value   : in     String);
   overriding
   procedure Color (Element : in out Color_Picker_Type;
                    RGBA    : in     Gnoga.RGBA_Type);
   overriding
   procedure Color (Element : in out Color_Picker_Type;
                    Enum    : in     Gnoga.Colors.Color_Enumeration);
   overriding
   function Color (Element : Color_Picker_Type) return Gnoga.RGBA_Type;

   -------------------------------------------------------------------------
   --  Date_Types
   -------------------------------------------------------------------------

   type Date_Type is new Form_Element_Type with private;
   type Date_Access is access all Date_Type;
   type Pointer_To_Date_Class is access all Date_Type'Class;

   -------------------------------------------------------------------------
   --  Date_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Date_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "");
   --  Value format yyyy-mm-dd for Date_Type

   -------------------------------------------------------------------------
   --  Time_Types
   -------------------------------------------------------------------------

   type Time_Type is new Form_Element_Type with private;
   type Time_Access is access all Time_Type;
   type Pointer_To_Time_Class is access all Time_Type'Class;

   -------------------------------------------------------------------------
   --  Time_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Time_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "");
   --  Value format HH:MM no time zone 24hour format

   -------------------------------------------------------------------------
   --  Month_Types
   -------------------------------------------------------------------------

   type Month_Type is new Form_Element_Type with private;
   type Month_Access is access all Month_Type;
   type Pointer_To_Month_Class is access all Month_Type'Class;

   -------------------------------------------------------------------------
   --  Month_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Month_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "");
   --  Value format yyyy-mm for Month_Type
   --  Months are 1-12

   -------------------------------------------------------------------------
   --  Week_Types
   -------------------------------------------------------------------------

   type Week_Type is new Form_Element_Type with private;
   type Week_Access is access all Week_Type;
   type Pointer_To_Week_Class is access all Week_Type'Class;

   -------------------------------------------------------------------------
   --  Week_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Week_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "");
   --  Value format yyyy-Www for Week_Type
   --  Date with the year and a W followed by the week number, with no time
   --  zone. A "week" goes from Monday to Sunday, with week 1 being the week
   --  containing the first Wednesday of the year, so could start on December
   --  30 or even January 2.

   -------------------------------------------------------------------------
   --  Date_Time_Types
   -------------------------------------------------------------------------

   type Date_Time_Type is new Form_Element_Type with private;
   type Date_Time_Access is access all Date_Time_Type;
   type Pointer_To_Date_Time_Class is access all Date_Time_Type'Class;

   -------------------------------------------------------------------------
   --  Date_Time_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Date_Time_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "");
   --  Value format yyyy-mm-ddTHH:MMZ
   --  Hour, minute, second, and fraction of a second based on UTC time zone

   -------------------------------------------------------------------------
   --  Date_Time_Local_Types
   -------------------------------------------------------------------------

   type Date_Time_Local_Type is new Form_Element_Type with private;
   type Date_Time_Local_Access is access all Date_Time_Local_Type;
   type Pointer_To_Date_Time_Local_Class is
     access all Date_Time_Local_Type'Class;

   -------------------------------------------------------------------------
   --  Date_Time_Local_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Date_Time_Local_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "");
   --  Value format yyyy-mm-ddTHH:MMZ
   --  Hour, minute, second, and fraction of a second based on UTC time zone

   -------------------------------------------------------------------------
   --  Number_Types
   -------------------------------------------------------------------------

   type Number_Type is new Form_Element_Type with private;
   type Number_Access is access all Number_Type;
   type Pointer_To_Number_Class is access all Number_Type'Class;

   -------------------------------------------------------------------------
   --  Number_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Number_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "");

   -------------------------------------------------------------------------
   --  Range_Types
   -------------------------------------------------------------------------

   type Range_Type is new Number_Type with private;
   type Range_Access is access all Range_Type;
   type Pointer_To_Range_Class is access all Range_Type'Class;

   -------------------------------------------------------------------------
   --  Range_Type - Creation Methods
   -------------------------------------------------------------------------

   overriding
   procedure Create (Element    : in out Range_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "");

   -------------------------------------------------------------------------
   --  Label_Types
   -------------------------------------------------------------------------

   type Label_Type is new Gnoga.Gui.Element.Element_Type with private;
   type Label_Access is access all Label_Type;
   type Pointer_To_Label_Class is access all Label_Type'Class;

   -------------------------------------------------------------------------
   --  Label_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Label_Type;
                     Form       : in out Form_Type'Class;
                     Label_For  : in out Element_Type'Class;
                     Content    : in     String  := "";
                     Auto_Place : in     Boolean := True;
                     ID         : in     String  := "");
   --  Creates a Label_For a form element in Form with Content as label.
   --  If Auto_Place is true, the label element will be moved in the DOM
   --  to just before Label_For.

   -------------------------------------------------------------------------
   --  Selection_Types
   -------------------------------------------------------------------------
   --  As an alternative to using the Add_Option method you can add
   --  as children Option_Type and Option_Group_Types. Option_Type
   --  and Option_Group_Type are added with Item.Create, they can
   --  be removed using Item.Remove and their order can be changed using
   --  the standard Element_Type.Place_* methods.

   type Selection_Type is new Form_Element_Type with private;
   type Selection_Access is access all Selection_Type;
   type Pointer_To_Selection_Class is access all Selection_Type'Class;

   -------------------------------------------------------------------------
   --  Selection_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Element         : in out Selection_Type;
      Form            : in out Form_Type'Class;
      Multiple_Select : in     Boolean  := False;
      Visible_Lines   : in     Positive := 1;
      Name            : in     String   := "";
      ID              : in     String   := "");
   --  If Parent is a Window_Type'Class will automatically set itself
   --  as the View on Parent if Attach is True

   -------------------------------------------------------------------------
   --  Selection_Type - Properties
   -------------------------------------------------------------------------

   procedure Multiple_Select (Element : in out Selection_Type;
                              Value   : in    Boolean := True);
   function Multiple_Select (Element : Selection_Type) return Boolean;

   procedure Visible_Lines (Element : in out Selection_Type;
                            Value   : in     Positive);
   function Visible_Lines (Element : Selection_Type) return Positive;

   function Selected_Index (Element : Selection_Type) return Natural;
   --  If no item currently selected returns 0, in multiple select boxes
   --  traverse the options using Selected.

   overriding
   function Value (Element : Selection_Type) return String;
   --  Returns the value of the currently selected item. For multiple select
   --  boxes get the value based on Index. The Value is the non-displayed
   --  value of the currently selected item.

   function Length (Element : Selection_Type) return Natural;
   --  Number of options in Selection_Type

   procedure Selected (Element : in out Selection_Type;
                       Index   : in     Positive;
                       Value   : in     Boolean := True);
   function Selected (Element : Selection_Type; Index : Positive)
                      return Boolean;

   procedure Disabled (Element : in out Selection_Type;
                       Index   : in     Positive;
                       Value   : in     Boolean := True);
   function Disabled (Element : Selection_Type; Index : Positive)
                      return Boolean;

   procedure Value (Element : in out Selection_Type;
                    Index   : in     Positive;
                    Value   : in     String);
   function Value (Element : Selection_Type; Index : Positive)
                   return String;
   --  Value is the non-displayed value of the the item at Index. If the form
   --  is submitted, the value not the "Text" is sent to the form's action
   --  URL.

   procedure Text (Element : in out Selection_Type;
                   Index   : in     Positive;
                   Value   : in     String);
   function Text (Element : Selection_Type; Index : Positive)
                  return String;
   --  The displayed text of the item at Index. The text is not submitted by
   --  the form only its value if the form is submitted to its action URL.

   -------------------------------------------------------------------------
   --  Selection_Type - Methods
   -------------------------------------------------------------------------

   procedure Add_Option (Element  : in out Selection_Type;
                         Value    : in     String;
                         Text     : in     String;
                         Index    : in     Natural := 0;
                         Selected : in     Boolean := False;
                         Disabled : in     Boolean := False;
                         ID       : in     String  := "");
   --  Call to add options to the Selection_Type Element. If Index is 0
   --  adds to end of list. Otherwise inserts at Index.

   procedure Remove_Option (Element  : in out Selection_Type;
                            Index    : in     Positive);

   procedure Empty_Options (Element  : in out Selection_Type);

   -------------------------------------------------------------------------
   --  Option_Type
   -------------------------------------------------------------------------

   type Option_Type is new Gnoga.Gui.Element.Element_Type with private;
   type Option_Access is access all Option_Type;
   type Pointer_To_Option_Class is access all Option_Type'Class;

   -------------------------------------------------------------------------
   --  Option_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Element   : in out Option_Type;
                     Form      : in out Form_Type'Class;
                     Selection : in out Gnoga.Gui.Element.Element_Type'Class;
                     Value     : in     String;
                     Text      : in     String;
                     Selected  : in     Boolean := False;
                     Disabled  : in     Boolean := False;
                     ID        : in     String := "");
   --  Creates an Option for Selection, this is an alternative to using
   --  Selection_Type.Add_Option that allows attaching events to the option.
   --  Depending on browser, some styling may be possible also.
   --  Will be added to end of Selection, the placement can be changed using
   --  standard Element_Type.Place_* methods and can also be removed using
   --  Element.Remove
   --  Selection can be an element of Selection_Type or Option_Group_Type
   --  NOTE: A few browsers allow limited styling of Element such as color
   --        some do not allow any. Test across your expected delivery
   --        platforms to insure the styling you use works as expected.

   -------------------------------------------------------------------------
   --  Option_Type - Properties
   -------------------------------------------------------------------------

   procedure Selected (Element : in out Option_Type;
                       Value   : in     Boolean := True);
   function Selected (Element : Option_Type)
                      return Boolean;

   procedure Disabled (Element : in out Option_Type;
                       Value   : in     Boolean := True);
   function Disabled (Element : Option_Type)
                      return Boolean;

   procedure Value (Element : in out Option_Type;
                    Value   : in     String);
   function Value (Element : Option_Type)
                   return String;

   overriding
   procedure Text (Element : in out Option_Type;
                   Value   : in     String);
   overriding
   function Text (Element : Option_Type)
                  return String;

   -------------------------------------------------------------------------
   --  Option_Group_Type
   -------------------------------------------------------------------------

   type Option_Group_Type is new Gnoga.Gui.Element.Element_Type with private;
   type Option_Group_Access is access all Option_Group_Type;
   type Pointer_To_Option_Group_Class is access all Option_Group_Type'Class;

   -------------------------------------------------------------------------
   --  Option_Group_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Element   : in out Option_Group_Type;
                     Form      : in out Form_Type'Class;
                     Selection : in out Selection_Type'Class;
                     Label     : in     String;
                     Disabled  : in     Boolean := False;
                     ID        : in     String := "");
   --  Option Groups create a tree like selection hierarchy in the Select.
   --  once added

   -------------------------------------------------------------------------
   --  Option_Group_Type - Properties
   -------------------------------------------------------------------------

   procedure Disabled (Element : in out Option_Group_Type;
                       Value   : in     Boolean := True);
   function Disabled (Element : Option_Group_Type)
                      return Boolean;

   procedure Label (Element : in out Option_Group_Type;
                    Value   : in     String);
   function Label (Element : Option_Group_Type)
                   return String;

   -------------------------------------------------------------------------
   --  File_Types
   -------------------------------------------------------------------------

   type File_Type is new Form_Element_Type with private;
   type File_Access is access all File_Type;
   type Pointer_To_File_Class is access all File_Type'Class;

   -------------------------------------------------------------------------
   --  File_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Element    : in out File_Type;
                     Form       : in out Form_Type'Class;
                     Multiple   : in     Boolean := False;
                     Name       : in     String  := "";
                     ID         : in     String  := "");

   -------------------------------------------------------------------------
   --  File_Type - Properties
   -------------------------------------------------------------------------

   procedure Accept_List (Element : in out File_Type;
                          Value   : in     String);
   function Accept_List (Element : File_Type)
                         return String;
   --  One or more unique file type specifiers describing file types to allow

   procedure Capture (Element : in out File_Type;
                      Value   : in     String);
   function Capture (Element : File_Type)
                     return String;
   --  What source to use for capturing image or video data

   procedure Multiple (Element : in out File_Type;
                       Value   : in     Boolean := True);
   function Multiple (Element : File_Type)
                      return Boolean;
   --  A Boolean which, if present, indicates that the user may choose more than one file

   procedure WebkitDirectory (Element : in out File_Type;
                              Value   : in     Boolean := True);
   function WebkitDirectory (Element : File_Type)
                             return Boolean;
   --  A Boolean indicating whether or not to only allow the user to choose a directory
   --  (or directories, if multiple is also present)

   function File_Count (Element : File_Type) return Natural;
   --  Return the number of selected files

   function File_Name (Element : File_Type; Index : Positive := 1) return String;
   --  Return the name of the specified file by its index

   function File_Size (Element : File_Type; Index : Positive := 1) return Natural;
   --  Return the size of the specified file by its index

   function File_MIME_Type (Element : File_Type; Index : Positive := 1) return String;
   --  Return the MIME type of the specified file by its index

   function File_Last_Modified (Element : File_Type; Index : Positive := 1) return Natural;
   --  Return the last modification time in millisecond of the specified file by its index

   function File_WebkitRelativePath (Element : File_Type; Index : Positive := 1) return String;
   --  Return the path of the file is relative to.

   -------------------------------------------------------------------------
   --  Tel_Types
   -------------------------------------------------------------------------

   type Tel_Type is new Form_Element_Type with private;
   type Tel_Access is access all Tel_Type;
   type Pointer_To_Tel_Class is access all Tel_Type'Class;

   -------------------------------------------------------------------------
   --  Tel_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Element    : in out Tel_Type;
                     Form       : in out Form_Type'Class;
                     Value      : in     String := "";
                     Name       : in     String := "";
                     ID         : in     String := "");

private
   type Form_Type is new Gnoga.Gui.View.View_Base_Type with null record;
   type Form_Element_Type is
     new Gnoga.Gui.Element.Element_Type with null record;
   type Text_Area_Type is new Form_Element_Type with null record;
   type Hidden_Type is new Form_Element_Type with null record;
   type Input_Image_Type is new Form_Element_Type with null record;
   type Input_Button_Type is new Form_Element_Type with null record;
   type Submit_Button_Type is new Input_Button_Type with null record;
   type Reset_Button_Type is new Input_Button_Type with null record;
   type Text_Type is new Form_Element_Type with null record;
   type Email_Type is new Text_Type with null record;
   type Password_Type is new Text_Type with null record;
   type Search_Type is new Text_Type with null record;
   type URL_Type is new Text_Type with null record;
   type Check_Box_Type is new Form_Element_Type with null record;
   type Radio_Button_Type is new Form_Element_Type with null record;
   type Color_Picker_Type is new Form_Element_Type with null record;
   type Date_Type is new Form_Element_Type with null record;
   type Time_Type is new Form_Element_Type with null record;
   type Month_Type is new Form_Element_Type with null record;
   type Week_Type is new Form_Element_Type with null record;
   type Date_Time_Type is new Form_Element_Type with null record;
   type Date_Time_Local_Type is new Form_Element_Type with null record;
   type Number_Type is new Form_Element_Type with null record;
   type Range_Type is new Number_Type with null record;
   type Label_Type is new Gnoga.Gui.Element.Element_Type with null record;
   type Data_List_Type is new Gnoga.Gui.View.View_Base_Type with null record;
   type Selection_Type is new Form_Element_Type with null record;
   type Option_Type is new Gnoga.Gui.Element.Element_Type with null record;
   type Option_Group_Type is
     new Gnoga.Gui.Element.Element_Type with null record;
   type File_Type is new Form_Element_Type with null record;
   type Tel_Type is new Form_Element_Type with null record;
end Ada_GUI.Gnoga.Gui.Element.Form;
