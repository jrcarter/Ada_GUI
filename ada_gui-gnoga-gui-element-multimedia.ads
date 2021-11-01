-- Ada_GUI implementation based on Gnoga. Adapted 2021
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--         G N O G A . G U I . E L E M E N T . M U L T I M E D I A          --
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

package Ada_GUI.Gnoga.Gui.Element.Multimedia is

   -------------------------------------------------------------------------
   --  Multimedia_Types
   -------------------------------------------------------------------------
   --  Base type for multimedia Elements

   type Multimedia_Type is new Gnoga.Gui.Element.Element_Type with private;
   type Multimedia_Access is access all Multimedia_Type;
   type Pointer_To_Multimedia_Class is access all Multimedia_Type'Class;

   -------------------------------------------------------------------------
   --  Media_Type - Properties
   -------------------------------------------------------------------------

   procedure Loop_Media (Media : in out Multimedia_Type; Value : in Boolean);
   function Loop_Media (Media : Multimedia_Type) return Boolean;

   function Media_Duration (Media : Multimedia_Type) return Float;
   --  Returns the duration of Media in seconds.

   procedure Media_Source (Media : in out Multimedia_Type; Source : in String);
   function Media_Source (Media : Multimedia_Type) return String;
   --  Returns the URL of the current Media

   procedure Media_Position (Media   : in out Multimedia_Type;
                             Seconds : in     Float);
   function Media_Position (Media : Multimedia_Type) return Float;
   --  Position of Media in seconds

   procedure Muted (Media : in out Multimedia_Type; Value : in Boolean);
   function Muted (Media : Multimedia_Type) return Boolean;

   function Paused (Media : Multimedia_Type) return Boolean;

   function Playback_Ended (Media : Multimedia_Type) return Boolean;
   --  Returns true of Media position has reached end of its duration

   procedure Playback_Rate (Media : in out Multimedia_Type; Value : in Float);
   function Playback_Rate (Media : Multimedia_Type) return Float;
   --  Playback rate.
   --  Common values - 1.0 normal, 0.5 half speed, -1.0 reverse

   function Ready_To_Play (Media : Multimedia_Type) return Boolean;
   --  True if media is ready to be played in element

   function Seeking (Media : Multimedia_Type) return Boolean;
   --  True if user is seeking through media

   subtype Volume_Range is Float range 0.0 .. 1.0;

   procedure Volume (Media : in out Multimedia_Type; Value : Volume_Range);
   function Volume (Media : Multimedia_Type) return Volume_Range;
   --  Media volume (not system volume) in Volume_Range

   -------------------------------------------------------------------------
   --  Media_Type - Methods
   -------------------------------------------------------------------------

   procedure Play (Media : in out Multimedia_Type);

   procedure Pause (Media : in out Multimedia_Type);

   procedure Load (Media : in out Multimedia_Type);
   --  Loads or reloads media

   function Can_Play (Media : Multimedia_Type; Media_Type : String)
                      return Boolean;
   --  Returns true if browser claims support of a media type. Browsers
   --  report possibility but not guarantees of being able to support a
   --  media type.
   --
   --  Common values:
   --    video/ogg
   --    video/mp4
   --    video/webm
   --    audio/mpeg
   --    audio/ogg
   --    audio/mp4
   --    audio/mp3
   --
   --  Common values, including codecs:
   --    video/ogg; codecs="theora, vorbis"
   --    video/mp4; codecs="avc1.4D401E, mp4a.40.2"
   --    video/webm; codecs="vp8.0, vorbis"
   --    audio/ogg; codecs="vorbis"
   --    audio/mp4; codecs="mp4a.40.5"

   -------------------------------------------------------------------------
   --  Media_Type - Event Handlers
   -------------------------------------------------------------------------

   --  The standard event order for a normal file load is:
   --    On_Load_Start
   --    On_Duration_Change
   --    On_Loaded_Meta_Data
   --    On_Loaded_Data
   --    On_Progress
   --    On_Can_Play
   --    On_Can_Play_Though

   procedure On_Media_Abort_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event);
   procedure Fire_On_Media_Abort (Media : in out Multimedia_Type);
   --  Media download aborted

   procedure On_Media_Error_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event);
   procedure Fire_On_Media_Error (Media : in out Multimedia_Type);
   --  Error loading media

   procedure On_Can_Play_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event);
   procedure Fire_On_Can_Play (Media : in out Multimedia_Type);
   --  Media ready to start playing

   procedure On_Can_Play_Through_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event);
   procedure Fire_On_Can_Play_Through (Media : in out Multimedia_Type);
   --  Browser believes it will not need to buffer any further and can play
   --  through to end with out stalling

   procedure On_Duration_Change_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event);
   procedure Fire_On_Duration_Change (Media : in out Multimedia_Type);
   --  The full duration of the media loading known

   procedure On_Emptied_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event);
   procedure Fire_On_Emptied (Media : in out Multimedia_Type);
   --  Playlist is emptied

   procedure On_Ended_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event);
   procedure Fire_On_Ended (Media : in out Multimedia_Type);
   --  Playlist is ended

   procedure On_Loaded_Data_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event);
   procedure Fire_On_Loaded_Data (Media : in out Multimedia_Type);
   --  Current media frame loaded

   procedure On_Loaded_Meta_Data_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event);
   procedure Fire_On_Loaded_Meta_Data (Media : in out Multimedia_Type);
   --  Meta data loaded

   procedure On_Load_Start_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event);
   procedure Fire_On_Load_Start (Media : in out Multimedia_Type);
   --  Connection attempt to media

   procedure On_Pause_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event);
   procedure Fire_On_Pause (Media : in out Multimedia_Type);

   procedure On_Play_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event);
   procedure Fire_On_Play (Media : in out Multimedia_Type);

   procedure On_Playing_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event);
   procedure Fire_On_Playing (Media : in out Multimedia_Type);

   procedure On_Progress_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event);
   procedure Fire_On_Progress (Media : in out Multimedia_Type);
   --  Downloading media

   procedure On_Rate_Change_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event);
   procedure Fire_On_Rate_Change (Media : in out Multimedia_Type);
   --  Play speed changed of media

   procedure On_Seeked_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event);
   procedure Fire_On_Seeked (Media : in out Multimedia_Type);
   --  User done seeking

   procedure On_Seeking_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event);
   procedure Fire_On_Seeking (Media : in out Multimedia_Type);
   --  User seeking in media

   procedure On_Stalled_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event);
   procedure Fire_On_Stalled (Media : in out Multimedia_Type);
   --  Media data download stalled

   procedure On_Suspend_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event);
   procedure Fire_On_Suspend (Media : in out Multimedia_Type);
   --  Download has completed or been paused

   procedure On_Time_Update_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event);
   procedure Fire_On_Time_Update (Media : in out Multimedia_Type);
   --  Position has changed by user or through playing media

   procedure On_Volume_Change_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event);
   procedure Fire_On_Volume_Change (Media : in out Multimedia_Type);

   procedure On_Waiting_Handler
     (Media   : in out Multimedia_Type;
      Handler : in     Gnoga.Gui.Action_Event);
   procedure Fire_On_Waiting (Media : in out Multimedia_Type);
   --  Video has stopped to buffer next frame

   -------------------------------------------------------------------------
   --  Media_Type - Event Methods
   -------------------------------------------------------------------------

   overriding
   procedure On_Message (Object  : in out Multimedia_Type;
                         Event   : in     String;
                         Message : in     String);
   --  Called on receiving any message or event from browser for Object

   -------------------------------------------------------------------------
   --  Audio_Types
   -------------------------------------------------------------------------

   type Audio_Type is new Multimedia_Type with private;
   type Audio_Access is access all Audio_Type;
   type Pointer_To_Audio_Class is access all Audio_Type'Class;

   -------------------------------------------------------------------------
   --  Audio_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Audio     : in out Audio_Type;
                     Parent    : in out Gnoga.Gui.Base_Type'Class;
                     Source    : in     String  := "";
                     Controls  : in     Boolean := True;
                     Preload   : in     Boolean := False;
                     Autoplay  : in     Boolean := False;
                     Autoloop  : in     Boolean := False;
                     Muted     : in     Boolean := False;
                     ID        : in     String  := "");
   --  Create an Audio control with audio from Content

   -------------------------------------------------------------------------
   --  Video_Types
   -------------------------------------------------------------------------

   type Video_Type is new Multimedia_Type with private;
   type Video_Access is access all Video_Type;
   type Pointer_To_Video_Class is access all Video_Type'Class;

   -------------------------------------------------------------------------
   --  Video_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create (Video     : in out Video_Type;
                     Parent    : in out Gnoga.Gui.Base_Type'Class;
                     Source    : in     String  := "";
                     Controls  : in     Boolean := True;
                     Preload   : in     Boolean := False;
                     Poster    : in     String  := "";
                     Autoplay  : in     Boolean := False;
                     Autoloop  : in     Boolean := False;
                     Muted     : in     Boolean := False;
                     ID        : in     String  := "");
   --  Create an Video control with Video from Content. Poster is a URL
   --  to an image to display until play begins.

private
   type Multimedia_Type is new Gnoga.Gui.Element.Element_Type with
      record
         On_Media_Abort_Event      : Gnoga.Gui.Action_Event := null;
         On_Can_Play_Event         : Gnoga.Gui.Action_Event := null;
         On_Can_Play_Through_Event : Gnoga.Gui.Action_Event := null;
         On_Duration_Change_Event  : Gnoga.Gui.Action_Event := null;
         On_Ended_Event            : Gnoga.Gui.Action_Event := null;
         On_Emptied_Event          : Gnoga.Gui.Action_Event := null;
         On_Media_Error_Event      : Gnoga.Gui.Action_Event := null;
         On_Loaded_Data_Event      : Gnoga.Gui.Action_Event := null;
         On_Loaded_Meta_Data_Event : Gnoga.Gui.Action_Event := null;
         On_Load_Start_Event       : Gnoga.Gui.Action_Event := null;
         On_Pause_Event            : Gnoga.Gui.Action_Event := null;
         On_Play_Event             : Gnoga.Gui.Action_Event := null;
         On_Playing_Event          : Gnoga.Gui.Action_Event := null;
         On_Progress_Event         : Gnoga.Gui.Action_Event := null;
         On_Rate_Change_Event      : Gnoga.Gui.Action_Event := null;
         On_Seeked_Event           : Gnoga.Gui.Action_Event := null;
         On_Seeking_Event          : Gnoga.Gui.Action_Event := null;
         On_Stalled_Event          : Gnoga.Gui.Action_Event := null;
         On_Suspend_Event          : Gnoga.Gui.Action_Event := null;
         On_Time_Update_Event      : Gnoga.Gui.Action_Event := null;
         On_Volume_Change_Event    : Gnoga.Gui.Action_Event := null;
         On_Waiting_Event          : Gnoga.Gui.Action_Event := null;
      end record;
   type Audio_Type is new Multimedia_Type with null record;
   type Video_Type is new Multimedia_Type with null record;
end Ada_GUI.Gnoga.Gui.Element.Multimedia;
