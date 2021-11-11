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
   type Multimedia_Type is new Gnoga.Gui.Element.Element_Type with null record;
   type Audio_Type is new Multimedia_Type with null record;
   type Video_Type is new Multimedia_Type with null record;
end Ada_GUI.Gnoga.Gui.Element.Multimedia;
