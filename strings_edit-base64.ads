--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.Base64                         Luebeck            --
--  Interface                                      Autumn, 2014       --
--                                                                    --
--                                Last revision :  18:40 01 Aug 2019  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--
--
--  This package  provides  Base64  encoding and decoding  as defined in
--  RFC 4648.
--
with Ada.Streams;  use Ada.Streams;
with Interfaces;   use Interfaces;

package Strings_Edit.Base64 is
--
-- From_Base64 -- Decode Base64 to plain string
--
--    Text - To convert
--
-- Returns :
--
--    Equivalent string
--
-- Exceptions :
--
--    Data_Error - Syntax error
--
   function From_Base64 (Text : String) return String;
--
-- To_Base64 -- Encode plain string as Base64
--
--    Text - To convert
--
-- Returns :
--
--    Base64 string
--
   function To_Base64 (Text : String) return String;
------------------------------------------------------------------------
--
-- Base64_Encoder -- Base64 encoding stream
--
--    Size - The stream buffer size
--
-- The encoder stream when  written encodes the input  into Base64.  The
-- encoded content can be then read from the stream.
--
   type Base64_Encoder
        (  Size : Stream_Element_Count
        )  is new Root_Stream_Type with private;
--
-- Flush -- The encoding
--
--    Stream - The encoding stream
--
-- This procedure is called at the end of encoding after the last stream
-- element has been written.  Status_Error is propagated when the stream
-- presently has  no space available.  The  operation  can  be  repeated
-- after reading from the stream.
--
-- Exceptions :
--
--    Status_Error - No space in the stream
--    Use_Error    - The stream is too small (fatal)
--
   procedure Flush (Stream : in out Base64_Encoder);
--
-- Free -- Available space
--
--    Stream - The encoding stream
--
-- Returns :
--
--    The number of elements that can be safely written
--
   function Free (Stream : Base64_Encoder) return Stream_Element_Count;
--
-- Is_Empty -- Check if the stream is empty
--
--    Stream - The encoding stream
--
-- Returns :
--
--    True of the stream can be read
--
   function Is_Empty (Stream : Base64_Encoder) return Boolean;
--
-- Is_Full -- Check if the stream is full
--
--    Stream - The encoding stream
--
-- Returns :
--
--    True if the stream cannot be written
--
   function Is_Full (Stream : Base64_Encoder) return Boolean;
--
-- Reset -- The stream
--
--    Stream - The encoding stream
--
   procedure Reset (Stream : in out Base64_Encoder);
--
-- Size -- The maximum stream capacity
--
--    Stream - The encoding stream
--
-- Returns :
--
--    How many elements can be written when the stream is empty
--
   function Size (Stream : Base64_Encoder) return Stream_Element_Count;
--
-- Used -- The number of elements in the stream
--
--    Stream - The encoding stream
--
-- Returns :
--
--    The number of elements available to read
--
   function Used (Stream : Base64_Encoder) return Stream_Element_Count;
------------------------------------------------------------------------
--
-- Base64_Decoder -- Base64 decoding stream
--
--    Size - The stream buffer size
--
-- The decoder stream when  written decodes the  input from Base64.  The
-- decoded content can be then read from the stream.
--
   type Base64_Decoder
        (  Size : Stream_Element_Count
        )  is new Root_Stream_Type with private;
--
-- Reset -- The stream
--
--    Stream - The encoding stream
--
   procedure Reset (Stream : in out Base64_Decoder);
--
-- Free -- Available space
--
--    Stream - The encoding stream
--
-- Returns :
--
--    The number of elements that can be safely written
--
   function Free (Stream : Base64_Decoder) return Stream_Element_Count;
--
-- Is_Empty -- Check if the stream is empty
--
--    Stream - The decoding stream
--
-- Returns :
--
--    True of the stream can be read
--
   function Is_Empty (Stream : Base64_Decoder) return Boolean;
--
-- Is_Full -- Check if the stream is full
--
--    Stream - The decoding stream
--
-- Returns :
--
--    True if the stream cannot be written
--
   function Is_Full (Stream : Base64_Decoder) return Boolean;
--
-- Size -- The maximum stream capacity
--
--    Stream - The decoding stream
--
-- Returns :
--
--    How many elements can be written when the stream is empty
--
   function Size (Stream : Base64_Decoder) return Stream_Element_Count;
--
-- Used -- The number of elements in the stream
--
--    Stream - The decoding stream
--
-- Returns :
--
--    The number of elements available to read
--
   function Used (Stream : Base64_Decoder) return Stream_Element_Count;

private
   type Base64_Stream
        (  Size : Stream_Element_Count
        )  is abstract new Root_Stream_Type with
   record
      Item_In  : Stream_Element_Count := 1;
      Item_Out : Stream_Element_Count := 1;
      Accum    : Unsigned_16          := 0;
      Bits     : Natural              := 0;
      FIFO     : Stream_Element_Array (1..Size);
   end record;
   function Free (Stream : Base64_Stream)
      return Stream_Element_Count is abstract;
   procedure Read
             (  Stream : in out Base64_Stream;
                Data   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             );
   function Size (Stream : Base64_Stream)
      return Stream_Element_Count is abstract;
   procedure Store
             (  Stream : in out Base64_Stream;
                Item   : Stream_Element
             );
   type Base64_Encoder
        (  Size : Stream_Element_Count
        )  is new Base64_Stream (Size) with null record;
   procedure Write
             (  Stream : in out Base64_Encoder;
                Data   : Stream_Element_Array
             );

   type Base64_Decoder
        (  Size : Stream_Element_Count
        )  is new Base64_Stream (Size) with
   record
      Equality : Boolean := False;
   end record;
   procedure Write
             (  Stream : in out Base64_Decoder;
                Data   : Stream_Element_Array
             );

end Strings_Edit.Base64;
