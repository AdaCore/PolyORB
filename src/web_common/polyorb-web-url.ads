------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      P O L Y O R B . W E B . U R L                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2000-2010, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;

package PolyORB.Web.URL is

   --  The general URL form as described in RFC2616 is:
   --
   --  http_URL = "http:" "//" host [ ":" port ] [ abs_path [ "?" query ]]
   --
   --  Note also that there are different RFC describing URL like the 2616 and
   --  1738 but they use different terminologies. Here we try to follow the
   --  names used in RFC2616 but we have implemented some extensions at the
   --  end of this package. For example the way Path and File are separated or
   --  the handling of user/password which is explicitly not allowed in the
   --  RFC but are used and supported in many browsers. Here are the extended
   --  URL supported:
   --
   --  http://username:password@www.here.com:80/dir1/dir2/xyz.html?p=8&x=doh
   --   |                            |       | |          |       |
   --   protocol                     host port path       file    parameters
   --
   --                                          <--  pathname  -->

   type Object is private;

   URL_Error : exception;

   Default_HTTP_Port  : constant := 80;
   Default_HTTPS_Port : constant := 443;

   function Parse
      (URL            : String;
       Check_Validity : Boolean := True;
       Normalize      : Boolean := False)
       return Object;
   --  Parse an URL and return an Object representing this URL. It is then
   --  possible to extract each part of the URL with the services bellow.
   --  Raises URL_Error if Check_Validity is true and the URL reference a
   --  resource above the web root directory.

   procedure Normalize (URL : in out Object);
   --  Removes all occurrences to parent directory ".." and current directory
   --  ".". Raises URL_Error if the URL reference a resource above the Web
   --  root directory.

   function Is_Valid (URL : Object) return Boolean;
   --  Returns True if the URL is valid (does not reference directory above
   --  the Web root).

   function URL (URL : Object) return String;
   --  Returns full URL string, this can be different to the URL passed if it
   --  has been normalized.

   function Protocol_Name (URL : Object) return String;
   --  Returns "http" or "https" depending on the protocol used by URL.

   function Host (URL : Object) return String;
   --  Returns the hostname.

   function Protocol (URL : Object) return String;
   --  returns the protocol used by the connection

   function Port (URL : Object) return Positive;
   --  Returns the port as a positive.

   function Port (URL : Object) return String;
   --  Returns the port as a string.

   function Abs_Path
     (URL    : Object;
      Encode : Boolean := False)
      return String;
   --  Returns the absolute path. This is the complete resource reference
   --  without the query part.

   function Query
     (URL    : Object;
      Encode : Boolean := False)
      return String;
   --  Returns the Query part of the URL or the empty string if none was
   --  specified. Note that character '?' is not part of the Query and is
   --  therefore not returned.

   --
   --  Below are extended API not part of the RFC 2616 URL specification.
   --

   function User (URL : Object) return String;
   --  Returns user name part of the URL. Returns the empty string if user was
   --  not specified.

   function Password (URL : Object) return String;
   --  Returns user's password part of the URL. Returns the empty string if
   --  password was not specified.

   function Server_Name (URL : Object) return String renames Host;

   function Security (URL : Object) return Boolean;
   --  Returns True if it is a Secure HTTP (HTTPS) URL.

   function Path (URL : Object; Encode : Boolean := False) return String;
   --  Returns the Path (including the leading slash). If Encode is True then
   --  the URL will be encoded using the Encode routine.

   function File (URL : Object; Encode : Boolean := False) return String;
   --  Returns the File. If Encode is True then the URL will be encoded using
   --  the Encode routine. Not that by File here we mean the latest part of
   --  the URL, it could be a real file or a diretory into the filesystem.
   --  Parent and current directories are part of the path.

   function Parameters
     (URL    : Object;
      Encode : Boolean := False)
      return String;
   --  Returns the Parameters (including the starting ? character). If Encode
   --  is True then the URL will be encoded using the Encode routine.

   function Pathname
     (URL    : Object;
      Encode : Boolean := False)
      return String
      renames Abs_Path;

   function Pathname_And_Parameters
     (URL    : Object;
      Encode : Boolean := False)
      return String;
   --  Returns the pathname and the parameters. This is equivalent to:
   --  Pathname & Parameters.

   function URI (URL : Object; Encode : Boolean := False) return String;
   --  Returns the URI. If Encode is True then the URI will be encoded using
   --  the Encode routine.
   --  For the SOAP personnality

   --
   --  URL Encoding and Decoding
   --

   function Encode (Str : String) return String;
   --  Encode Str into a URL-safe form. Many characters are forbiden into an
   --  URL and needs to be encoded. A character is encoded by %XY where XY is
   --  the character's ASCII hexadecimal code. For example a space is encoded
   --  as %20.

   function Decode (Str : String) return String;
   --  This is the oposite of Encode above.

private

   use Ada.Strings.Unbounded;

   type Path_Status is (Valid, Wrong);

   type Object is record
      User      : Unbounded_String;
      Password  : Unbounded_String;
      Host      : Unbounded_String;
      Protocol  : Unbounded_String;
      Port      : Positive          := Default_HTTP_Port;
      Security  : Boolean           := False;
      Path      : Unbounded_String; -- Original path
      N_Path    : Unbounded_String; -- Normalized path
      File      : Unbounded_String;
      Params    : Unbounded_String;
      Status    : Path_Status       := Wrong;
   end record;

end PolyORB.Web.URL;
