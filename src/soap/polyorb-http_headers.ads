------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . H T T P _ H E A D E R S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

package PolyORB.HTTP_Headers is

   pragma Preelaborate;

   type Header is
     (
      --  <ENUM>

      --  general-header

      H_Cache_Control,        --  >> "Cache-Control"
      H_Connection,           --  >> "Connection"
      H_Date,                 --  >> "Date"
      H_Pragma,               --  >> "Pragma"
      H_Trailer,              --  >> "Trailer"
      H_Transfer_Encoding,    --  >> "Transfer-Encoding"
      H_Upgrade,              --  >> "Upgrade"
      H_Via,                  --  >> "Via"
      H_Warning,              --  >> "Warning"

      --  request-header

      H_Accept,               --  >> "Accept"
      H_Accept_Charset,       --  >> "Accept-Charset"
      H_Accept_Language,      --  >> "Accept-Language"
      H_Authorization,        --  >> "Authorization"
      H_Expect,               --  >> "Expect"
      H_From,                 --  >> "From"
      H_Host,                 --  >> "Host"
      H_If_Match,             --  >> "If-Match"
      H_If_Modified_Since,    --  >> "If-Modified-Since"
      H_If_None_Match,        --  >> "If-None-Match"
      H_If_Range,             --  >> "If-Range"
      H_If_Unmodified_Since,  --  >> "If-Unmodified-Since"
      H_Max_Forwards,         --  >> "Max-Forwards"
      H_Proxy_Authorization,  --  >> "Proxy-Authorization"
      H_Range,                --  >> "Range"
      H_Referer,              --  >> "Referer"
      H_TE,                   --  >> "TE"
      H_User_Agent,           --  >> "User-Agent"

      --  response-header

      H_Accept_Ranges,        --  >> "Accept-Ranges"
      H_Age,                  --  >> "Age"
      H_ETag,                 --  >> "ETag"
      H_Location,             --  >> "Location"
      H_Proxy_Authenticate,   --  >> "Proxy-Authenticate"
      H_Retry_After,          --  >> "Retry-After"
      H_Server,               --  >> "Server"
      H_Vary,                 --  >> "Vary"
      H_WWW_Authenticate,     --  >> "WWW-Authenticate"

      --  entity-header

      H_Allow,                --  >> "Allow"
      H_Content_Encoding,     --  >> "Content-Encoding"
      H_Content_Language,     --  >> "Content-Language"
      H_Content_Length,       --  >> "Content-Length"
      H_Content_Location,     --  >> "Content-Location"
      H_Content_MD5,          --  >> "Content-MD5"
      H_Content_Range,        --  >> "Content-Range"
      H_Content_Type,         --  >> "Content-Type"
      H_Expires,              --  >> "Expires"
      H_Last_Modified,        --  >> "Last-Modified"
      H_SOAPAction,           --  >> "SOAPAction"
      Extension_Header

      --  </ENUM>
      );
   pragma Convention (C, Header);

   subtype General_Header is Header
     range H_Cache_Control .. H_Warning;

   subtype Request_Header is Header
     range H_Accept .. H_User_Agent;

   subtype Response_Header is Header
     range H_Accept_Ranges .. H_WWW_Authenticate;

   subtype Entity_Header is Header
     range H_Allow .. Extension_Header;

   function To_String (Id : Header) return String;
   function In_Word_Set (S : String) return Header;

end PolyORB.HTTP_Headers;
