-----------------------------------------------------------------------------
--                                                                         --
--                         ADASOCKETS COMPONENTS                           --
--                                                                         --
--               S O C K E T S . W I N D O W S _ L I N K                   --
--                                                                         --
--                                S p e c                                  --
--                                                                         --
--                        $ReleaseVersion: 0.1.9 $                         --
--                                                                         --
--  Copyright (C) 1998  École Nationale Supérieure des Télécommunications  --
--                                                                         --
--   AdaSockets is free software; you can  redistribute it and/or modify   --
--   it  under terms of the GNU  General  Public License as published by   --
--   the Free Software Foundation; either version 2, or (at your option)   --
--   any later version.   AdaSockets is distributed  in the hope that it   --
--   will be useful, but WITHOUT ANY  WARRANTY; without even the implied   --
--   warranty of MERCHANTABILITY   or FITNESS FOR  A PARTICULAR PURPOSE.   --
--   See the GNU General Public  License  for more details.  You  should   --
--   have received a copy of the  GNU General Public License distributed   --
--   with AdaSockets; see   file COPYING.  If  not,  write  to  the Free   --
--   Software  Foundation, 59   Temple Place -   Suite  330,  Boston, MA   --
--   02111-1307, USA.                                                      --
--                                                                         --
--   As a special exception, if  other  files instantiate generics  from   --
--   this unit, or  you link this  unit with other  files to produce  an   --
--   executable,  this  unit does  not  by  itself cause  the  resulting   --
--   executable to be  covered by the  GNU General Public License.  This   --
--   exception does  not  however invalidate any  other reasons  why the   --
--   executable file might be covered by the GNU Public License.           --
--                                                                         --
--   The main repository for this software is located at:                  --
--       http://www-inf.enst.fr/ANC/                                       --
--                                                                         --
-----------------------------------------------------------------------------
--  Dmitiry Anisimkov changes only for Win32 platform                      --
-----------------------------------------------------------------------------

with Interfaces.C.Strings;

package body Sockets.Windows_Link is

   use Interfaces, Interfaces.C, Interfaces.C.Strings;

   Function_Unsupported_On_Win32 : exception;
   WSAVERNOTSUPPORTED            : exception;

   type WSADATA is
      record
         wVersion       : unsigned_16;
         wHighVersion   : unsigned_16;
         szDescription  : char_array (1 .. 129);
         szSystemStatus : char_array (1 .. 257);
         iMaxSockets    : unsigned_16;
         iMaxUdpDg      : unsigned_16;
         lpVendorInfo   : chars_ptr;
      end record;
   pragma Convention (C, WSADATA);

   WSAInfo : aliased WSADATA;

   type LPWSADATA is access all WSADATA;
   pragma Convention (C, LPWSADATA);

   function WSAStartup
     (VersionRequested : Short_Integer;
      WSAData          : LPWSADATA)
     return Integer;

   pragma Import (StdCall, WSAStartup, "WSAStartup");

   -----------------
   -- Unsupported --
   -----------------

   procedure Unsupported is
   begin
      raise Function_Unsupported_On_Win32;
   end Unsupported;

   Version : constant Short_Integer := 16#0101#;

begin
   if WSAStartup (Version, WSAInfo'Access) /= 0 then
      raise WSAVERNOTSUPPORTED;
   end if;
end Sockets.Windows_Link;
