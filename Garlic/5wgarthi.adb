------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                   S Y S T E M . G A R L I C . T H I N                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
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
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with System.Garlic.Debug;
pragma Warnings (Off, System.Garlic.Debug);

package body System.Garlic.Thin is

   --  This is the Windows version of s-garthi.adb.

   WSAData_Dummy : array (1 .. 512) of C.int;

   WS_Version : constant := 16#0101#;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      function WSAStartup (WS_Version     : Interfaces.C.int;
                           WSADataAddress : System.Address)
        return Interfaces.C.int;
      pragma Import (Stdcall, WSAStartup, "WSAStartup");
      Return_Value : Interfaces.C.int;
   begin
      Return_Value := WSAStartup (WS_Version, WSAData_Dummy'Address);
      pragma Assert (Interfaces.C."=" (Return_Value, 0));
   end Initialize;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
      procedure WSACleanup;
      pragma Import (Stdcall, WSACleanup, "WSACleanup");
   begin
      WSACleanup;
   end Shutdown;

   -----------
   -- Clear --
   -----------

   procedure Clear  (FS : in out Fd_Set) is
   begin
      FS.fd_count := 0;
   end Clear;

   ---------
   -- Set --
   ---------

   procedure Set    (FS : in out Fd_Set; Socket : in Socket_Fd) is
      use type C.unsigned;
   begin
      FS.fd_count := FS.fd_count + 1;
      FS.fd_array (FS.fd_count) := Socket;
   end Set;

   ------------
   -- Is_Set --
   ------------

   function  Is_Set (FS : in Fd_Set; Socket : in Socket_Fd)
     return Boolean is
   begin
      for K in 1 .. FS.fd_count loop
         if FS.fd_array (K) = Socket then
            return True;
         end if;
      end loop;
      return False;
   end Is_Set;

   --------------
   -- C_Setsid --
   --------------

   procedure C_Setsid is
   begin
      null;
   end C_Setsid;

end System.Garlic.Thin;
