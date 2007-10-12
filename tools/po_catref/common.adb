------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               C O M M O N                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Output;

package body Common is

   use PolyORB.Sockets;
   use Output;

   --------------------------------
   -- Output_Address_Information --
   --------------------------------

   procedure Output_Address_Information
     (Addr : PolyORB.Sockets.Sock_Addr_Type)
   is
   begin
      declare
         Host : constant Host_Entry_Type := Get_Host_By_Address (Addr.Addr);

      begin
         Put_Line ("Host Name", Official_Name (Host));
         Put_Line ("Address", Image (Addr.Addr));
         Put_Line ("Family", Family_Type'Image (Addr.Family));
         Put_Line ("Port", Port_Type'Image (Addr.Port));
      end;

   exception
      when Host_Error =>
         Put_Line ("Address", Image (Addr.Addr));
         Put_Line ("Family", Family_Type'Image (Addr.Family));
         Put_Line ("Port", Port_Type'Image (Addr.Port));
   end Output_Address_Information;

   -------------------------------
   -- Output_Object_Information --
   -------------------------------

   procedure Output_Object_Information (Obj : PolyORB.Objects.Object_Id) is
   begin
      Put_Line ("Object_Id", PolyORB.Objects.Image (Obj));
   end Output_Object_Information;

end Common;
