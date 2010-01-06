------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . B I N D I N G _ D A T A . G I O P . I N E T        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2008, Free Software Foundation, Inc.          --
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

--  Common utilities for GIOP instances that rely on IP sockets.

with PolyORB.Buffers;
with PolyORB.Utils.Sockets;

package PolyORB.Binding_Data.GIOP.INET is

   procedure Common_Marshall_Profile_Body
     (Buffer             : access Buffers.Buffer_Type;
      Profile            : Profile_Access;
      Sock               : Utils.Sockets.Socket_Name;
      Marshall_Object_Id : Boolean);

   function Common_Unmarshall_Profile_Body
     (Buffer                       : access Buffers.Buffer_Type;
      Profile                      : Profile_Access;
      Unmarshall_Object_Id         : Boolean;
      Unmarshall_Tagged_Components : Boolean)
      return Utils.Sockets.Socket_Name;
   --  If True always unmarshall tagged component, if False then the
   --  tagged components are unmarshalled only if Version_Minor >= 1.

   function Common_IIOP_DIOP_Profile_To_Corbaloc
     (Profile : Profile_Access;
      Address : Utils.Sockets.Socket_Name;
      Prefix  : String) return String;

   function Common_IIOP_DIOP_Corbaloc_To_Profile
     (Str           : String;
      Default_Major : Types.Octet;
      Default_Minor : Types.Octet;
      Profile       : access Profile_Access) return Utils.Sockets.Socket_Name;
   --  Set Profile.all.all and return address according to given corbaloc URI.
   --  In case of error, Profile.all is freed.

end PolyORB.Binding_Data.GIOP.INET;
