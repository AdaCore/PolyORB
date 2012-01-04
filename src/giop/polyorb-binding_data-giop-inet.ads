------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . B I N D I N G _ D A T A . G I O P . I N E T        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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
