------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                           B R O C A . I I O P                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces.C;
with CORBA;
with Broca.Sequences;
with Broca.Buffers;
with Broca.Locks;
with Broca.IOP;
with Sockets.Thin;
with Interfaces.C;
pragma Elaborate_All (Broca.IOP);

package Broca.IIOP is
   type Version_Type is
      record
         Major : CORBA.Octet;
         Minor : CORBA.Octet;
      end record;

   --  Simply linked list of strands.
   --  A strand is simply a connection with associated data.
   type Strand_Type;
   type Strand_Ptr is access Strand_Type;
   type Strand_Type is
      record
         Next : Strand_Ptr;
         --  File descriptor for the connection.
         Fd : Interfaces.C.int;
         --  Request id for the next request.
         Request_Id : CORBA.Unsigned_Long;
         --  A client is using this strand and waiting for a reply.
         Lock : Broca.Locks.Mutex_Type;
      end record;

   type Profile_IIOP_Type is new IOP.Profile_Type with
      record
         --  Informations directly taken from the IOR.
         IIOP_Version : Version_Type;
         Host : CORBA.String;
         Port : CORBA.Unsigned_Short;
         Network_Port : Interfaces.C.unsigned_short;
         Object_Key : Broca.Sequences.Octet_Sequence;
         --  Components: Natural;

         --  The address corresponding to host/port.
         Socket_Address : Sockets.Thin.Sockaddr_In;

         --  List of strands.
         Strands : Strand_Ptr := null;

         --  Lock on the list of strands.
         Lock : Broca.Locks.Rw_Lock_Type;
      end record;

   type Profile_IIOP_Ptr is access all Profile_IIOP_Type;

   --  Find a free connection (or create a new one) for a message to an
   --  OBJECT via PROFILE.
   function Find_Connection
     (Profile : access Profile_IIOP_Type)
     return IOP.Connection_Ptr;

   function Get_Profile_Tag
     (Profile : Profile_IIOP_Type)
     return IOP.Profile_Tag;

   procedure Create_Profile
     (Buffer : access Broca.Buffers.Buffer_Type;
      Profile : out IOP.Profile_Ptr);

private

   function Get_Object_Key
     (Profile : Profile_IIOP_Type)
     return Broca.Sequences.Octet_Sequence;

end Broca.IIOP;
