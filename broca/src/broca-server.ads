------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                         B R O C A . S E R V E R                          --
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

with CORBA;
with Broca.Buffers; use Broca.Buffers;
with Broca.POA;
with Broca.Stream;

package Broca.Server is
   --  Log.
   --  This is displayed/stored only if broca.flags.log is true.
   procedure Log (S : String);

   --  A server can receive and send messages.
   --  It can do this by managing connections, eg from inet.
   --
   --  Every server must derive of type server_type, and must register itself
   --  with the register procedure.
   type Server_Type is abstract tagged null record;

   --  Put a message into BUFFER.  Currently, this is a blocking operation, ie,
   --  it must put a message.
   --  BUFFER can already have been allocated, as a result it can be reused,
   --  or freed and replaced.
   procedure Perform_Work
     (Server : access Server_Type)
      is abstract;

   --  During the building of an IOR, this procedure is called to know the
   --  length of the profile for the object designed by INDEX.
   --  Length can be null, if the server can't create a profile.
   --  Need only to update IOR.POS.
   --  OBJECT_KEY must be 4-aligned
   --  procedure Marshall_Size_Profile
   --    (Server : access Server_Type;
   --     IOR : access Broca.Buffers.Buffer_Type;
   --     Object_Key : access Broca.Buffers.Buffer_Type)
   --   is abstract;

   --  True if, and only if, this server can create profiles.
   function Can_Create_Profile
     (Server : access Server_Type)
     return Boolean is abstract;

   --  During the building of an IOR, the procedure is called to marshall a
   --  profile.  The length of the profile (ie, the number added to IOR.POS)
   --  must be the same as that of MARSHALL_SIZE_PROFILE.
   --  In particular, it must be zero for no profile.
   --  OBJECT_KEY must be 4-aligned
   procedure Marshall_Profile
     (Server     : access Server_Type;
      IOR        : access Broca.Buffers.Buffer_Type;
      Object_Key : Encapsulation)
      is abstract;

   type Server_Ptr is access all Server_Type'Class;

   --  This private type is identify a server.  It acts like a cookie for
   --  a server.
   type Server_Id_Type is private;

   --  A server, in order to be active, must register itself with this
   --  procedure.
   procedure Register
     (Server : Server_Ptr;
      Id : out Server_Id_Type);

   --  This procedure is called by a POA to request a server task to perform
   --  arbitrary work, such as cleaning the POA up.
   procedure Request_Cleanup
     (POA : Broca.POA.POA_Object_Ptr);

   --  When a server has a request that can be processed, it must inform
   --  with this procedure.
   --  POS is the identifier coming from register.
   procedure New_Request
     (Id : Server_Id_Type);

   --  This procedure is designed to be called by perform_work primitive to
   --  process a message.
   procedure Handle_Message
     (Stream : Broca.Stream.Stream_Ptr;
      Buffer : access Broca.Buffers.Buffer_Type);

   --  Register a POA.
   --  broca.poa.all_poas_lock should have been lock_w.
   procedure Register_POA (POA : Broca.POA.POA_Object_Ptr);

   --  Unregister a POA.
   --  broca.poa.all_poas_lock should have been lock_w.
   procedure Unregister_POA (POA : Broca.POA.POA_Object_Ptr);

   --  This procedure builds an IOR.
   --  It can return a null_string if there is no profiles for this object.
   --  KEY is only the key for the POA, not the full object key.
   function Build_IOR
     (Type_Id : CORBA.RepositoryId;
      POA : Broca.POA.POA_Object_Ptr;
      Key : Broca.Buffers.Encapsulation)
     return Encapsulation;
private
   type Server_Id_Type is new Natural;
end Broca.Server;
