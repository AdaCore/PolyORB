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
with CORBA.Object;

with Broca.Buffers; use Broca.Buffers;
with Broca.POA;
with Broca.Stream;
with Broca.IOP;

package Broca.Server is

   pragma Elaborate_Body;

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

   --  True if, and only if, this server can create profiles.
   function Can_Create_Profile
     (Server : access Server_Type)
     return Boolean is abstract;

   --  Return a profile that denotes Server's transport endpoint
   --  and the given object key.
   --  The returned access designates a dynamically-allocated
   --  profile. It is up to the caller to deallocate the associated
   --  storage after use.
   function Make_Profile
     (Server     : access Server_Type;
      Object_Key : Encapsulation)
     return Broca.IOP.Profile_Ptr
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
     (POA : Broca.POA.Ref);

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
   procedure Register_POA
     (POA : Broca.POA.Ref);

   --  Unregister a POA.
   --  broca.poa.all_poas_lock should have been lock_w.
   procedure Unregister_POA
     (POA : Broca.POA.Ref);

   --  Coding of object key:
   --  An object key is a sequence of octets, whose length LENGTH is known.
   --   0 ..  3: boot time or 0 if persistent POA.
   --   4 ..  7: poa index in all_POAs
   --   8 .. 11: date of the entry
   --  12 .. 15: N_POA
   --  (*) number of POAs in the POA path name, can be 0 if POA is
   --  TRANSIENT.  The POA path name starts after the RootPOA, ie for
   --  an object of the RootPOA, its path name is empty.
   --  alignment on long boundary.
   --  for 1 .. N_POA
   --     0 .. X: poa name, as a CORBA string
   --  ? .. ?: key given by the POA, the length is known.

   --  Marshall procedure are called only by Build_IOR.
   --  The POA must be locked to prevent any destruction.

   procedure Marshall
     (Buffer : access Buffers.Buffer_Type;
      POA    : in Broca.POA.Ref);

   --  Unmarshall an object_key. POA is the last POA that was reached.
   --  It can be different from the POA of the object, if a adapter
   --  activator has to be called but the state of the POA associated
   --  with the activator was not active. If POA is an intermediate
   --  POA, then KEY.POS is 0.
   --
   --  After the call, POA, if not null, has been link_lock.lock_R.
   --  POA_STATE is the state of the POA.

   procedure Unmarshall
     (Buffer    : access Buffers.Buffer_Type;
      POA       : out Broca.POA.Ref;
      POA_State : out Broca.POA.Processing_State_Type);

   function Build_IOR
     (Type_Id : CORBA.RepositoryId;
      POA     : Broca.POA.Ref;
      Key     : Broca.Buffers.Encapsulation)
     return CORBA.Object.Ref;
   --  This procedure builds an IOR and returns a properly
   --  externalised object reference. KEY is only the key for the
   --  POA, not the full object key.

private

   type Server_Id_Type is new Natural;

end Broca.Server;
