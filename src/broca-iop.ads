------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                            B R O C A . I O P                             --
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

with Ada.Finalization;

with CORBA;

with Broca.Opaque;
with Broca.Buffers;
with Broca.Sequences;

package Broca.IOP is

   -----------------------------------
   -- Abstract GIOP connection type --
   -----------------------------------

   type Connection_Type is abstract tagged private;
   type Connection_Ptr is access all Connection_Type'Class;

   function Get_Request
     (Connection : access Connection_Type)
     return CORBA.Unsigned_Long;
   --  Get a new request id for this connection.

   procedure Release
     (Connection : access Connection_Type) is abstract;
   --  Release a previously suspended connection.

   procedure Send
     (Connection : access Connection_Type;
      Buffer     : access Buffers.Buffer_Type) is abstract;
   --  Send a buffer to a connection. Raise Comm_Failure on error.

   function Receive
     (Connection : access Connection_Type;
      Length     : Opaque.Index_Type)
     return Opaque.Octet_Array_Ptr is abstract;
   --  Receive data from a connection. Raise Comm_Failure on error.

   --------------------------------
   -- Abstract GIOP profile type --
   --------------------------------

   subtype Profile_Tag is CORBA.Unsigned_Long;

   Tag_Internet_IOP        : constant Profile_Tag;
   Tag_Multiple_Components : constant Profile_Tag;

   type Profile_Type is abstract
     new Ada.Finalization.Limited_Controlled
     with private;

   function Get_Object_Key
     (Profile : Profile_Type)
     return Broca.Sequences.Octet_Sequence is abstract;
   --  Retrieve the opaque object key from Profile.

   function Find_Connection
     (Profile : access Profile_Type)
     return Connection_Ptr is abstract;
   --  Find a connection (or create a new one) that matches this
   --  profile into order to send a message to the transport endpoint
   --  designated by Profile.

   function Get_Profile_Tag
     (Profile : Profile_Type)
      return Profile_Tag is abstract;
   --  Return standard protocol profile tag.

   procedure Marshall_Profile_Body
     (Buffer  : access Buffers.Buffer_Type;
      Profile : Profile_Type) is abstract;
   --  Marshall the ProfileBody for Profile into Buffer.

   --  function Unmarshall_Profile_Body
   --    (Buffer : access Buffers.Buffer_Type)
   --    return Profile_Ptr
   --  Unmarshall a ProfileBody from Buffer.
   --  For each derived type of Profile_Type, one such function
   --  must be defined and registered using Broca.IOP.Register.

   type Profile_Ptr is access all Profile_Type'Class;

   procedure Marshall_Tagged_Profile
     (Buffer : access Buffers.Buffer_Type;
      Profile : Profile_Type'Class);
   --  Marshall a TaggedProfile into Buffer.

   function Unmarshall_Tagged_Profile
     (Buffer : access Buffers.Buffer_Type)
     return Profile_Ptr;
   --  Unmarshall a TaggedProfile from Buffer.
   --  The Profile_Type designated by the returned
   --  Profile_Ptr is dynamically allocated; it is up
   --  to the caller to release the associated storage
   --  when the profile is not needed anymore.

   type Profile_Ptr_Array is
     array (CORBA.Unsigned_Long range <>) of Profile_Ptr;

   type Profile_Ptr_Array_Ptr is access Profile_Ptr_Array;

   -----------------------
   -- Object References --
   -----------------------

   procedure Encapsulate_IOR
     (Buffer   : access Buffers.Buffer_Type;
      Type_Id  : in CORBA.String;
      Profiles : in Profile_Ptr_Array_Ptr);

   procedure Decapsulate_IOR
     (Buffer   : access Buffers.Buffer_Type;
      Type_Id  : out CORBA.String;
      Profiles : out Profile_Ptr_Array_Ptr);

   type Unmarshall_Profile_Body_Type is
     access function
     (Buffer  : access Buffers.Buffer_Type)
     return Profile_Ptr;

   procedure Register
     (Tag                     : in Profile_Tag;
      Unmarshall_Profile_Body : in Unmarshall_Profile_Body_Type);
   --  Register Unmarshall_Profile_Body as the function used
   --  to unmarshall a Tagged Profile Body corresponding to Tag.

   --  procedure Unegister
   --    (Tag : in Profile_Tag);
   --  Remove any registered unmarshalling function associated
   --  with Tag.

private

   Tag_Internet_IOP        : constant Profile_Tag := 0;
   Tag_Multiple_Components : constant Profile_Tag := 1;

   type Connection_Type is abstract tagged record
      Request : CORBA.Unsigned_Long := 1;
   end record;

   type Profile_Type is abstract
     new Ada.Finalization.Limited_Controlled
     with null record;

end Broca.IOP;
