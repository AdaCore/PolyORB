------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                            B R O C A . I O P                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.5 $
--                                                                          --
--         Copyright (C) 1999, 2000 ENST Paris University, France.          --
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
with Broca.Opaque;
with Broca.Buffers;
with Broca.Sequences;

package Broca.IOP is

   -----------------------------------------
   -- The abstract interface for all GIOP --
   -- connections.                        --
   -----------------------------------------

   --  Contains data for a connection.
   type Connection_Type is abstract tagged private;
   type Connection_Ptr is access all Connection_Type'Class;

   function Get_Request_Id
     (Connection : access Connection_Type)
     return CORBA.Unsigned_Long;
   --  Get a new Request Id for this Connection.

   procedure Release_Connection
     (Connection : access Connection_Type) is abstract;
   --  Release a previously locked connection.

   procedure Send
     (Connection : access Connection_Type;
      Buffer     : access Buffers.Buffer_Type)
      is abstract;
   --  Send a buffer to a connection. Raise Comm_Failure
   --  on error.

   --  procedure Receive
   --    (Connection : access Connection_Type;
   --     Buffer     : access Buffers.Buffer_Type)
   --     is abstract;
   --  Receive data from a connection. Fill exactly Buffer.
   --  Raise Comm_Failure on error.

   function Receive
     (Connection : access Connection_Type;
      Length     : Opaque.Index_Type)
     return Opaque.Octet_Array
      is abstract;
   --  Receive data from a connection.
   --  Raise Comm_Failure on error.

   -------------------------------------------------------
   -- The abstract interface for all GIOP profile types --
   -------------------------------------------------------

   subtype Profile_Tag is CORBA.Unsigned_Long;
   Tag_Internet_IOP        : constant Profile_Tag := 0;
   Tag_Multiple_Components : constant Profile_Tag := 1;

   type Profile_Type is abstract tagged limited private;

   function Get_Object_Key
     (Profile : Profile_Type)
     return Broca.Sequences.Octet_Sequence is abstract;
   --  Retrieve the opaque object key from Profile.

   function Find_Connection
     (Profile : access Profile_Type)
     return Connection_Ptr is abstract;
   --  Find a free connection (or create a new one) for
   --  a message to the transport endpoint designated by
   --  Profile, and reserve it.

   function Get_Profile_Tag
     (Profile : Profile_Type)
      return Profile_Tag is abstract;
   --  Return Profile's Standard Protocol Profile tag.

   type Profile_Ptr is access all Profile_Type'Class;

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

   type Encapsulate_Profile_Type is
     access procedure
     (Buffer  : access Buffers.Buffer_Type;
      Profile : access Profile_Type'Class);

   type Decapsulate_Profile_Type is
     access procedure
     (Buffer  : access Buffers.Buffer_Type;
      Profile : out Profile_Ptr);

   procedure Register
     (Profile     : in Profile_Tag;
      Encapsulate : in Encapsulate_Profile_Type;
      Decapsulate : in Decapsulate_Profile_Type);

   type Profile_Record is
      record
         Encapsulate : Encapsulate_Profile_Type;
         Decapsulate : Decapsulate_Profile_Type;
      end record;

   Callbacks : array (Tag_Internet_IOP .. Tag_Multiple_Components)
     of Profile_Record;

private

   type Connection_Type is abstract tagged record
      Request_Id : CORBA.Unsigned_Long := 1;
   end record;

   type Profile_Type is abstract tagged limited null record;

end Broca.IOP;
