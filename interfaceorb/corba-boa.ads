------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                            C O R B A . B O A                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.25 $
--                                                                          --
--         Copyright (C) 1999-2000 ENST Paris University, France.           --
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

with AdaBroker.OmniORB;

package CORBA.BOA is

   procedure Init (Identifier : in Standard.String);

   procedure Impl_Is_Ready (Non_Blocking : in Boolean := False);
   --  Calling this function will cause the BOA to start accepting requests
   --  from other address spaces.  Default behaviour will block
   --  indefinitely on this call if the Non_Blocking argument is not set to
   --  True

   procedure Impl_Shutdown;
   --  This is the reverse of impl_is_ready().  When this call returns, all
   --  the internal threads and network connections will be shutdown.  Any
   --  thread blocking on impl_is_ready is unblocked.  When this call
   --  returns, requests from other address spaces will not be dispatched.
   --  The BOA can be reactivated by impl_is_ready(), it will continue to
   --  use the existing network addresses when reactivated.
   --
   --  Note: This function should not be called in the implementation of a
   --  CORBA interface. Otherwise, this call will be blocked indefinitely
   --  waiting on itself to complete the request.


   procedure Destroy;
   --
   --  Calling this function will destroy this BOA. The function will call
   --  impl_shutdown() implicitly if it has not been called. When this call
   --  returns, the network addresses (endpoints) where this BOA listens on
   --  will be freed.
   --
   --  Note: After this call, the BOA should not be used directly or
   --  indirectly, otherwise the behaviour is undefined. If there is any
   --  object implementation still registered with the BOA when this
   --  function is called, the object implementation should not be called
   --  afterwards. This function does not call the dispose method of the
   --  implementations.
   --
   --  Note: Initialisation of another BOA using ORB::BOA_init() is not
   --  supported. The behaviour of ORB::BOA_init() after this function is
   --  called is undefined.


   procedure Object_Is_Ready
     (Obj  : in AdaBroker.OmniORB.ImplObject'Class);
   --  Tell the BOA that this object is ready to accept connexions it has
   --  to be done once (and only once) for each local object.  The user HAS
   --  to call this function, it cannot be called automatically.

   procedure Dispose_Object
     (Obj  : in AdaBroker.OmniORB.ImplObject'Class);
   --  Tell the BOA that this object is going to be destroyed and that it
   --  should not accept connexions any longer The user HAS to call this
   --  function, it cannot be called automatically.

end CORBA.BOA;

