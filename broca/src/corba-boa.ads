------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                            C O R B A . B O A                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.3 $
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

with Broca.Imp_Object;

package CORBA.BOA is

   procedure Implementation_Is_Ready (Non_Blocking : in Boolean := False)
     renames Broca.Imp_Object.Implementation_Is_Ready;
   --  Calling this function will cause the BOA to start accepting requests
   --  from other address spaces.  Default behaviour will block
   --  indefinitely on this call if the Non_Blocking argument is not set to
   --  True

   procedure Object_Is_Ready
     (Obj  : access Broca.Imp_Object.Implemented_Object'Class)
     renames Broca.Imp_Object.Object_Is_Ready;
   --  Tells the BOA that this object is ready to accept connexions.  It has
   --  to be done once (and only once) for each local object.  The user HAS
   --  to call this function, it cannot be called automatically.

--    procedure Dispose_Object
--      (Self : in Object;
--       Obj  : in OmniObject.Implemented_Object'Class);
--    --  Tells the BOA that this object is going to be destroyed and that it
--    --  should not accept connexions any longer The user HAS to call this
--    --  function, it cannot be called automatically.

end CORBA.BOA;

