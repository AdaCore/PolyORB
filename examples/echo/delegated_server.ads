------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                     D E L E G A T E D _ S E R V E R                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 1999 ENST Paris University, France.             --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.Object;
with Echo.Delegate;

package Delegated_Server is

   function Echo_With_Delegation
     (Self : access Integer;
      Mesg : in CORBA.String)
     return CORBA.String;
   --  Function that will be called when a delegation is used. Note that
   --  the Self integer has no meaning here, but a Self is mandatory.

   package Delegated is new Echo.Delegate
     (Wrapped    => Integer,
      echoString => Echo_With_Delegation);
   --  The package Echo.Delegate is instantiated, with the dummy Integer
   --  type as the "real" type.

   Dummy : aliased Integer;
   --  A dummy "object"

end Delegated_Server;
