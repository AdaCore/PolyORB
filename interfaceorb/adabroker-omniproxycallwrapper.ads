------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--       A D A B R O K E R . O M N I P R O X Y C A L L W R A P P E R        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.4 $
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

--  For each function defined in the IDL file, a descendant of
--  Omniproxycalldesc is created. It is the object in charge of storing the
--  arguments of the function, marshalling them into a bufferedstream, call
--  the remote object, and unmarshall the result.

with CORBA.Object;

with AdaBroker; use AdaBroker;
with AdaBroker.OmniORB;
with AdaBroker.OmniProxyCallDesc;

package AdaBroker.OmniProxyCallWrapper is

   procedure Invoke
     (Obj       : in CORBA.Object.Ref'Class;
      Call_Desc : in out OmniProxyCallDesc.Object'Class);
   --  Reimplemented in Ada to call the C++ ORB (modified by Fabien)
   --
   --  Previous solution : wrapper around void invoke(omniObject* o,
   --  OmniProxyCallDesc& call_desc) in proxyCall.cc L 46

   procedure One_Way
     (Obj       : in CORBA.Object.Ref'Class;
      Call_Desc : in out OmniProxyCallDesc.Object'Class);
   --  Reimplemented in Ada to call the C++ ORB see proxyCall.cc L181

private

   function Omni_Call_Transient_Exception_Handler
     (Obj     : in AdaBroker.OmniORB.OmniObject'Class;
      Retries : in CORBA.Unsigned_Long;
      Minor   : in CORBA.Unsigned_Long;
      Status  : in CORBA.Completion_Status)
      return CORBA.Boolean;
   --  This method is wrapped around C method
   --  _omni_callTransientExceptionHandler ( see Ada_CORBA_Exceptions.hh)

   function Omni_Comm_Failure_Exception_Handler
     (Obj     : in AdaBroker.OmniORB.OmniObject'Class;
      Retries : in CORBA.Unsigned_Long;
      Minor   : in CORBA.Unsigned_Long;
      Status  : in CORBA.Completion_Status)
      return CORBA.Boolean;
   --  This method is wrapped around C method
   --  _omni_commFailureExceptionHandler ( see Ada_CORBA_Exceptions.hh)

   function Omni_System_Exception_Handler
     (Obj     : in AdaBroker.OmniORB.OmniObject'Class;
      Retries : in CORBA.Unsigned_Long;
      Minor   : in CORBA.Unsigned_Long;
      Status  : in CORBA.Completion_Status)
      return CORBA.Boolean;
   --  This method is wrapped around C method
   --  _omni_callSystemExceptionHandler ( see Ada_CORBA_Exceptions.hh)

end AdaBroker.OmniProxyCallWrapper;
