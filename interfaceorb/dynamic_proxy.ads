------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                       D Y N A M I C . P R O X Y                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1 $
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
--                  (email: adabroker-devel@ada.eu.org)                     --
--                                                                          --
------------------------------------------------------------------------------

with CORBA;
with CORBA.NVList;
with AdaBroker.OmniProxyCallDesc;
with AdaBroker.GIOP_C;
use CORBA;

package Dynamic_Proxy is

   type Operation_Proxy is
     new AdaBroker.OmniProxyCallDesc.Object with private;

   type Operation_Type is (Operation_Function, Operation_Procedure);

   procedure Init
     (Self : in out Operation_Proxy;
      Op   : in     CORBA.Identifier;
      Args : in     CORBA.NVList.Object;
      Res  : in     NamedValue;
      Ot   : in     Operation_Type);

   function Get_Function_Result
     (Self : in Operation_Proxy)
      return NamedValue;
   --  Returns the return value for remote functions

   function Get_Procedure_Result
     (Self     : in Operation_Proxy)
      return NVList.Object;
   --  Returns the OUT values for remore procedures

   --
   --  the functions below override the ones of OmniProxyCallDesc
   --

   function Operation
     (Self : in Operation_Proxy)
      return CORBA.String;
   --  Returns the name of the subprogram

   function Align_Size
     (Self    : in Operation_Proxy;
      Size_In : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long;
   --  Computes the size needed to marshall the arguments contained in the NV

   procedure Marshal_Arguments
     (Self        : in     Operation_Proxy;
      GIOP_Client : in out AdaBroker.GIOP_C.Object);
   --  Marshalling of the arguments of the operation (from the NamedValues)

   procedure Unmarshal_Returned_Values
     (Self        : in out Operation_Proxy;
      GIOP_Client : in out AdaBroker.GIOP_C.Object);
   --  Unmarshall the returned value (to a NamedValue)

private

   procedure Unmarshal_Returned_Value
     (Self        : in     Operation_Proxy;
      GIOP_Client : in out AdaBroker.GIOP_C.Object;
      A           :    out Any;
      Tck         : in     TCKind);

   type Operation_Proxy is  new AdaBroker.OmniProxyCallDesc.Object with
      record
         Op_Type        : Operation_Type;
         Op_Name        : CORBA.Identifier;
         Args           : NVList.Object;
         Private_Result : NamedValue;
      end record;

end Dynamic_Proxy;
