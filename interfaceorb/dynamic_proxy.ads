------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                       D Y N A M I C . P R O X Y                          --
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
--                  (email: adabroker-devel@ada.eu.org)                     --
--                                                                          --
------------------------------------------------------------------------------


--
--  This package is used in the implementation of the DII.
--
--  It is used to emulate the work that was done by the generated files in
--  static invocation that are used on the client side (i.e. for a xxx.idl
--  file : xxx.ad?, xxx-proxy.ad?, xxx-stream.ad?).
--  It means that this package is a sort of generic stub. By type dispatching,
--  its functions are called in a transparent way by the general invocation
--  mecanism (invoke in adabroker-omniproxucallwrapper) exactly the same way
--  the generated ones are in static invocation.
--
--  A good way to understand how it works is to trace the calls in the static
--  invocation example Echo (from example/echo/client.adb, and then trace the
--  calls in the corresponding dynamic example (dii_client.adb in
--  examples/dii_echo) which uses the same Echo server.
--

with CORBA;
with CORBA.NVList;
with AdaBroker.OmniProxyCallDesc;
with AdaBroker.GIOP_C;
use CORBA;

package Dynamic_Proxy is

   type Operation_Proxy is
     new AdaBroker.OmniProxyCallDesc.Object with private;
   --  a generic proxy object

   type Operation_Type is (Operation_Function, Operation_Procedure);
   --  used to know if we will have to get one return value (function)
   --  or a list of return values (procedure with OUT/INOUT arguments)

   procedure Init
     (Self : in out Operation_Proxy;
      Op   : in     CORBA.Identifier;
      Args : in     CORBA.NVList.Object;
      Res  : in     NamedValue;
      Ot   : in     Operation_Type);
   --  Inits an operation proxy, given the name of the operation,
   --  the nvlist of arguments, the named value of the return value,
   --  and the type of operation (precedure or function).
   --  Called by CORBA.Request.Invoke

   function Get_Function_Result
     (Self : in Operation_Proxy)
      return NamedValue;
   --  Returns the return value for function operations : retrieve it from
   --  the Operation_Proxy fields.
   --  Called by CORBA.Request.Return_Value

   function Get_Procedure_Result
     (Self     : in Operation_Proxy)
      return NVList.Object;
   --  Returns the OUT/INOUT values for procedure operations
   --  Called by  CORBA.Request.Return_Arguments



   --------------------------------------------------------------
   --  the functions below override the ones of OmniProxyCallDesc
   --------------------------------------------------------------

   function Operation
     (Self : in Operation_Proxy)
      return CORBA.String;
   --  Returns the name of the operation

   function Align_Size
     (Self    : in Operation_Proxy;
      Size_In : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long;
   --  Computes the size needed to marshall the arguments contained
   --  in the NVList

   procedure Marshal_Arguments
     (Self        : in     Operation_Proxy;
      GIOP_Client : in out AdaBroker.GIOP_C.Object);
   --  Marshalling of the arguments of the operation (from the NamedValues)

   procedure Unmarshal_Returned_Values
     (Self        : in out Operation_Proxy;
      GIOP_Client : in out AdaBroker.GIOP_C.Object);
   --  Unmarshall the returned values (to a NamedValue if function, to a
   --  NVList if procedure)

private

   function Align_From_Any
     (A       : in Any;
      Size_In : in Unsigned_Long)
      return Unsigned_Long;
   --  Computes (recusively for complex types) the size needed to marshall
   --  the content of an Any. Called by Align_Size

   procedure Marshall_From_Any
     (A           : in      Any;
      GIOP_Client : in out AdaBroker.GIOP_C.Object);
   --  Marshalls (recursively for complex types) the content of an Any
   --  Called by Marshal_Arguments

   procedure  Unmarshall_To_Any
     (GIOP_Client : in out AdaBroker.GIOP_C.Object;
      A           :    out Any;
      Tc          : in TypeCode.Object);
   --  Creates an Any from the given typecode and the unmarshalled things
   --  Called by Unmarshal_Returned_Values

   type Operation_Proxy is  new AdaBroker.OmniProxyCallDesc.Object with
   --  Generic proxy definition
      record
         Op_Type        : Operation_Type;
         --  function or procedure
         Op_Name        : CORBA.Identifier;
         --  name of the operation
         Args           : NVList.Object;
         --  arguments
         Private_Result : NamedValue;
         --  return value
      end record;

end Dynamic_Proxy;
