------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       T E S T . C L I E N T O R B I N I T I A L I Z E R . I M P L        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.Impl;
with PortableInterceptor.ClientRequestInterceptor;

with Test.ClientInterceptor.Impl;

package body Test.ClientORBInitializer.Impl is

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self            : access Object;
      Logical_Type_Id :        Standard.String)
      return Boolean
   is
      pragma Unreferenced (Self);

   begin
      return CORBA.Is_Equivalent
        (Logical_Type_Id,
         Test.ClientORBInitializer.Repository_Id)
        or else CORBA.Is_Equivalent
        (Logical_Type_Id,
         "IDL:omg.org/CORBA/Object:1.0")
        or else CORBA.Is_Equivalent
        (Logical_Type_Id,
         PortableInterceptor.ORBInitializer.Repository_Id);
   end Is_A;

   ---------------
   -- Post_Init --
   ---------------

   procedure Post_Init
     (Self : access Object;
      Info : PortableInterceptor.ORBInitInfo.Local_Ref)
   is
      pragma Unreferenced (Self);

      Client_Ref : PortableInterceptor.ClientRequestInterceptor.Local_Ref;
      Client_Ptr : Test.ClientInterceptor.Impl.Object_Ptr;

   begin
      Client_Ptr := new Test.ClientInterceptor.Impl.Object;
      PortableInterceptor.ClientRequestInterceptor.Set
        (Client_Ref, CORBA.Impl.Object_Ptr (Client_Ptr));
      PortableInterceptor.ORBInitInfo.Add_Client_Request_Interceptor
        (Info, Client_Ref);
   end Post_Init;

end Test.ClientORBInitializer.Impl;
