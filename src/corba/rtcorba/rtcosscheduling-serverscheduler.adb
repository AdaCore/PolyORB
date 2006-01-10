------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      R T C O S S C H E D U L I N G . S E R V E R S C H E D U L E R       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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

with RTCosScheduling.ServerScheduler.Impl;
with CORBA.Policy;
with PortableServer.POAManager;
with PortableServer.POA;
with CORBA.Object;

package body RTCosScheduling.ServerScheduler is

   ----------------
   -- Create_POA --
   ----------------

   function Create_POA
     (Self         : Local_Ref;
      Parent       : PortableServer.POA.Ref;
      Adapter_Name : CORBA.String;
      A_POAManager : PortableServer.POAManager.Ref;
      Policies     : CORBA.Policy.PolicyList)
     return PortableServer.POA.Ref
   is
      Self_Ref : CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return RTCosScheduling.ServerScheduler.Impl.Create_POA
        (RTCosScheduling.ServerScheduler.Impl.Object_Ptr (Entity_Of (Self)),
         Parent,
         Adapter_Name,
         A_POAManager,
         Policies);
   end Create_POA;

   ---------------------
   -- Schedule_Object --
   ---------------------

   procedure Schedule_Object
     (Self : Local_Ref;
      Obj  : CORBA.Object.Ref;
      Name : CORBA.String)
   is
      Self_Ref : CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      RTCosScheduling.ServerScheduler.Impl.Schedule_Object
        (RTCosScheduling.ServerScheduler.Impl.Object_Ptr (Entity_Of (Self)),
         Obj,
         Name);
   end Schedule_Object;

end RTCosScheduling.ServerScheduler;
