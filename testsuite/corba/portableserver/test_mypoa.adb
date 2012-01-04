------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           T E S T _ M Y P O A                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.Impl;
with CORBA.ORB;
with CORBA.Policy;

with PortableServer.POA.Helper;

with PolyORB.Utils.Report;

with Echo.Helper;
with Echo.Impl;

package body Test_MyPOA is

   use CORBA;

   use PolyORB.Utils.Report;

   ------------
   -- To_Ref --
   ------------

   function To_Ref
     (Self : CORBA.Object.Ref'Class)
     return My_POA_Ref
   is
      Result : My_POA_Ref;

   begin
      Set (Result, CORBA.Object.Entity_Of (Self));

      return Result;
   end To_Ref;

   ----------------
   -- Test_MyPOA --
   ----------------

   procedure Run_Test_MyPOA
   is
      MyPOA : My_POA_Ref;

      Root_POA : constant PortableServer.POA.Local_Ref
        := PortableServer.POA.Helper.To_Local_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));

      Policies : CORBA.Policy.PolicyList;

      Obj : constant CORBA.Impl.Object_Ptr := new Echo.Impl.Object;

      Obj_Ref : Echo.Ref;

   begin
      New_Test ("User defined child of POA");

      MyPOA := To_Ref
        (PortableServer.POA.Create_POA
         (Root_POA,
          To_CORBA_String ("My_POA"),
          PortableServer.POA.Get_The_POAManager (Root_POA),
          Policies));

      Output ("Attach user defined child of POA", True);

      Obj_Ref := Echo.Helper.To_Ref
        (PortableServer.POA.Servant_To_Reference
         (PortableServer.POA.Local_Ref (MyPOA), PortableServer.Servant (Obj)));

      Output ("Attach servant", True);

      Output ("Test invocation on servant",
              "Hello Ada World !" =
              To_Standard_String
              (Echo.echoString
               (Obj_Ref, To_CORBA_String ("Hello Ada World !"))));

   end Run_Test_MyPOA;

end Test_MyPOA;
