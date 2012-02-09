------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . C O R B A _ P . A D A P T E R A C T I V A T O R      --
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

pragma Ada_2005;

with CORBA;

package body PolyORB.CORBA_P.AdapterActivator is

   ------------
   -- Create --
   ------------

   procedure Create
     (Self :    out PPT.AdapterActivator_Access;
      AA   : access PortableServer.AdapterActivator.Ref'Class)
   is
      Activator : constant Object_Ptr := new Object;

   begin
      Self := new CORBA_AdapterActivator;
      Activator.AA := AA_Ptr (AA);

      Set (CORBA_AdapterActivator (Self.all),
           PolyORB.Smart_Pointers.Entity_Ptr (Activator));
   end Create;

   ---------------------------
   -- Get_Adapter_Activator --
   ---------------------------

   function Get_Adapter_Activator
     (Self : CORBA_AdapterActivator)
     return PortableServer.AdapterActivator.Ref'Class
   is
      Activator : constant Object_Ptr := Object_Ptr (Entity_Of (Self));

   begin
      return Activator.AA.all;
   end Get_Adapter_Activator;

   ---------------------
   -- Unknown_Adapter --
   ---------------------

   overriding procedure Unknown_Adapter
     (Self   : access CORBA_AdapterActivator;
      Parent : access PPT.Obj_Adapter'Class;
      Name   : String;
      Result :    out Boolean;
      Error  : in out PolyORB.Errors.Error_Container)
   is
      use PolyORB.Errors;

      CORBA_POA : PortableServer.POA_Forward.Ref;

      Activator : constant PortableServer.AdapterActivator.Ref'Class :=
                    Get_Adapter_Activator (Self.all);
   begin
      PortableServer.POA_Forward.Set
        (CORBA_POA,
         PolyORB.Smart_Pointers.Entity_Ptr (Parent));

      Result := PortableServer.AdapterActivator.Unknown_Adapter
        (Activator,
         CORBA_POA,
         CORBA.To_CORBA_String (Name));

   exception
      when others =>
         Result := False;

         Throw (Error,
                Obj_Adapter_E,
                System_Exception_Members'
                (Minor     => 1,
                 Completed => Completed_No));
   end Unknown_Adapter;

end PolyORB.CORBA_P.AdapterActivator;
