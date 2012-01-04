------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            F I L E . I M P L                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

with CORBA.ORB;
pragma Elaborate_All (CORBA.ORB);
with PortableServer.POA.Helper;
pragma Elaborate_All (PortableServer.POA);

with File.Skel;
pragma Warnings (Off, File.Skel);
with File.Helper;

package body File.Impl is

   type File_Ptr is access all Object'Class;

   Root_POA_String : constant CORBA.String
     := CORBA.To_CORBA_String ("RootPOA");

   Root_POA : PortableServer.POA.Local_Ref;

   function Get_Root_POA return PortableServer.POA.Local_Ref;

   function Get_Root_POA return PortableServer.POA.Local_Ref is
   begin
      if PortableServer.POA.Is_Nil (Root_POA) then
         Root_POA := PortableServer.POA.Helper.To_Local_Ref
          (CORBA.ORB.Resolve_Initial_References
           (CORBA.ORB.ObjectId (Root_POA_String)));
      end if;
      return Root_POA;
   end Get_Root_POA;

   function New_File
     return File.Ref
   is
      Obj : constant File_Ptr
        := new Object;

      Oid : constant PortableServer.ObjectId
        := PortableServer.POA.Activate_Object
        (Get_Root_POA, PortableServer.Servant (Obj));
      pragma Warnings (Off, Oid);
      --  Not referenced (created in order to
      --  evaluate the effects of Activate_Object).
   begin
      return File.Helper.To_Ref
        (PortableServer.POA.Servant_To_Reference
         (Get_Root_POA, PortableServer.Servant (Obj)));
   end New_File;

   function get_Image
     (Self : access Object)
     return CORBA.String is
   begin
      return Self.Image;
   end get_Image;

   procedure set_Image
     (Self : access Object;
      To : CORBA.String) is
   begin
      Self.Image := To;
   end set_Image;

end File.Impl;
