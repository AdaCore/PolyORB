------------------------------------------------------------------------------
--                                                                          --
--                           ADABROKER SERVICES                             --
--                                                                          --
--                            F I L E . I M P L                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
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

with CORBA;
pragma Elaborate_All (CORBA);
with CORBA.ORB;
pragma Elaborate_All (CORBA.ORB);
with PortableServer.POA;
pragma Elaborate_All (PortableServer.POA);

with File.Skel;
pragma Elaborate (File.Skel);
pragma Warnings (Off, File.Skel);
with File.Helper;

package body File.Impl is

   type File_Ptr is access all Object'Class;

   Root_POA_String : constant CORBA.String
     := CORBA.To_CORBA_String ("RootPOA");

   Root_POA : constant PortableServer.POA.Ref
     := PortableServer.POA.To_Ref
          (CORBA.ORB.Resolve_Initial_References
           (CORBA.ORB.ObjectId (Root_POA_String)));

   function New_File
     return File.Ref
   is
      Obj : constant File_Ptr
        := new Object;

      Oid : constant PortableServer.ObjectId
        := PortableServer.POA.Activate_Object
        (Root_POA, PortableServer.Servant (Obj));
      pragma Warnings (Off, Oid);
      --  Not referenced (created in order to
      --  evaluate the effects of Activate_Object).
   begin
      return File.Helper.To_Ref
        (PortableServer.POA.Servant_To_Reference
         (Root_POA, PortableServer.Servant (Obj)));
   end New_File;

   function get_Image
     (Self : access Object)
     return CORBA.String is
   begin
      return Self.Image;
   end get_Image;

   procedure set_Image
     (Self : access Object;
      To : in CORBA.String) is
   begin
      Self.Image := To;
   end set_Image;

end File.Impl;
