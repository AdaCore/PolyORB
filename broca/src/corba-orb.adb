------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                            C O R B A . O R B                             --
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

with Broca.Buffers; use Broca.Buffers;
with Broca.IOR;
with Broca.ORB;

with CORBA.Object;
with CORBA.NVList;
with CORBA.Impl;

with Broca.Debug;
pragma Elaborate (Broca.Debug);

package body CORBA.ORB is

   Flag : constant Natural := Broca.Debug.Is_Active ("corba.orb");
   procedure O is new Broca.Debug.Output (Flag);

   ----------------------
   -- String_To_Object --
   ----------------------

   procedure String_To_Object
     (From : in CORBA.String;
      To   : out CORBA.Object.Ref'Class)
   is
      Data : aliased Encapsulation
        := Broca.IOR.IOR_String_To_Octets (From);
      Data_Buffer : aliased Buffer_Type;
   begin
      pragma Debug (O ("String_To_Object : enter"));

      Decapsulate (Data'Access, Data_Buffer'Access);
      Broca.ORB.IOR_To_Object (Data_Buffer'Access, To);
      Release (Data_Buffer);

      if CORBA.Object.Is_Nil (To) then
         pragma Debug (O ("String_To_Object : null object returned"));
         null;
      end if;

   end String_To_Object;

   function List_Initial_Services return ObjectIdList
     renames Broca.ORB.List_Initial_Services;

   function Resolve_Initial_References
     (Identifier : ObjectId)
     return CORBA.Object.Ref'Class
     renames Broca.ORB.Resolve_Initial_References;

   procedure Run renames Broca.ORB.Run;

   ----------------------------------
   --  Dynamic Invocation Related  --
   ----------------------------------

   -----------------
   -- Create_List --
   -----------------
   procedure Create_List
     (Count    : in     CORBA.Long;
      New_List :    out CORBA.NVList.Ref) is
   begin
      CORBA.NVList.Set
        (New_List,
         CORBA.Impl.Object_Ptr (CORBA.NVList.Create_Object));
   end Create_List;

end CORBA.ORB;



