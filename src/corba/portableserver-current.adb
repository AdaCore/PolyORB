------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O R T A B L E S E R V E R . C U R R E N T                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2004 Free Software Foundation, Inc.           --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with PolyORB.CORBA_P.Initial_References;

with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Smart_Pointers;
with PolyORB.Utils.Strings.Lists;

package body PortableServer.Current is

   function Create return CORBA.Object.Ref;

   ------------
   -- Create --
   ------------

   function Create return CORBA.Object.Ref
   is
      Result : Ref;

      Current : constant PolyORB.Smart_Pointers.Entity_Ptr :=
        new Current_Object;

   begin
      Current_Object (Current.all).Thread :=
        PolyORB.Tasking.Threads.Current_Task;

      Set (Result, Current);

      return CORBA.Object.Ref (Result);
   end Create;

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Obj             : access Current_Object;
      Logical_Type_Id : in     Standard.String)
     return Boolean
   is
      pragma Unreferenced (Obj);
   begin
      return CORBA.Is_Equivalent
          (Logical_Type_Id,
           "IDL:omg.org/PortableServer/Current:1.0")
        or else CORBA.Is_Equivalent
          (Logical_Type_Id,
           "IDL:omg.org/CORBA/Object:1.0");
   end Is_A;

   ------------
   -- To_Ref --
   ------------

   function To_Ref
     (Self : CORBA.Object.Ref'Class)
     return Ref
   is
      Result : Ref;

   begin
      if CORBA.Object.Entity_Of (Self).all
        not in Current_Object'Class then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      Set (Result, CORBA.Object.Entity_Of (Self));

      return Result;
   end To_Ref;

   -------------
   -- Get_POA --
   -------------

   function Get_POA
     (Self : Ref)
     return PortableServer.POA_Forward.Ref is
   begin
      pragma Warnings (Off);
      return Get_POA (Self);
      pragma Warnings (On);
   end Get_POA;

   -------------------
   -- Get_Object_Id --
   -------------------

   function Get_Object_Id
     (Self : Ref)
     return ObjectId is
   begin
      pragma Warnings (Off);
      return Get_Object_Id (Self);
      pragma Warnings (On);
   end Get_Object_Id;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out NoContext_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= NoContext'Identity then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      To := NoContext_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   ---------------------
   -- Raise_NoContext --
   ---------------------

   procedure Raise_NoContext
     (Excp_Memb : in NoContext_Members)
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Excp_Memb);
      pragma Warnings (On); --  WAG:3.15

   begin
      raise NoContext;
   end Raise_NoContext;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize
   is
      use PolyORB.CORBA_P.Initial_References;

   begin
      Register_Initial_Reference ("POACurrent", Create'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Utils.Strings;
   use PolyORB.Utils.Strings.Lists;

begin
   Register_Module
     (Module_Info'
      (Name      => +"portableserver.current",
       Conflicts => Empty,
       Depends   => +"corba.initial_references",
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access));
end PortableServer.Current;
