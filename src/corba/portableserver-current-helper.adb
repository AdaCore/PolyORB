------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O R T A B L E S E R V E R . C U R R E N T . H E L P E R         --
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

-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC version 2.3.0w.
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
-------------------------------------------------
pragma Style_Checks ("NM32766");

with PolyORB.Utils.Strings;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization);
with PolyORB.Exceptions;
with PolyORB.Std;
with PolyORB.Any;

package body PortableServer.Current.Helper is

   function Unchecked_To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return PortableServer.Current.Local_Ref
   is
      Result : PortableServer.Current.Local_Ref;
   begin
      Set (Result,
           CORBA.Object.Object_Of (The_Ref));
      return Result;
   end Unchecked_To_Local_Ref;

   function To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return PortableServer.Current.Local_Ref
   is
   begin
      if CORBA.Object.Is_Nil (The_Ref)
        or else CORBA.Object.Is_A (The_Ref, Repository_Id) then
         return Unchecked_To_Local_Ref (The_Ref);
      end if;
      CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
   end To_Local_Ref;

   function From_Any (Item : CORBA.Any) return PortableServer.Current.NoContext_Members is
      Result : NoContext_Members;
      pragma Warnings (Off);
      pragma Unreferenced (Item);
      pragma Warnings (On);
   begin
      return Result;
   end From_Any;

   function To_Any
     (Item : PortableServer.Current.NoContext_Members) return CORBA.Any
   is
      Result : constant CORBA.Any :=
                 CORBA.Internals.Get_Empty_Any_Aggregate (TC_NoContext);
      pragma Warnings (Off);
      pragma Unreferenced (Item);
      pragma Warnings (On);
   begin
      return Result;
   end To_Any;

   procedure Raise_NoContext_From_Any
     (Item    : PolyORB.Any.Any;
      Message : PolyORB.Std.String);
   pragma No_Return (Raise_NoContext_From_Any);

   procedure Raise_NoContext_From_Any
     (Item    : PolyORB.Any.Any;
      Message : PolyORB.Std.String)
   is
      Members : constant NoContext_Members := From_Any (CORBA.Any (Item));
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (NoContext'Identity,
         Members,
         Message);
   end Raise_NoContext_From_Any;

   procedure Raise_NoContext
     (Members : NoContext_Members)
   is
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (NoContext'Identity,
         Members);
   end Raise_NoContext;

   procedure Deferred_Initialization is
   begin

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("Current");
         Id   : constant CORBA.String :=
                  CORBA.To_CORBA_String
                    ("IDL:omg.org/PortableServer/Current:1.0");
      begin
         TC_Current :=
           CORBA.TypeCode.Internals.To_CORBA_Object (PolyORB.Any.TypeCode.TC_Object);
         CORBA.Internals.Add_Parameter (TC_Current, CORBA.To_Any (Name));
         CORBA.Internals.Add_Parameter (TC_Current, CORBA.To_Any (Id));
      end;

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("NoContext");
         Id   : constant CORBA.String :=
                  CORBA.To_CORBA_String
                    ("IDL:omg.org/PortableServer/Current/NoContext:1.0");
      begin
         TC_NoContext :=
           CORBA.TypeCode.Internals.To_CORBA_Object (PolyORB.Any.TypeCode.TC_Except);
         CORBA.Internals.Add_Parameter (TC_NoContext, CORBA.To_Any (Name));
         CORBA.Internals.Add_Parameter (TC_NoContext, CORBA.To_Any (Id));
      end;
      PolyORB.Exceptions.Register_Exception
        (CORBA.TypeCode.Internals.To_PolyORB_Object (TC_NoContext),
         Raise_NoContext_From_Any'Access);

   end Deferred_Initialization;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
   begin
      Register_Module
        (Module_Info'
         (Name      => +"PortableServer.Current.Helper",
          Conflicts => PolyORB.Initialization.String_Lists.Empty,
          Depends   =>
                  +"any"
                  & "exceptions"
          ,
          Provides  => PolyORB.Initialization.String_Lists.Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access,
          Shutdown  => null));
   end;

end PortableServer.Current.Helper;
