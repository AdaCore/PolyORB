------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 R T C O R B A . R T O R B . H E L P E R                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
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

with PolyORB.Utils.Strings;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization);
with PolyORB.Exceptions;

package body RTCORBA.RTORB.Helper is

   ----------------------------
   -- Unchecked_To_Local_Ref --
   ----------------------------

   function Unchecked_To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return RTCORBA.RTORB.Local_Ref
   is
      Result : RTCORBA.RTORB.Local_Ref;

   begin
      Set (Result, CORBA.Object.Object_Of (The_Ref));

      return Result;
   end Unchecked_To_Local_Ref;

   ------------------
   -- To_Local_Ref --
   ------------------

   function To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return RTCORBA.RTORB.Local_Ref
   is
   begin
      --        if CORBA.Object.Is_Nil (The_Ref)
      --          or else CORBA.Object.Is_A (The_Ref, Repository_Id)
      --        then
      --           return Unchecked_To_Local_Ref (The_Ref);
      --        end if;
      --        CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);

      if CORBA.Object.Entity_Of (The_Ref).all
        not in RTORB_Object'Class then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      return Unchecked_To_Local_Ref (The_Ref);
   end To_Local_Ref;

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : CORBA.Any)
     return RTCORBA.RTORB.InvalidThreadpool_Members
   is
      pragma Unreferenced (Item);

      Result : InvalidThreadpool_Members;

   begin
      return Result;
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : RTCORBA.RTORB.InvalidThreadpool_Members)
     return CORBA.Any
   is
      pragma Unreferenced (Item);

      Result : CORBA.Any :=
        CORBA.Internals.Get_Empty_Any_Aggregate (TC_InvalidThreadpool);

   begin
      return Result;
   end To_Any;

   --------------------------------------
   -- Raise_InvalidThreadpool_From_Any --
   --------------------------------------

   procedure Raise_InvalidThreadpool_From_Any
     (Item    : PolyORB.Any.Any;
      Message : Standard.String);
   pragma No_Return (Raise_InvalidThreadpool_From_Any);

   procedure Raise_InvalidThreadpool_From_Any
     (Item    : PolyORB.Any.Any;
      Message : Standard.String)
   is
      Members : constant InvalidThreadpool_Members
        := From_Any (CORBA.Internals.To_CORBA_Any (Item));

   begin
      PolyORB.Exceptions.User_Raise_Exception
        (InvalidThreadpool'Identity,
         Members,
         Message);
   end Raise_InvalidThreadpool_From_Any;

   -----------------------------
   -- Raise_InvalidThreadpool --
   -----------------------------

   procedure Raise_InvalidThreadpool
     (Members : InvalidThreadpool_Members)
   is
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (InvalidThreadpool'Identity,
         Members);
   end Raise_InvalidThreadpool;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization;

   procedure Deferred_Initialization is
      Name : CORBA.String
        := CORBA.To_CORBA_String ("InvalidThreadpool");
      Id : CORBA.String
        := CORBA.To_CORBA_String ("IDL:RTCORBA/RTORB/InvalidThreadpool:1.0");
   begin
      CORBA.TypeCode.Internals.Add_Parameter
        (TC_InvalidThreadpool, CORBA.To_Any (Name));
      CORBA.TypeCode.Internals.Add_Parameter
        (TC_InvalidThreadpool, CORBA.To_Any (Id));

      PolyORB.Exceptions.Register_Exception
        (CORBA.TypeCode.Internals.To_PolyORB_Object (TC_InvalidThreadpool),
         Raise_InvalidThreadpool_From_Any'Access);
   end Deferred_Initialization;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"RTCORBA.RTORB.Helper",
       Conflicts => Empty,
       Depends   => +"any"
       & "exceptions",
       Provides  => Empty,
       Implicit  => False,
       Init      => Deferred_Initialization'Access,
       Shutdown  => null));
end RTCORBA.RTORB.Helper;
