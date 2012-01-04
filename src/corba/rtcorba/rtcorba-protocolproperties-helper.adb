------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    R T C O R B A . P R O T O C O L P R O P E R T I E S . H E L P E R     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2007-2012, Free Software Foundation, Inc.          --
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

pragma Style_Checks ("NM32766");

with PolyORB.Utils.Strings;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization);
with PolyORB.Any;

package body RTCORBA.ProtocolProperties.Helper is

   function Unchecked_To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return RTCORBA.ProtocolProperties.Local_Ref
   is
      Result : RTCORBA.ProtocolProperties.Local_Ref;
   begin
      Set (Result,
           CORBA.Object.Object_Of (The_Ref));
      return Result;
   end Unchecked_To_Local_Ref;

   function To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return RTCORBA.ProtocolProperties.Local_Ref
   is
   begin
      if CORBA.Object.Is_Nil (The_Ref)
        or else CORBA.Object.Is_A (The_Ref, Repository_Id) then
         return Unchecked_To_Local_Ref (The_Ref);
      end if;
      CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
   end To_Local_Ref;

   procedure Deferred_Initialization is
   begin

      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("ProtocolProperties");
         Id : constant CORBA.String := CORBA.To_CORBA_String ("IDL:omg.org/RTCORBA/ProtocolProperties:1.0");
      begin
         TC_ProtocolProperties :=
           CORBA.TypeCode.Internals.To_CORBA_Object (PolyORB.Any.TypeCode.TC_Object);
         CORBA.Internals.Add_Parameter (TC_ProtocolProperties, CORBA.To_Any (Name));
         CORBA.Internals.Add_Parameter (TC_ProtocolProperties, CORBA.To_Any (Id));
      end;

   end Deferred_Initialization;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
   begin
      Register_Module
        (Module_Info'
         (Name      => +"RTCORBA.ProtocolProperties.Helper",
          Conflicts => PolyORB.Initialization.String_Lists.Empty,
          Depends   =>
                  +"any"
          ,
          Provides  => PolyORB.Initialization.String_Lists.Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access,
          Shutdown  => null));
   end;

end RTCORBA.ProtocolProperties.Helper;
