------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               R T C O S S C H E D U L I N G . H E L P E R                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2006 Free Software Foundation, Inc.           --
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
with PolyORB.Exceptions;
with PolyORB.Any;
with CORBA;

package body RTCosScheduling.Helper is

   --------------
   -- From_Any --
   --------------

   function From_Any
     (Item : in CORBA.Any)
     return RTCosScheduling.UnknownName_Members
   is
      pragma Unreferenced (Item);

      Result : UnknownName_Members;

   begin
      return Result;
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any
     (Item : in RTCosScheduling.UnknownName_Members)
     return CORBA.Any
   is
      pragma Unreferenced (Item);

      Result : CORBA.Any :=
        CORBA.Internals.Get_Empty_Any_Aggregate (TC_UnknownName);

   begin
      return Result;
   end To_Any;

   --------------------------------
   -- Raise_UnknownName_From_Any --
   --------------------------------

   procedure Raise_UnknownName_From_Any
     (Item : in PolyORB.Any.Any);
   pragma No_Return (Raise_UnknownName_From_Any);

   procedure Raise_UnknownName_From_Any
     (Item : in PolyORB.Any.Any)
   is
      Members : constant UnknownName_Members
        := From_Any (CORBA.Internals.To_CORBA_Any (Item));
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (UnknownName'Identity,
         Members);
   end Raise_UnknownName_From_Any;

   -----------------------
   -- Raise_UnknownName --
   -----------------------

   procedure Raise_UnknownName (Members : in UnknownName_Members) is
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (UnknownName'Identity,
         Members);
   end Raise_UnknownName;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization;

   procedure Deferred_Initialization is
   begin

      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("UnknownName");
         Id : CORBA.String := CORBA.To_CORBA_String
           ("IDL:RTCosScheduling/UnknownName:1.0");
      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_UnknownName, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_UnknownName, CORBA.To_Any (Id));
      end;

      PolyORB.Exceptions.Register_Exception
        (CORBA.TypeCode.Internals.To_PolyORB_Object (TC_UnknownName),
         Raise_UnknownName_From_Any'Access);
   end Deferred_Initialization;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
   begin
      Register_Module
        (Module_Info'
         (Name      => +"RTCosScheduling.Helper",
          Conflicts => Empty,
          Depends   => +"any"
          & "exceptions",
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access));
   end;
end RTCosScheduling.Helper;
