------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 PORTABLEINTERCEPTOR.ORBINITINFO.HELPER                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2006 Free Software Foundation, Inc.           --
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

with PolyORB.Exceptions;
with PolyORB.Initialization;
with PolyORB.Utils.Strings;

package body PortableInterceptor.ORBInitInfo.Helper is

   procedure Raise_DuplicateName_From_Any (Item : in PolyORB.Any.Any);
   pragma No_Return (Raise_DuplicateName_From_Any);

   procedure Raise_InvalidName_From_Any (Item : in PolyORB.Any.Any);
   pragma No_Return (Raise_InvalidName_From_Any);

   procedure Deferred_Initialization;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("ObjectId");
         Id   : CORBA.String := CORBA.To_CORBA_String (ObjectId_Repository_Id);
      begin
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_ObjectId, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_ObjectId, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_ObjectId, CORBA.To_Any (CORBA.TC_String));
      end;

      declare
         Name          : CORBA.String
           := CORBA.To_CORBA_String ("DuplicateName");
         Id            : CORBA.String
           := CORBA.To_CORBA_String (DuplicateName_Repository_Id);
         Arg_Name_Name : CORBA.String := CORBA.To_CORBA_String ("name");
      begin
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_DuplicateName, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_DuplicateName, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_DuplicateName, CORBA.To_Any (CORBA.TC_String));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_DuplicateName, CORBA.To_Any (Arg_Name_Name));

         PolyORB.Exceptions.Register_Exception
           (CORBA.TypeCode.Internals.To_PolyORB_Object (TC_DuplicateName),
            Raise_DuplicateName_From_Any'Access);
      end;

      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("InvalidName");
         Id   : CORBA.String
           := CORBA.To_CORBA_String (InvalidName_Repository_Id);
      begin
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_InvalidName, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
          (TC_InvalidName, CORBA.To_Any (Id));

         PolyORB.Exceptions.Register_Exception
           (CORBA.TypeCode.Internals.To_PolyORB_Object (TC_InvalidName),
            Raise_InvalidName_From_Any'Access);
      end;
   end Deferred_Initialization;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : in CORBA.Any) return DuplicateName_Members is
      Index       : CORBA.Any;
      Result_Name : CORBA.String;
   begin
      Index := CORBA.Internals.Get_Aggregate_Element (Item,
                                                      CORBA.TC_String,
                                                      CORBA.Unsigned_Long (0));
      Result_Name := CORBA.From_Any (Index);

      return (Name => Result_Name);
   end From_Any;

   function From_Any (Item : in CORBA.Any) return InvalidName_Members is
      pragma Unreferenced (Item);

      Result : InvalidName_Members;
   begin
      return Result;
   end From_Any;

   function From_Any (Item : in CORBA.Any) return ObjectId is
      Result : constant CORBA.String := CORBA.From_Any (Item);
   begin
      return PortableInterceptor.ORBInitInfo.ObjectId (Result);
   end From_Any;

   -------------------------
   -- Raise_DuplicateName --
   -------------------------

   procedure Raise_DuplicateName (Members : in DuplicateName_Members) is
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (DuplicateName'Identity,
         Members);
   end Raise_DuplicateName;

   ----------------------------------
   -- Raise_DuplicateName_From_Any --
   ----------------------------------

   procedure Raise_DuplicateName_From_Any (Item : in PolyORB.Any.Any) is
      Members : constant DuplicateName_Members
        := From_Any (CORBA.Internals.To_CORBA_Any (Item));
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (DuplicateName'Identity,
         Members);
   end Raise_DuplicateName_From_Any;

   -----------------------
   -- Raise_InvalidName --
   -----------------------

   procedure Raise_InvalidName (Members : in InvalidName_Members) is
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (InvalidName'Identity,
         Members);
   end Raise_InvalidName;

   --------------------------------
   -- Raise_InvalidName_From_Any --
   --------------------------------

   procedure Raise_InvalidName_From_Any (Item : in PolyORB.Any.Any) is
      Members : constant InvalidName_Members
        := From_Any (CORBA.Internals.To_CORBA_Any (Item));
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (InvalidName'Identity,
         Members);
   end Raise_InvalidName_From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : in DuplicateName_Members) return CORBA.Any is
      Result : CORBA.Any
        := CORBA.Internals.Get_Empty_Any_Aggregate (TC_DuplicateName);

   begin
      CORBA.Internals.Add_Aggregate_Element (Result, CORBA.To_Any (Item.Name));
      return Result;
   end To_Any;

   function To_Any (Item : in InvalidName_Members) return CORBA.Any is
      pragma Unreferenced (Item);

      Result : CORBA.Any
        := CORBA.Internals.Get_Empty_Any_Aggregate (TC_InvalidName);

   begin
      return Result;
   end To_Any;

   function To_Any (Item : in ObjectId) return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.String (Item));

   begin
      CORBA.Internals.Set_Type (Result, TC_ObjectId);
      return Result;
   end To_Any;

   ------------------
   -- To_Local_Ref --
   ------------------

   function To_Local_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
      return Local_Ref
   is
   begin
      if CORBA.Object.Is_Nil (The_Ref)
        or else CORBA.Object.Is_A (The_Ref, Repository_Id)
      then
         return Unchecked_To_Local_Ref (The_Ref);
      end if;

      CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
   end To_Local_Ref;

   ----------------------------
   -- Unchecked_To_Local_Ref --
   ----------------------------

   function Unchecked_To_Local_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
      return Local_Ref
   is
      Result : Local_Ref;
   begin
      Set (Result, CORBA.Object.Object_Of (The_Ref));
      return Result;
   end Unchecked_To_Local_Ref;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
   begin
      Register_Module
        (Module_Info'
         (Name      => +"PortableInterceptor.ORBInitInfo.Helper",
          Conflicts => Empty,
          Depends   => +"any"
          & "exceptions",
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access));
   end;
end PortableInterceptor.ORBInitInfo.Helper;
