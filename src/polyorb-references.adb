------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . R E F E R E N C E S                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Object references.

--  $Id$

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Tags;

with PolyORB.Filters.Interface;
with PolyORB.Log;

package body PolyORB.References is

   use type PolyORB.Components.Component_Access;
   use PolyORB.Log;
   use PolyORB.Smart_Pointers;
   use PolyORB.Utils.Strings;

   package L is new PolyORB.Log.Facility_Log ("polyorb.references");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   type Reference_Info_Access is access all Reference_Info'Class;

   ------------------------
   -- Local declarations --
   ------------------------

   function Ref_Info_Of (R : Ref) return Reference_Info_Access;
   --  Obtain the object reference information from R.

   --  When an object reference is bound (i.e. associated at
   --  runtime with a transport service endpoint and a messaging
   --  protocol stack), it becomes associated with a Binding_Object
   --  which will remain in existence until all references to
   --  the object have been finalized (at which time the transport
   --  connection and protocol stack will be torn down, as a
   --  result of finalizing the binding object).

   type Binding_Object is new Smart_Pointers.Entity with record
      BO_Component : Components.Component_Access;
   end record;

   procedure Finalize (X : in out Binding_Object);

   ----------------------
   -- Create_Reference --
   ----------------------

   procedure Create_Reference
     (Profiles : Profile_Array;
      Type_Id  : String;
      R        : out Ref)
   is
      use type Binding_Data.Profile_Access;
   begin
      if Profiles'Length = 0 then
         Set (R, null);
      else
         for I in Profiles'Range loop
            null;
            pragma Assert (Profiles (I) /= null);
         end loop;

         declare
            RIP : constant Entity_Ptr := new Reference_Info;
            TRIP : Reference_Info renames Reference_Info (RIP.all);
         begin
            TRIP.Type_Id  := new String'(Type_Id);
            TRIP.Profiles := Profile_Seqs.To_Sequence (Profiles);
            Set (R, RIP);
         end;
      end if;

      pragma Debug (O ("New " & Image (R)));

   end Create_Reference;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (RI : in out Reference_Info)
   is
      Profiles : Profile_Array
        := Profile_Seqs.To_Element_Array (RI.Profiles);
   begin
      Free (RI.Type_Id);
      for I in Profiles'Range loop
         pragma Debug
           (O ("Destroying profile of type "
               & Ada.Tags.External_Tag (Profiles (I)'Tag)));
         Binding_Data.Destroy_Profile (Profiles (I));
      end loop;
   end Finalize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (X : in out Binding_Object)
   is
      M : Filters.Interface.Disconnect_Request;
   begin
      pragma Debug (O ("Finalizing binding object"));
      Components.Emit_No_Reply
        (X.BO_Component, M);
   end Finalize;

   ----------------------
   -- Get_Binding_Info --
   ----------------------

   procedure Get_Binding_Info
     (R   :     Ref;
      BOC : out Components.Component_Access;
      Pro : out Binding_Data.Profile_Access)
   is
      RI : constant Reference_Info_Access
        := Ref_Info_Of (R);
      BOP : constant Entity_Ptr := Entity_Of (RI.Binding_Object_Ref);
   begin
      if BOP = null then
         BOC := null;
         Pro := null;
      else
         declare
            BO : Binding_Object renames Binding_Object (BOP.all);
         begin
            pragma Assert (BO.BO_Component /= null);
            BOC := BO.BO_Component;
            Pro := RI.Binding_Object_Profile;
         end;
      end if;
   end Get_Binding_Info;

   -----------
   -- Image --
   -----------

   function Image (R : Ref) return String
   is
      P : constant Profile_Array := Profiles_Of (R);
      Res : Unbounded_String
        := To_Unbounded_String ("Object reference: ");
   begin
      if P'Length = 0 then
         Res := Res & "<nil or invalid reference>";
      else
         Res := Res & Type_Id_Of (R) & ASCII.LF;
         for I in P'Range loop
            Res := Res & "  " & Ada.Tags.External_Tag
              (P (I).all'Tag) & ASCII.LF;
            Res := Res & "    " & Binding_Data.Image (P (I).all)
              & ASCII.LF;
         end loop;
      end if;

      return To_String (Res);
   end Image;

   --------------------
   -- Is_Same_Object --
   --------------------

   function Is_Same_Object (Left, Right : Ref) return Boolean
   is
      use type Profile_Seqs.Sequence;

      Left_RI : constant Reference_Info_Access := Ref_Info_Of (Left);
      Right_RI : constant Reference_Info_Access := Ref_Info_Of (Right);
   begin
      return Left_RI.Type_Id.all = Right_RI.Type_Id.all
        and then Left_RI.Profiles = Right_RI.Profiles;
   end Is_Same_Object;

   -----------------
   -- Profiles_Of --
   -----------------

   function Profiles_Of (R : Ref) return Profile_Array is
      RI : constant Reference_Info_Access := Ref_Info_Of (R);
   begin
      if RI /= null then
         return Profile_Seqs.To_Element_Array (RI.Profiles);
      else
         declare
            Null_Profile_Array : Profile_Array (1 .. 0);
         begin
            return Null_Profile_Array;
         end;
      end if;
   end Profiles_Of;

   -----------------
   -- Ref_Info_Of --
   -----------------

   function Ref_Info_Of (R : Ref) return Reference_Info_Access is
      E : constant Entity_Ptr := Entity_Of (R);
   begin
      if E /= null then
         if E.all in Reference_Info'Class then
            return Reference_Info_Access (E);
         else
            pragma Debug (O ("Ref_Info_Of: entity is a "
                             & Ada.Tags.External_Tag (E'Tag)));
            null;
         end if;
      else
         pragma Debug (O ("Ref_Info_Of: nil ref."));
         null;
      end if;
      return null;
   end Ref_Info_Of;

   ----------------------
   -- Set_Binding_Info --
   ----------------------

   procedure Set_Binding_Info
     (R   : Ref;
      BOC : Components.Component_Access;
      Pro : Binding_Data.Profile_Access)
   is
      RI : constant Reference_Info_Access := Ref_Info_Of (R);
      BOP : constant Entity_Ptr := new Binding_Object;
      BO : Binding_Object renames Binding_Object (BOP.all);
   begin
      pragma Assert (Is_Nil (RI.Binding_Object_Ref));
      pragma Assert (BOC /= null);
      BO.BO_Component := BOC;
      Set (RI.Binding_Object_Ref, BOP);
      RI.Binding_Object_Profile := Pro;
   end Set_Binding_Info;

   ------------------------
   -- Share_Binding_Info --
   ------------------------

   procedure Share_Binding_Info
     (Dest   : Ref;
      Source : Ref)
   is
      RD : constant Reference_Info_Access := Ref_Info_Of (Dest);
      RS : constant Reference_Info_Access := Ref_Info_Of (Source);
   begin
      RD.Binding_Object_Ref := RS.Binding_Object_Ref;
      if RD.Type_Id'Length = 0 then
         Free (RD.Type_Id);
         RD.Type_Id := new String'(RS.Type_Id.all);
      end if;
   end Share_Binding_Info;

   ----------------
   -- Type_Id_Of --
   ----------------

   function Type_Id_Of (R : Ref) return String is
   begin
      return Ref_Info_Of (R).Type_Id.all;
      --  XXX Perhaps some cases of R not designating
      --  a ref_info should be supported here?
   end Type_Id_Of;

end PolyORB.References;
