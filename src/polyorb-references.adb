--  Object references.

--  $Id$

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Tags;
with Ada.Unchecked_Deallocation;

with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);

package body PolyORB.References is

   use PolyORB.Log;
   use PolyORB.Smart_Pointers;

   package L is new PolyORB.Log.Facility_Log ("polyorb.references");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Create_Reference
     (Profiles : Profile_Array;
      Type_Id  : String;
      R        : out Ref) is
   begin
      if Profiles'Length = 0 then
         Set (R, null);
      else
         declare
            RIP : constant Entity_Ptr := new Reference_Info;
            TRIP : Reference_Info renames Reference_Info (RIP.all);
         begin
            TRIP.Type_Id  := new String'(Type_Id);
            TRIP.Profiles := Profile_Seqs.To_Sequence (Profiles);
            Set (R, RIP);
         end;
      end if;
   end Create_Reference;

   function Profiles_Of (R : Ref) return Profile_Array
   is
      RIP : constant Entity_Ptr
        := Entity_Of (R);
   begin
      if RIP = null
        or else not (RIP.all in Reference_Info'Class) then
         raise Constraint_Error;
         --  XXX should this be allowed? (implicit export)
      end if;

      return Profile_Seqs.To_Element_Array
        (Reference_Info (RIP.all).Profiles);
   end Profiles_Of;

   function Type_Id_Of (R : Ref) return String
   is
      RIP : constant Entity_Ptr
        := Entity_Of (R);
   begin
      if RIP = null
        or else not (RIP.all in Reference_Info'Class) then
         raise Constraint_Error;
         --  XXX should this be allowed? (denoted entity
         --  could have an intrinsic type).
      end if;

      return Reference_Info (RIP.all).Type_Id.all;
   end Type_Id_Of;

   function Image (R : Ref) return String
   is
      P : constant Profile_Array
        := Profiles_Of (R);
      Res : Unbounded_String
        := To_Unbounded_String ("Object reference:" & ASCII.LF);
   begin
      for I in P'Range loop
         Res := Res & "  " & Ada.Tags.External_Tag
           (P (I).all'Tag) & ASCII.LF;
         Res := Res & "    " & Binding_Data.Image (P (I).all)
           & ASCII.LF;
      end loop;

      return To_String (Res);
   end Image;

   procedure Free is new Ada.Unchecked_Deallocation
     (String, String_Ptr);

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
         Destroy_Profile (Profiles (I));
      end loop;
   end Finalize;

end PolyORB.References;
