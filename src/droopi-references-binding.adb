--  Object references (binding operation).

--  $Id$

with Ada.Tags;

with Droopi.Binding_Data.Local;
with Droopi.Log;
pragma Elaborate_All (Droopi.Log);

with Droopi.Obj_Adapters;
with Droopi.ORB;

package body Droopi.References.Binding is

   use Droopi.Log;

   package L is new Droopi.Log.Facility_Log ("droopi.references.binding");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   function Bind
     (R         : Ref;
      Local_ORB : ORB.ORB_Access)
     return Objects.Servant_Access is
   begin
      pragma Debug (O ("Bind: enter"));
      if Is_Nil (R) then
         raise Invalid_Reference;
      end if;

      declare
         use Binding_Data;
         use Binding_Data.Local;
         use Obj_Adapters;
         use ORB;
         use Profile_Seqs;

         Profiles : constant Element_Array
           := To_Element_Array (R.Profiles);

         Best_Preference : Profile_Preference
           := Profile_Preference'First;
         Best_Profile_Index : Integer := Profiles'Last + 1;
      begin
         --  XXX should probably rework the two-phase preference
         --  -> bind mechanism, else we could have a case where
         --  one non-local profile is preferred to a local, but
         --  less preferred, profile. On the other hand, this
         --  might be useful because it allows implementation
         --  of All_Calls_Remote simply through prefs fiddling.

         for I in Profiles'Range loop
            declare
               P : constant Profile_Preference
                 := Get_Profile_Preference (Profiles (I).all);
            begin
               if P > Best_Preference then
                  Best_Preference := P;
                  Best_Profile_Index := I;
               end if;
            end;
         end loop;

         if Best_Profile_Index > Profiles'Last
           or else Best_Preference = Profile_Preference'First
         then
            raise Invalid_Reference;
            --  No supported profile found.
         end if;

         declare
            P : Profile_Access
              renames Profiles (Best_Profile_Index);
         begin
            pragma Debug (O ("Found profile: "
                             & Ada.Tags.External_Tag (P'Tag)));
            null;

            if P.all in Local_Profile_Type
              or else Is_Profile_Local (Local_ORB, P) then
               --  Easy case: local profile.
               --  Resolve object id within local object adapter.
               return Find_Servant
                 (Object_Adapter (Local_ORB), Get_Object_Key (P.all));

               --  ==> When binding a local reference, an OA
               --      is needed. Where do we obtain it from?
               --      Droopi.References cannot depend on Obj_Adapters!
               --      ... but D.R.Binding can depend on anything.

               --      We also need to know what profiles are local,
               --      presumably by sending the ORB an Is_Local_Profile
               --      query for each profile (for the condition below).

            else
               null;
               --  XXX TODO!
               --     S := Find_Session (P.Address);
               --     return Make_Surrogate (S);
            end if;

         end;

         --  XXX TODO?!
         raise Not_Implemented;
      end;
   end Bind;

end Droopi.References.Binding;
