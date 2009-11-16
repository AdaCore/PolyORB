------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . R E F E R E N C E S . B I N D I N G            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2009, Free Software Foundation, Inc.          --
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

--  Object references (binding operation).

with Ada.Tags;

with PolyORB.Binding_Data.Local;
with PolyORB.Binding_Objects;
with PolyORB.Log;
with PolyORB.Obj_Adapters;
with PolyORB.Objects;
with PolyORB.Setup;
with PolyORB.Servants;
with PolyORB.Types;

package body PolyORB.References.Binding is

   use PolyORB.Binding_Data;
   use PolyORB.Errors;
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.references.binding");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   function Find_Tagged_Profile
     (R      : Ref;
      Tag    : Binding_Data.Profile_Tag;
      Delete : Boolean) return Binding_Data.Profile_Access;
   --  Find a profile in R with the specified Tag.
   --  If Delete is true and a matching profile is found,
   --  then the profile is removed from R.

   ----------
   -- Bind --
   ----------

   procedure Bind
     (R          :        Ref'Class;
      Local_ORB  :        ORB.ORB_Access;
      QoS        :        PolyORB.QoS.QoS_Parameters;
      Servant    :    out Components.Component_Access;
      Pro        :    out Binding_Data.Profile_Access;
      Local_Only :        Boolean;
      Error      : in out PolyORB.Errors.Error_Container)
   is
      use type Components.Component_Access;
      use Binding_Data.Local;
      use Binding_Objects;
      use Obj_Adapters;
      use ORB;

      Selected_Profile : Profile_Access;

      Object_Id : PolyORB.Objects.Object_Id_Access;

      Existing_Servant : Components.Component_Access;
      Existing_Profile : Binding_Data.Profile_Access;
      Existing_BO      : PolyORB.Smart_Pointers.Ref;

      Best_Profile_Is_Local : Boolean;

   begin
      pragma Debug (C, O ("Bind: enter"));

      if Is_Nil (R) then
         Throw (Error,
                Inv_Objref_E,
                System_Exception_Members'(Minor => 0,
                                          Completed => Completed_No));
         return;
      end if;

      --  Initial values: failure.
      Servant := null;
      Pro := null;

      --  First check whether the reference is already bound,
      --  and in that case reuse the binding object.

      pragma Debug (C, O ("Bind: Check for already bound reference"));

      Get_Binding_Info (R, QoS, Existing_Servant, Existing_Profile);

      if Existing_Servant /= null then
         if (not Local_Only)
           or else Existing_Profile.all in Local_Profile_Type
           or else Is_Profile_Local (Local_ORB, Existing_Profile)
         then
            Servant := Existing_Servant;
            Pro := Existing_Profile;
            pragma Debug (C, O ("Bind: The reference is already bound"));
         end if;

         return;
      end if;

      --  XXX should probably rework the two-phase preference
      --  -> bind mechanism, else we could have a case where
      --  one non-local profile is preferred to a local, but
      --  less preferred, profile. On the other hand, this
      --  might be useful because it allows implementation
      --  of All_Calls_Remote simply through prefs fiddling.

      Selected_Profile := Get_Preferred_Profile (R, False);

      if Selected_Profile = null then
         Throw (Error,
                Inv_Objref_E,
                System_Exception_Members'(Minor => 0,
                                          Completed => Completed_No));

         return;
      end if;

      Best_Profile_Is_Local :=
        Selected_Profile.all in Local_Profile_Type
          or else Is_Profile_Local (Local_ORB, Selected_Profile);

      --  Check if there is a binding object which we can reuse (remote case)

      if not Best_Profile_Is_Local then
         pragma Debug (C, O ("Bind: Check for reusable BO"));
         Existing_BO := Find_Reusable_Binding_Object
                          (Local_ORB, Selected_Profile, QoS);

         if not Smart_Pointers.Is_Nil (Existing_BO) then
            Pro := Selected_Profile;
            Servant := Get_Component (Existing_BO);
            pragma Debug (C, O ("Bind: Found reusable BO for reference"));
            return;
         end if;
      end if;

      --  No reusable binding object found

      declare
         use PolyORB.Objects;

         OA_Entity : constant PolyORB.Smart_Pointers.Entity_Ptr :=
                       Get_OA (Selected_Profile.all);
         OA        : constant Obj_Adapter_Access :=
                       Obj_Adapter_Access (OA_Entity);

         S : PolyORB.Servants.Servant_Access;
      begin
         pragma Debug
           (C, O ("Found profile: " & Ada.Tags.External_Tag
               (Selected_Profile'Tag)));

         if Best_Profile_Is_Local then

            --  Local profile

            Object_Id := Get_Object_Key (Selected_Profile.all);

            if Object_Id = null then
               pragma Debug (C, O ("Unable to locate object"));
               return;
            end if;

            if not Is_Proxy_Oid (OA, Object_Id) then

               --  Real local object

               Find_Servant (OA, Object_Id, S, Error);

               if Found (Error) then
                  return;
               end if;

               Pro := Selected_Profile;
               Servant := Components.Component_Access (S);
               return;

               --  ==> When binding a local reference, an OA
               --      is needed. Where do we obtain it from?
               --      PolyORB.References cannot depend on Obj_Adapters!
               --      ... but P.R.Binding can depend on anything.
               --      We also need to know what profiles are local,
               --      presumably by sending the ORB an Is_Local_Profile
               --      query for each profile (for the condition below).
            end if;

            if Local_Only then
               return;
            end if;

            declare
               Continuation : PolyORB.References.Ref;
            begin
               Proxy_To_Ref (OA, Object_Id, Continuation, Error);

               if Found (Error) then
                  return;
               end if;

               if not Is_Nil (Continuation) then

                  --  Record a reference to Continuation in Selected_Profile.
                  --  This is necessary in order to prevent the profiles in
                  --  Continuation (a ref to the actual object) from being
                  --  finalized before Selected_Profile (a local profile with
                  --  proxy oid) is finalized itself.

                  Binding_Data.Set_Continuation
                    (Selected_Profile,
                     Smart_Pointers.Ref (Continuation));

                  pragma Debug (C, O ("Bind: recursing on proxy ref"));
                  Bind (Continuation,
                        Local_ORB,
                        QoS,
                        Servant,
                        Pro,
                        Local_Only,
                        Error);
                  if Found (Error) then
                     return;
                  end if;

                  pragma Debug (C, O ("Recursed."));

                  Share_Binding_Info (Dest => Ref (R), Source => Continuation);
                  pragma Debug (C, O ("Cached binding data."));
               end if;
               pragma Debug (C, O ("About to finalize Continuation"));
            end;

            --  End of processing for local profile case.

            return;

         end if;

         if Local_Only then
            return;
         end if;

         declare
            RI : constant Reference_Info_Access := Ref_Info_Of (R);
            BO : Smart_Pointers.Ref;

         begin
            pragma Debug (C, O ("Binding non-local profile"));
            pragma Debug (C, O ("Creating new binding object"));

            PolyORB.Binding_Data.Bind_Profile
              (Selected_Profile,
               Components.Component_Access (Local_ORB),
               QoS,
               BO,
               Error);
            --  The Session itself acts as a remote surrogate
            --  of the designated object.

            if Found (Error) then
               return;
            end if;

            Binding_Info_Lists.Append
              (RI.Binding_Info, (BO, Selected_Profile));

            Servant := Get_Component (BO);
            Pro     := Selected_Profile;
            pragma Debug (C, O ("... done"));
         end;
      end;
   end Bind;

   -------------------------
   -- Find_Tagged_Profile --
   -------------------------

   function Find_Tagged_Profile
     (R      : Ref;
      Tag    : Binding_Data.Profile_Tag;
      Delete : Boolean)
     return Binding_Data.Profile_Access
   is
      use type PolyORB.Types.Unsigned_Long;

   begin
      if Is_Nil (R) then
         return null;
      end if;

      declare
         Profiles : constant Profile_Array := Profiles_Of (R);
      begin
         for J in Profiles'Range loop
            if Tag = Get_Profile_Tag (Profiles (J).all) then

               if Delete then
                  declare
                     New_Array : constant Profile_Array_Access :=
                       new Profile_Array (Profiles'First .. Profiles'Last - 1);
                  begin
                     New_Array (New_Array'First .. New_Array'First + J - 1)
                       := Profiles (Profiles'First .. Profiles'First + J - 1);
                     New_Array (New_Array'First + J .. New_Array'Last)
                       := Profiles (Profiles'First + J + 1 .. Profiles'Last);
                     Free (Reference_Info (Entity_Of (R).all).Profiles);
                     Reference_Info (Entity_Of (R).all).Profiles := New_Array;
                  end;
               end if;

               return Profiles (J);
            end if;
         end loop;

         --  No match.
         return null;

      end;
   end Find_Tagged_Profile;

   ---------------------------
   -- Get_Preferred_Profile --
   ---------------------------

   function Get_Preferred_Profile
     (R            : Ref'Class;
      Ignore_Local : Boolean)
      return Binding_Data.Profile_Access
   is
      Profiles : constant Profile_Array := Profiles_Of (R);

      Best_Profile_Index : Integer            := Profiles'Last + 1;
      Best_Preference    : Profile_Preference := Profile_Preference'First;

   begin
      for J in Profiles'Range loop
         if not Ignore_Local
           or else Profiles (J).all
                             not in Binding_Data.Local.Local_Profile_Type
         then
            declare
               P : constant Profile_Preference
                 := Get_Profile_Preference (Profiles (J).all);

            begin
               if P > Best_Preference then
                  Best_Preference := P;
                  Best_Profile_Index := J;
               end if;
            end;
         end if;
      end loop;

      if Best_Profile_Index > Profiles'Last
        or else Best_Preference = Profile_Preference'First
      then
         return null;

      else
         return Profiles (Best_Profile_Index);
      end if;
   end Get_Preferred_Profile;

   ------------------------
   -- Get_Tagged_Profile --
   ------------------------

   procedure Get_Tagged_Profile
     (R         :        Ref;
      Tag       :        Binding_Data.Profile_Tag;
      Pro       :    out Binding_Data.Profile_Access;
      Error     : in out PolyORB.Errors.Error_Container)
   is
      use PolyORB.ORB;
      use type PolyORB.Types.Unsigned_Long;

      Local_ORB : ORB_Access renames Setup.The_ORB;

      Result : Binding_Data.Profile_Access
        := Find_Tagged_Profile (R, Tag, Delete => False);

   begin
      if Result = null then
         --  This ref has no profile with that tag:
         --  try to create one.

         pragma Debug (C, O ("Get_Tagged_Profile: creating proxy"));

         declare
            use PolyORB.Obj_Adapters;
            use PolyORB.Objects;

            Proxy_Oid : Object_Id_Access;

            Proxy_Ref : References.Ref;
         begin
            To_Proxy_Oid (Object_Adapter (Local_ORB), R, Proxy_Oid, Error);

            if Found (Error) then
               return;
            end if;

            if Proxy_Oid /= null then
               Create_Reference
                 (Local_ORB, Proxy_Oid, Type_Id_Of (R), Proxy_Ref);

               --  If Create_Reference has created a ref containing
               --  a profile with the desired tag, move that profile
               --  into R so it won't be destroyed while R is in use.

               Result := Find_Tagged_Profile
                 (Proxy_Ref, Tag, Delete => True);
               pragma Debug (C, O ("Created a proxy profile."));
            else
               pragma Debug (C, O ("Could not create proxy oid."));
               null;
            end if;

            if Result /= null then
               declare
                  Profiles : Profile_Array renames
                    Reference_Info (Entity_Of (R).all).Profiles.all;
                  New_Array : constant Profile_Array_Access :=
                    new Profile_Array (Profiles'First .. Profiles'Last + 1);
               begin
                  New_Array (New_Array'First .. New_Array'Last - 1)
                    := Profiles (Profiles'First .. Profiles'Last);
                  New_Array (New_Array'Last) := Result;
                  Free (Reference_Info (Entity_Of (R).all).Profiles);
                  Reference_Info (Entity_Of (R).all).Profiles := New_Array;
               end;
            else
               pragma Debug (C, O ("Could not create proxy."));
               null;
            end if;
         end;
      end if;

      Pro := Result;
   end Get_Tagged_Profile;

   ------------
   -- Unbind --
   ------------

   procedure Unbind (R : Ref'Class) is
      use Binding_Objects;
      use Smart_Pointers;

      RI : constant Reference_Info_Access := Ref_Info_Of (R);

   begin
      if RI /= null then
         Binding_Info_Lists.Deallocate (RI.Binding_Info);
      end if;
   end Unbind;

end PolyORB.References.Binding;
