------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . R E F E R E N C E S . B I N D I N G            --
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

--  Object references (binding operation).

--  $Id$

with Ada.Exceptions;
with Ada.Tags;

with PolyORB.Binding_Data.Local;
with PolyORB.Components;
with PolyORB.Filters;
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);
with PolyORB.Obj_Adapters;
with PolyORB.Objects;
with PolyORB.ORB;
with PolyORB.Setup;
with PolyORB.Transport;
with PolyORB.Types;

package body PolyORB.References.Binding is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.references.binding");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Bind
     (R         : Ref;
      Local_ORB : ORB.ORB_Access;
      Servant   : out Components.Component_Access;
      Pro       : out Binding_Data.Profile_Access)
   is
      use type Components.Component_Access;
      use Binding_Data;
      use Binding_Data.Local;
      use Obj_Adapters;
      use ORB;
      use Profile_Seqs;

      Profiles : constant Element_Array := Profiles_Of (R);

      Best_Preference : Profile_Preference := Profile_Preference'First;
      Best_Profile_Index : Integer := Profiles'Last + 1;

      Object_Id : PolyORB.Objects.Object_Id_Access;

      Existing_Servant : Components.Component_Access;
      Existing_Profile : Binding_Data.Profile_Access;
   begin
      pragma Debug (O ("Bind: enter"));
      if Is_Nil (R) then
         raise Invalid_Reference;
      end if;

      Get_Binding_Info (R, Existing_Servant, Existing_Profile);
      if Existing_Servant /= null then
         --  The reference is already bound: reuse the binding
         --  object.
         Servant := Existing_Servant;
         Pro := Existing_Profile;
         return;
      end if;

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
         Selected_Profile : Profile_Access
           renames Profiles (Best_Profile_Index);
         OA : constant Obj_Adapter_Access
           := Object_Adapter (Local_ORB);
      begin
         pragma Debug
           (O ("Found profile: " & Ada.Tags.External_Tag
               (Selected_Profile'Tag)));

         if Selected_Profile.all in Local_Profile_Type
           or else Is_Profile_Local (Local_ORB, Selected_Profile)
         then

            --  Local profile

            Object_Id := Get_Object_Key (Selected_Profile.all);

            if Is_Proxy_Oid (OA, Object_Id) then
               begin
                  declare
                     Continuation : constant PolyORB.References.Ref
                       := Proxy_To_Ref (OA, Object_Id);
                  begin
                     if Is_Nil (Continuation) then
                        --  Fail.
                        Servant := null;
                        Pro := null;
                     else
                        Binding_Data.Set_Continuation
                          (Selected_Profile,
                           Smart_Pointers.Ref (Continuation));
                        --  This is necessary in order to prevent the
                        --  profiles in Continuation (a ref to the
                        --  actual object) from being finalised before
                        --  Selected_Profile (a local profile with a
                        --  proxy oid) is finalized itself.
                        pragma Debug (O ("Bind: recursing on proxy ref"));
                        Bind (Continuation, Local_ORB, Servant, Pro);
                        pragma Debug (O ("Recursed."));
                        Set_Binding_Info (R, Servant, Pro);
                        pragma Debug (O ("Cached binding data."));
                     end if;
                     pragma Debug (O ("About to finalize Continuation"));
                  end;
               exception
                  when E : others =>
                     pragma Debug (O ("Argh! Got exception:"));
                     pragma Debug
                       (O (Ada.Exceptions.Exception_Information (E)));
                     null;
               end;
            else
               --  Real local object
               Pro := Selected_Profile;
               Servant := Components.Component_Access
                 (Find_Servant
                  (Object_Adapter (Local_ORB), Object_Id));
               return;

               --  ==> When binding a local reference, an OA
               --      is needed. Where do we obtain it from?
               --      PolyORB.References cannot depend on Obj_Adapters!
               --      ... but D.R.Binding can depend on anything.
               --      We also need to know what profiles are local,
               --      presumably by sending the ORB an Is_Local_Profile
               --      query for each profile (for the condition below).
            end if;

            --  End of processing for local profile case.

            return;

         end if;

         declare
            use PolyORB.Components;
            use PolyORB.Filters;

            New_TE      : Transport.Transport_Endpoint_Access;
            New_Filter  : Filter_Access;
            FU : Filter_Access;
         begin
            pragma Debug (O ("Binding non-local profile"));
            pragma Debug (O ("Creating new binding object"));

            PolyORB.Binding_Data.Bind_Non_Local_Profile
              (Selected_Profile.all, New_TE,
               Component_Access (New_Filter));

            pragma Debug (O ("Registering endpoint"));
            ORB.Register_Endpoint
              (Local_ORB, New_TE, New_Filter, Client);
            pragma Debug (O ("... done"));

            loop
               FU := Filter_Access (Upper (New_Filter));
               exit when FU = null;
               New_Filter := FU;
            end loop;

            pragma Debug (O ("Cacheing binding info"));
            Servant := Components.Component_Access (New_Filter);
            Pro := Selected_Profile;
            Set_Binding_Info (R, Servant, Selected_Profile);
            pragma Debug (O ("... done"));

            --  The Session itself acts as a remote surrogate
            --  of the designated object.

         end;
      end;
   end Bind;

   function Find_Tagged_Profile
     (R      : Ref;
      Tag    : Binding_Data.Profile_Tag;
      Delete : Boolean)
     return Binding_Data.Profile_Access;
   --  Find a profile in R with the specified Tag.
   --  If Delete is true and a matching profile is found,
   --  then the profile is removed from R.

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
         for I in Profiles'Range loop
            if Tag = Get_Profile_Tag (Profiles (I).all) then
               if Delete then
                  Profile_Seqs.Delete
                    (Reference_Info (Entity_Of (R).all).Profiles,
                     I, I);
               end if;
               return Profiles (I);
            end if;
         end loop;

         --  No match.
         return null;

      end;
   end Find_Tagged_Profile;

   procedure Get_Tagged_Profile
     (R         :     Ref;
      Tag       :     Binding_Data.Profile_Tag;
      Pro       : out Binding_Data.Profile_Access)
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

         pragma Debug (O ("Get_Tagged_Profile: creating proxy"));

         declare
            use PolyORB.Obj_Adapters;
            use PolyORB.Objects;
            Proxy_Oid : constant Object_Id_Access
              := To_Proxy_Oid (Object_Adapter (Local_ORB), R);
            Proxy_Ref : References.Ref;
         begin

            if Proxy_Oid /= null then
               Create_Reference
                 (Local_ORB, Proxy_Oid, Type_Id_Of (R), Proxy_Ref);

               --  If Create_Reference has created a ref containing
               --  a profile with the desired tag, move that profile
               --  into R so it won't be destroyed while R is in use.

               Result := Find_Tagged_Profile
                 (Proxy_Ref, Tag, Delete => True);
               pragma Debug (O ("Created a proxy profile."));
            else
               pragma Debug (O ("Could not create proxy oid."));
               null;
            end if;

            if Result /= null then
               Profile_Seqs.Append
                 (Reference_Info (Entity_Of (R).all).Profiles,
                  Result);
            else
               pragma Debug (O ("Could not create proxy."));
               null;
            end if;
         end;
      end if;

      Pro := Result;
   end Get_Tagged_Profile;

end PolyORB.References.Binding;
