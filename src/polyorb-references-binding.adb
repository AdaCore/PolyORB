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
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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

with Ada.Tags;

with PolyORB.Binding_Data.Local;
with PolyORB.Components;
with PolyORB.Filters;
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);

with PolyORB.Obj_Adapters;
with PolyORB.ORB;
with PolyORB.Transport;

package body PolyORB.References.Binding is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.references.binding");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Bind
     (R         : Ref;
      Local_ORB : ORB.ORB_Access;
      Servant   : out Components.Component_Access;
      Oid       : out Objects.Object_Id_Access)
   is
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
           := Profiles_Of (R);

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
            POid : constant Objects.Object_Id_Access
              := PolyORB.Binding_Data.Get_Object_Key (P.all);
         begin
            pragma Debug (O ("Found profile: "
                             & Ada.Tags.External_Tag (P'Tag)));
            null;

            Oid := POid;
            if P.all in Local_Profile_Type
              or else Is_Profile_Local (Local_ORB, P) then
               --  Easy case: local profile.
               --  Resolve object id within local object adapter.
               Servant := Components.Component_Access
                 (Find_Servant
                  (Object_Adapter (Local_ORB),
                   Get_Object_Key (P.all)));

               --  ==> When binding a local reference, an OA
               --      is needed. Where do we obtain it from?
               --      PolyORB.References cannot depend on Obj_Adapters!
               --      ... but D.R.Binding can depend on anything.

               --      We also need to know what profiles are local,
               --      presumably by sending the ORB an Is_Local_Profile
               --      query for each profile (for the condition below).

            else
               declare
                  use PolyORB.Components;
                  use PolyORB.Filters;

                  Binding_Object : Components.Component_Access;
                  New_TE      : Transport.Transport_Endpoint_Access;
                  New_Filter  : Filter_Access;
                  FU : Filter_Access;
               begin
                  pragma Debug (O ("Binding profile"));
                  Binding_Object := Binding_Data.Get_Binding_Object (P.all);

                  if Binding_Object /= null then
                     pragma Debug (O ("Using existing binding object"));
                     Servant := Binding_Object;
                  else
                     pragma Debug (O ("Creating new binding object"));

                     PolyORB.Binding_Data.Bind_Profile
                       (P.all, New_TE, Component_Access (New_Filter));
                     ORB.Register_Endpoint
                       (Local_ORB, New_TE, New_Filter, Client);

                     loop
                        FU := Filter_Access (Upper (New_Filter));
                        exit when FU = null;
                        New_Filter := FU;
                     end loop;

                     Set_Binding_Object
                       (P.all, Components.Component_Access (New_Filter));
                     Servant := Components.Component_Access (New_Filter);
                     --  The Session itself acts as a remote surrogate
                     --  of the designated object.

                  end if;
               end;
            end if;

         end;
      end;
   end Bind;

end PolyORB.References.Binding;
