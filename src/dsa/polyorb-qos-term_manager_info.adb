------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . Q O S . T E R M _ M A N A G E R _ I N F O         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2008, Free Software Foundation, Inc.          --
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

with PolyORB.Binding_Objects;
with PolyORB.Buffers;
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.QoS.Service_Contexts;
with PolyORB.QoS;
with PolyORB.References.IOR;
with PolyORB.Representations.CDR.Common;
with PolyORB.Request_QoS;
with PolyORB.Smart_Pointers;
with PolyORB.Utils.Strings;

package body PolyORB.QoS.Term_Manager_Info is

   use PolyORB.Buffers;
   use PolyORB.Log;
   use PolyORB.QoS.Service_Contexts;
   use PolyORB.References.IOR;
   use PolyORB.References;
   use PolyORB.Representations.CDR.Common;
   use PolyORB.Requests;
   use PolyORB.Tasking.Mutexes;

   -------------
   -- Logging --
   -------------

   package L is new Log.Facility_Log ("polyorb.qos.term_manager_info");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
                renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
               renames L.Enabled;

   ---------------
   -- Callbacks --
   ---------------

   function To_TMInfo_Service_Context (QoS : QoS_Parameter_Access)
     return Service_Context;
   --  Callback to convert a QoS parameter to a TMInfo service context

   function To_QoS_DSA_TM_Info_Parameter (SC : Service_Context)
     return QoS_Parameter_Access;
   --  Callback to convert a TMInfo service context to a QoS parameter

   ---------------------
   -- Extract_TM_Info --
   ---------------------

   procedure Extract_TM_Info (R : access Request)
   is
      use PolyORB.Annotations;
      use PolyORB.Binding_Objects;
      use PolyORB.QoS.Term_Manager_Info;
      use PolyORB.QoS;
      use PolyORB.Request_QoS;

      Note : BO_Note;
      BO : constant Binding_Object_Access :=
             Binding_Object_Access (
               Smart_Pointers.Entity_Of (R.Dependent_Binding_Object));
      QoS_Acc : QoS_Parameter_Access;
   begin

      --  If the Dependant BO of the request is null, this is a request done on
      --  the client side and therefore do not interest us here.

      if BO /= null then
         Enter_BO_Note_Lock;
         Get_Note (Notepad_Of (BO).all, Note, Default_BO_Note);
         Leave_BO_Note_Lock;

         --  If reference is already set, no need to extract info again

         if Is_Nil (Note.TM_Ref) then
            pragma Debug (C, O ("Extracting TM info from request"));

            --  Extract the QoS parameter from the request

            QoS_Acc := Extract_Request_Parameter
              (DSA_TM_Info, Request_Access (R));

            --  Store the reference in the requestor BO's notepad

            Note.TM_Ref := QoS_DSA_TM_Info_Parameter_Access (QoS_Acc).TM_Ref;
            Set_Note (Notepad_Of (BO).all, Note);
         end if;
      end if;
   end Extract_TM_Info;

   -------------------------------
   -- To_TMInfo_Service_Context --
   -------------------------------

   function To_TMInfo_Service_Context (QoS : QoS_Parameter_Access)
     return Service_Context
   is
      Result : Service_Context := (TMInfo, null);
   begin
      if QoS = null then
         return Result;
      end if;

      declare
         TMInfo : QoS_DSA_TM_Info_Parameter
         renames QoS_DSA_TM_Info_Parameter (QoS.all);
         Buffer : Buffer_Access := new Buffer_Type;

      begin
         Start_Encapsulation (Buffer);

         pragma Debug (C, O ("Encapsulate :" & Image (TMInfo.TM_Ref)));

         Marshall_IOR (Buffer, TMInfo.TM_Ref);
         Result.Context_Data := new Encapsulation'(Encapsulate (Buffer));
         Release (Buffer);
      end;

      return Result;
   end To_TMInfo_Service_Context;

   ----------------------------------
   -- To_QoS_DSA_TM_Info_Parameter --
   ----------------------------------

   function To_QoS_DSA_TM_Info_Parameter (SC : Service_Context)
     return QoS_Parameter_Access
   is
      Buffer     : aliased Buffer_Type;
      TM_Ref     : References.Ref;
   begin

      Decapsulate (SC.Context_Data, Buffer'Access);
      TM_Ref := Unmarshall_IOR (Buffer'Access);

      pragma Debug (C, O ("Decapsulate:" & Image (TM_Ref)));

      return new QoS_DSA_TM_Info_Parameter'(Kind       => DSA_TM_Info,
                                            TM_Ref     => TM_Ref);
   end To_QoS_DSA_TM_Info_Parameter;

   ------------------------
   -- Enter_BO_Note_Lock --
   ------------------------

   procedure Enter_BO_Note_Lock is
   begin
      Enter (Lock);
   end Enter_BO_Note_Lock;

   ------------------------
   -- Leave_BO_Note_Lock --
   ------------------------

   procedure Leave_BO_Note_Lock is
   begin
      Leave (Lock);
   end Leave_BO_Note_Lock;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Create (Lock);

      --  Register the TMInfo service context callbacks

      Register (DSA_TM_Info, To_TMInfo_Service_Context'Access);
      Register (TMInfo, To_QoS_DSA_TM_Info_Parameter'Access);
   end Initialize;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
   begin
      Register_Module
        (Module_Info'
           (Name      => +"request_qos.dsa_tm_info",
            Conflicts => Empty,
            Depends   => Empty,
            Provides  => Empty,
            Implicit  => False,
            Init      => Initialize'Access,
            Shutdown  => null));
   end;
end PolyORB.QoS.Term_Manager_Info;
