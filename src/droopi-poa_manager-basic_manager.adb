with Droopi.CORBA_P.Exceptions;
with Droopi.Log;
pragma Elaborate_All (Droopi.Log);

package body Droopi.POA_Manager.Basic_Manager is

   use Droopi.Locks;
   use Droopi.CORBA_P.Exceptions;
   use Droopi.Log;
   use Requests_Queue_P;

   package L is new Droopi.Log.Facility_Log
     ("droopi.poa_manager.basic_manager");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Do_Wait_For_Completion (Self : access Basic_POA_Manager);
   --  Wait for completion

   procedure Do_Etherealize_Objects (Self : access Basic_POA_Manager);
   --  Etherealize the objects of the associated POAs
   --  (in case a Servant Manager is used with a RETAIN policy)

   procedure Destroy_If_Unused (Self : access Basic_POA_Manager);
   --  Destroy the POAManager if it is no longer used by any POA,
   --  and the POAManager has been created only for

   --------------
   -- Activate --
   --------------

   procedure Activate
     (Self : access Basic_POA_Manager)
   is
   begin
      pragma Debug (O ("Activate POAManager"));
      Lock_W (Self.State_Lock);
      if Self.Current_State = INACTIVE then
         Unlock_W (Self.State_Lock);
         Raise_Adapter_Inactive;
      else
         Self.Current_State := ACTIVE;
      end if;
      Unlock_W (Self.State_Lock);
   end Activate;

   -------------------
   -- Hold_Requests --
   -------------------

   procedure Hold_Requests
     (Self                : access Basic_POA_Manager;
      Wait_For_Completion :        Boolean)
   is
   begin
      pragma Debug (O ("Hold requests, Wait_For_Completion is "
                       & Wait_For_Completion'Img));
      Lock_W (Self.State_Lock);
      if Self.Current_State = INACTIVE then
         Unlock_W (Self.State_Lock);
         Raise_Adapter_Inactive;
      else
         Self.Current_State := HOLDING;
         null;
      end if;
      Unlock_W (Self.State_Lock);

      if Wait_For_Completion then
         Do_Wait_For_Completion (Self);
      end if;
   end Hold_Requests;

   ----------------------
   -- Discard_Requests --
   ----------------------

   procedure Discard_Requests
     (Self                : access Basic_POA_Manager;
      Wait_For_Completion :        Boolean)
   is
   begin
      pragma Debug (O ("Discard requests, Wait_For_Completion is "
                       & Wait_For_Completion'Img));
      Lock_W (Self.State_Lock);
      if Self.Current_State = INACTIVE then
         Unlock_W (Self.State_Lock);
         Raise_Adapter_Inactive;
      else
         Self.Current_State := DISCARDING;
         null;
      end if;
      Unlock_W (Self.State_Lock);

      if Wait_For_Completion then
         Do_Wait_For_Completion (Self);
      end if;
   end Discard_Requests;

   ----------------
   -- Deactivate --
   ----------------

   procedure Deactivate
     (Self                : access Basic_POA_Manager;
      Etherealize_Objects :        Boolean;
      Wait_For_Completion :        Boolean)
   is
   begin
      pragma Debug (O ("Hold requests, Wait_For_Completion is "
                       & Wait_For_Completion'Img
                       & ", Etherealize_Objects is "
                       & Etherealize_Objects'Img));
      Lock_W (Self.State_Lock);
      if Self.Current_State = INACTIVE then
         Unlock_W (Self.State_Lock);
         Raise_Adapter_Inactive;
      else
         Self.Current_State := INACTIVE;
      end if;
      Unlock_W (Self.State_Lock);

      if Etherealize_Objects then
         Do_Etherealize_Objects (Self);
      end if;
      if Wait_For_Completion then
         Do_Wait_For_Completion (Self);
      end if;
   end Deactivate;

   ---------------
   -- Get_State --
   ---------------

   function Get_State (Self : Basic_POA_Manager)
                      return State
   is
   begin
      pragma Debug (O ("POAManager state is "
                       & Self.Current_State'Img));
      return Self.Current_State;
   end Get_State;

   ------------
   -- Create --
   ------------

   procedure Create (M : access Basic_POA_Manager)
   is
      use Droopi.POA_Types.POA_Sequences;
   begin
      pragma Debug (O ("Create a new Basic_POA_Manager"));
      Create (M.State_Lock);
      Create (M.Count_Lock);
      Create (M.POAs_Lock);
      Create (M.Queue_Lock);

      Lock_W (M.POAs_Lock);
      M.Managed_POAs := new POAList;
      Unlock_W (M.POAs_Lock);

      Lock_W (M.State_Lock);
      M.Current_State := HOLDING;
      Unlock_W (M.State_Lock);

      Lock_W (M.Queue_Lock);
      Create (M.Holded_Requests, Queue_Size);
      Unlock_W (M.Queue_Lock);
   end Create;

   ------------------
   -- Register_POA --
   ------------------

   procedure Register_POA
     (Self : access Basic_POA_Manager;
      OA   : Obj_Adapter_Access)
   is
      use Droopi.POA_Types.POA_Sequences;
   begin
      pragma Debug (O ("Register a new POA"));
      Lock_W (Self.POAs_Lock);
      for I in 1 .. Length (Sequence (Self.Managed_POAs.all)) loop
         if Element_Of (Sequence (Self.Managed_POAs.all), I) = null then
            Replace_Element (Sequence (Self.Managed_POAs.all), I, OA);
            Unlock_W (Self.POAs_Lock);
            Inc_Usage_Counter (Self);
            return;
         end if;
      end loop;
      Append (Sequence (Self.Managed_POAs.all), OA);
      Unlock_W (Self.POAs_Lock);
      Inc_Usage_Counter (Self);
   end Register_POA;

   ----------------
   -- Remove_POA --
   ----------------

   procedure Remove_POA
     (Self : access Basic_POA_Manager;
      OA   : Obj_Adapter_Access)
   is
      use Droopi.POA_Types.POA_Sequences;
      A_Child : Obj_Adapter_Access;
   begin
      pragma Debug (O ("Remove a POA"));
      Lock_W (Self.POAs_Lock);
      for I in 1 .. Length (Sequence (Self.Managed_POAs.all)) loop
         A_Child := Element_Of (Sequence (Self.Managed_POAs.all), I);
         if A_Child = OA then
            Replace_Element (Sequence (Self.Managed_POAs.all), I, null);
            Unlock_W (Self.POAs_Lock);
            Dec_Usage_Counter (Self);
            Destroy_If_Unused (Self);
            return;
         end if;
      end loop;
      Unlock_W (Self.POAs_Lock);

      raise Invalid_Obj_Adapter;
   end Remove_POA;

   ----------------------
   -- Get_Hold_Servant --
   ----------------------

   function Get_Hold_Servant
     (Self : access Basic_POA_Manager;
      OA   :        Obj_Adapter_Access)
     return Hold_Servant_Base_Access
   is
      S         : Hold_Servant_Access;
      New_Entry : Queue_Element_Access;
   begin
      pragma Debug (O ("Get a Hold_Servant"));
      Lock_W (Self.Queue_Lock);

      if Get_Count (Self.Holded_Requests) >=
        Get_Max_Count (Self.Holded_Requests)
      then
         Unlock_W (Self.Queue_Lock);
         Raise_Transient (1);
      end if;

      New_Entry     := new Queue_Element;
      New_Entry.OA  := OA;
      Add (Self.Holded_Requests, New_Entry);

      S             := new Hold_Servant;
      S.Queue_Entry := New_Entry;
      Unlock_W (Self.Queue_Lock);

      return Hold_Servant_Base_Access (S);
   end Get_Hold_Servant;

   -----------------------
   -- Inc_Usage_Counter --
   -----------------------

   procedure Inc_Usage_Counter
     (Self : access Basic_POA_Manager)
   is
   begin
      Lock_W (Self.Count_Lock);
      Self.Usage_Count := Self.Usage_Count + 1;
      Unlock_W (Self.Count_Lock);
      pragma Debug (O ("Increase usage to "
                       & Self.Usage_Count'Img));
   end Inc_Usage_Counter;

   -----------------------
   -- Dec_Usage_Counter --
   -----------------------

   procedure Dec_Usage_Counter
     (Self : access Basic_POA_Manager)
   is
   begin
      Lock_W (Self.Count_Lock);
      Self.Usage_Count := Self.Usage_Count - 1;
      Unlock_W (Self.Count_Lock);
      pragma Debug (O ("Decrease usage to "
                       & Self.Usage_Count'Img));
   end Dec_Usage_Counter;

   ----------------------------
   -- Do_Wait_For_Completion --
   ----------------------------

   procedure Do_Wait_For_Completion
     (Self : access Basic_POA_Manager)
   is
   begin
      --  ??? What's this thing about the threads ? Ignored for now.
      --  ??? Iterates through the POAs to wait for completion
      null;
   end Do_Wait_For_Completion;

   ----------------------------
   -- Do_Etherealize_Objects --
   ----------------------------

   procedure Do_Etherealize_Objects
     (Self : access Basic_POA_Manager)
   is
   begin
      null;
      --  ??? To be implemented
   end Do_Etherealize_Objects;

   -----------------------
   -- Destroy_If_Unused --
   -----------------------

   procedure Destroy_If_Unused
     (Self : access Basic_POA_Manager)
   is
      BPM : Basic_POA_Manager_Access
        := Basic_POA_Manager_Access (Self);
   begin
      Lock_R (Self.Count_Lock);
      if Self.Usage_Count = 0 then
         pragma Debug (O ("POAManager is no longer used, destroying it"));
         Unlock_R (Self.Count_Lock);
         Destroy (Self.State_Lock);
         Destroy (Self.Count_Lock);
         Destroy (Self.POAs_Lock);
         Destroy (Self.Queue_Lock);
         Destroy (Self.Holded_Requests);
         Free (BPM);
         return;
      end if;
      Unlock_R (Self.Count_Lock);
   end Destroy_If_Unused;

   ------------
   -- Create --
   ------------

   procedure Create
     (HS  : in out Hold_Servant;
      QEA : in     Queue_Element_Access)
   is
   begin
      HS.Queue_Entry := QEA;
   end Create;

   ----------
   -- Left --
   ----------

   function "="
     (Left, Right : Hold_Servant)
     return Boolean
   is
   begin
      return Left.Queue_Entry = Right.Queue_Entry;
   end "=";

   --------------------
   -- Handle_Message --
   --------------------

   function Handle_Message
     (Obj : access Hold_Servant;
      Msg :        Droopi.Components.Message'Class)
     return Droopi.Components.Message'Class
   is
      S            : Hold_Servant_Access;
      Null_Message : Droopi.Components.Null_Message;
   begin
      pragma Debug (O ("Hold Servant queues message"));
      --      Obj.Queue_Entry.Msg := Msg;
      --  ??? How do we queue the messages?

      S := Hold_Servant_Access (Obj);
      Free (S);
      return Null_Message;
   end Handle_Message;

end Droopi.POA_Manager.Basic_Manager;
