with Sequences.Unbounded;
with Sequences.Unbounded.Search;
with Droopi.Log;
pragma Elaborate_All (Droopi.Log);

with CORBA.Policy_Types;

package body CORBA.POA.Basic_POA is

   use POA_Types;
   use Droopi.Log;
   use CORBA.POA_Manager;
   use CORBA.Policy;
   use CORBA.Policy_Types;

   package L is new Droopi.Log.Facility_Log ("CORBA.POA.Root_POA");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   function Get_Child (Adapter :    Obj_Adapter_Access;
                       Name    : in String)
                      return POA_Types.Obj_Adapter_Access;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child (Adapter :    Obj_Adapter_Access;
                       Name    : in String)
                      return POA_Types.Obj_Adapter_Access
   is
      function Match (Item   : POA_Types.Obj_Adapter_Access;
                      Needle : String) return Boolean;
      function Match (Item   : POA_Types.Obj_Adapter_Access;
                      Needle : String)
                     return Boolean
      is
      begin
         return (Name = Needle);
      end Match;

      package Search_POAList is new POA_Sequences.Search (String, Match);
      use POA_Sequences;

      Result : POAList;
   begin
      Result := Search_POAList.Sub_Sequence (Adapter.Children.all, Name);
      if Result = POA_Sequences.Null_Sequence then
         --  Clean Result
         return POA_Sequences.Element_Of (Result, 1);
      else
         return null;
      end if;
   end Get_Child;

   ----------------
   -- Create_POA --
   ----------------

   function Create_POA
     (Self         : Obj_Adapter_Access;
      Adapter_Name : String;
      A_POAManager : POA_Manager.POAManager_Access;
      Policies     : Policy.PolicyList_Access)
     return Obj_Adapter_Access
   is
      New_Obj_Adapter : Obj_Adapter_Access;
   begin
      O ("Enter Basic_POA.Create_POA");
      --  ??? Add check code here

      --  Look if there is already a child with this name
      if Self /= null
        and then Self.Children /= null
        and then  Get_Child (Self, Adapter_Name) /= null
      then
         O ("POA already exists!");
         --  ??? Raise AdapterAlreadyExists exception
      end if;

      --  Create new object adapter
      New_Obj_Adapter        := new Obj_Adapter;
      New_Obj_Adapter.Name   := Adapter_Name;
      New_Obj_Adapter.Father := POA_Types.Obj_Adapter_Access (Self);
      if A_POAManager = null then
         --  New_Obj_Adapter.POA_Manager := new POA_Manager;
         --  ??? Use factory instead
         null;
      else
         New_Obj_Adapter.POA_Manager := A_POAManager;
      end if;

      --  Init policies with those given by the user
      if Policies /= null then
         declare
            A_Policy : Policy_Access;
         begin
            for I in 1 .. Policy_Sequences.Length (Policies.all) loop
               A_Policy := Policy_Sequences.Element_Of (Policies.all, I);
               case A_Policy.Policy_Type is
                  when THREAD_POLICY_ID =>
                     if New_Obj_Adapter.Thread_Policy /= null then
                        O ("Duplicate in ThreadPolicy: using last one");
                        --  ??? What is the desired behavior in this case?
                     end if;
                     New_Obj_Adapter.Thread_Policy
                       := new ThreadPolicy'(ThreadPolicy (A_Policy.all));
                     --  ??? etc. Check code first
                  when others =>
                     null;
               end case;
            end loop;
         end;
      end if;

      --  Use default policy if not provided by the user
      if New_Obj_Adapter.Thread_Policy = null then
         --  New_Obj_Adapter.Thread_Policy := new ThreadPolicy'(THREAD_POLICY_ID);
         --  ??? Should use policies factory
         null;
      end if;

      --  ??? Check compatibilities between policies

      --  ??? If error, clean memory

      --  ??? Register new obj_adapter as a sibling of the current POA

      if Self /= null then
         Self.Name := To_CORBA_String ("Bla");
      end if;

      return New_Obj_Adapter;
   end Create_POA;

   ------------
   -- Create --
   ------------

   procedure Create (OA : out Obj_Adapter)
   is
   begin
      null;
   end Create;
   --  Initialize.

   -------------
   -- Destroy --
   -------------

   procedure Destroy (OA : in out Obj_Adapter)
   is
   begin
      null;
   end Destroy;
   --  Finalize.

   ------------
   -- Export --
   ------------

   function Export
     (OA  : access Obj_Adapter;
      Obj :        Droopi.Objects.Servant_Access)
     return Droopi.Objects.Object_Id
   is
   begin
      return Export (OA, Obj);
   end Export;

   --------------
   -- Unexport --
   --------------

   procedure Unexport
     (OA : access Obj_Adapter;
      Id :        Droopi.Objects.Object_Id)
   is
   begin
      null;
   end Unexport;
   --  Id is an object identifier attributed by OA.
   --  The corresponding association is suppressed.

   ------------------------
   -- Get_Empty_Arg_List --
   ------------------------

   function Get_Empty_Arg_List
     (OA     : Obj_Adapter;
      Oid    : Droopi.Objects.Object_Id;
      Method : Droopi.Requests.Operation_Id)
     return CORBA.NVList.Ref
   is
   begin
      return Get_Empty_Arg_List (OA, Oid, Method);
   end Get_Empty_Arg_List;

   ----------------------
   -- Get_Empty_Result --
   ----------------------

   function Get_Empty_Result
     (OA     : Obj_Adapter;
      Oid    : Droopi.Objects.Object_Id;
      Method : Droopi.Requests.Operation_Id)
     return CORBA.Any
   is
   begin
      return Get_Empty_Result (OA, Oid, Method);
   end Get_Empty_Result;

   ------------------
   -- Find_Servant --
   ------------------

   function Find_Servant
     (OA : access Obj_Adapter;
      Id :        Droopi.Objects.Object_Id)
     return Droopi.Objects.Servant_Access
   is
   begin
      return Find_Servant (OA, Id);
   end Find_Servant;

   ---------------------
   -- Release_Servant --
   ---------------------

   procedure Release_Servant
     (OA      : access Obj_Adapter;
      Id      :        Droopi.Objects.Object_Id;
      Servant : in out Droopi.Objects.Servant_Access)
   is
   begin
      null;
   end Release_Servant;

end CORBA.POA.Basic_POA;
