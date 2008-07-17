------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . D S A _ P . S T O R A G E S . D S M          --
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

--  This package body contains the full declaration of the concret type
--  DSM_Manager. This is convenient because it allows to use non remote
--  types in DSM_Manager record (Mutexes, Any), so only RACW primitives
--  parameters types need to be remote types compatible.

--  Note :
--  Invalidation phase as described in the algorithm is incomplete and
--  should cause deadlocks in specific cases. If an invalidation request
--  that refers to the acquirement of Write access by another partition
--  is received, it do not make sense since local partition has obtained
--  a later Write access on the variable.
--  So we track version numbers of variables, and ingnore invalidation
--  requests that refer to previous version numbers of the variable than
--  the local partition one.

pragma Ada_2005;

with Ada.Unchecked_Conversion;

with System.Partition_Interface;

with PolyORB.Any;
with PolyORB.Log;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Condition_Variables;

package body PolyORB.DSA_P.Storages.DSM is

   use System.Partition_Interface;
   use PolyORB.Any;
   use PolyORB.Log;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Tasking.Condition_Variables;

   --  Access rights to a shared variable can be Write, Read or None

   type Status_Type is (Write, Read, None);

   --  Mutexes and barrier used for objects synchronization

   type Synchonization_Tools is record
      Critical_Section : Mutex_Access;
      Protected_Object : Mutex_Access;
      Wait_Mutex       : Mutex_Access;
      Wait_Barrier     : Condition_Access;
   end record;

   function Extract_Pkg_Name (Var_Name : String) return String;
   --  Var_Name is a fully qualified variable string name. Remove suffix
   --  to get package string name.

   -----------------
   -- DSM_Manager --
   -----------------

   --  The variable state is composed of the following attribute :
   --  * Data       : System.DSA_Types.Any_Container_Ptr data representation
   --  * Status     : Variable mode (Write, Read or None)
   --  * Prob_Owner : Probable owner (Li & Hudak algorithm)
   --  * Copies     : Copy set (Li & Hudak algorithm)
   --  * Synchs     : Synchronisation tools
   --  * Locked     : Lock used for protected types
   --  * Version    : Increased each times we handle a remote request

   type DSM_Manager is new DSM_Manager_Type with record
      Data       : PolyORB.Any.Any;
      Status     : Status_Type;
      Prob_Owner : DSM_Manager_RACW;
      Copies     : Copy_Set_Type;
      Synchs     : Synchonization_Tools;
      Locked     : Boolean;
      Version    : Integer;
   end record;
   type DSM_Manager_Access is access all DSM_Manager'Class;

   --  DSM_Manager type primitives

   --  Remotely called primitives

   overriding
   procedure Invalidate_Request
     (Self      : access DSM_Manager;
      Rqst_Node : DSM_Manager_RACW;
      Version   : Integer);

   overriding
   procedure Write_Request
     (Self        : access DSM_Manager;
      Rqst_Node   : DSM_Manager_RACW);

   overriding
   procedure Write_Reply
     (Self        : access DSM_Manager;
      Var_Data    : SDT.Any_Container_Ptr;
      Read_Copies : Copy_Set_Type;
      Version     : Integer);

   overriding
   procedure Read_Request
     (Self       : access DSM_Manager;
      Rqst_Node  : DSM_Manager_RACW);

   overriding
   procedure Read_Reply
     (Self        : access DSM_Manager;
      Var_Data    : SDT.Any_Container_Ptr;
      Reply_Node  : DSM_Manager_RACW;
      Version     : Integer);

   overriding
   function Get_Initial_Owner
     (Self      : access DSM_Manager;
      Var_Name  : String)
      return DSM_Manager_RACW;

   --  Locally called primitives

   overriding
   procedure Read
     (Self : access DSM_Manager;
      Var  : in out SDT.Any_Container_Ptr);

   overriding
   procedure Write
     (Self : access DSM_Manager;
      Var  : in out SDT.Any_Container_Ptr);

   overriding
   procedure Lock   (Self : access DSM_Manager);

   overriding
   procedure Unlock (Self : access DSM_Manager);

   overriding
   function Create
     (Manager_Factory : access DSM_Manager;
      Full_Name       : String)
      return Shared_Data_Manager_RACW;

   --------------------------
   -- Unchecked_Conversion --
   --------------------------

   function DAC_To_AC is
     new Ada.Unchecked_Conversion
       (SDT.Any_Container_Ptr, Any_Container_Ptr);
   --  Convert Any_Container_Ptr DSA type to PolyORB one

   function AC_To_DAC is
     new Ada.Unchecked_Conversion
       (Any_Container_Ptr, SDT.Any_Container_Ptr);
   --  Convert Any_Container_Ptr PolyORB type to DSA one

   function DSM_Manager_To_Address is
     new Ada.Unchecked_Conversion
       (DSM_Manager_Access, System.Address);
   --  Convert DSM_Manager_Access to system address

   function Address_To_DSM_Manager is
     new Ada.Unchecked_Conversion
       (System.Address, DSM_Manager_RACW);
   --  Convert system address to DSM_Manager_Access

   -------------
   -- Logging --
   -------------

   package L is new Log.Facility_Log ("polyorb.dsa_p.storages.dsm");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
                renames L.Output;
   function C  (Level : Log_Level := Debug) return Boolean
                renames L.Enabled;

   ----------------------
   -- Extract_Pkg_Name --
   ----------------------

   function Extract_Pkg_Name (Var_Name : String) return String is
   begin
      for Index in reverse Var_Name'Range loop
         if Var_Name (Index) = '.' then
            return Var_Name (Var_Name'First .. Index - 1);
         end if;
      end loop;
      return "";
   end Extract_Pkg_Name;

   ------------
   -- Create --
   ------------

   function Create
     (Manager_Factory : access DSM_Manager;
      Full_Name       : String)
      return Shared_Data_Manager_RACW
   is
      use Copy_Set_Tables;
      Manager    : DSM_Manager_Access;
      Owner_Addr : System.Address;

   begin
      pragma Debug (C, O ("create DSM manager"));

      Manager        := new DSM_Manager;
      Manager.Locked := False;

      --  Initializing dynamic table

      Initialize (Manager.Copies);

      --  Create synchonisation objects

      Create (Manager.Synchs.Critical_Section);
      Create (Manager.Synchs.Protected_Object);
      Create (Manager.Synchs.Wait_Mutex);
      Create (Manager.Synchs.Wait_Barrier);

      --  Initialize variable owner

      if Manager_Factory.Prob_Owner = DSM_Manager_RACW (Manager_Factory) then
         pragma Debug (C, O ("I am the initial owner of variable "
           & Full_Name));

         Manager.Status     := Write;
         Manager.Prob_Owner := DSM_Manager_RACW (Manager);
      else
         pragma Debug (C, O ("Retrieve initial owner of variable "
                             & Full_Name & " in name serveur"));

         --  Retrieve initial owner of the variable

         System.Partition_Interface.Retrieve_RACW_From_Name_Server
           (Name    => Extract_Pkg_Name (Full_Name),
            Kind    => "SP",
            Stub_Tag => DSM_Manager_RACW'Stub_Type'Tag,
            Addr    => Owner_Addr);

         Manager.Status     := None;
         Manager.Prob_Owner := Get_Initial_Owner
           (Address_To_DSM_Manager (Owner_Addr), Full_Name);
      end if;

      return Shared_Data_Manager_RACW (Manager);
   end Create;

   -----------------------
   -- Get_Initial_Owner --
   -----------------------

   function Get_Initial_Owner
     (Self      : access DSM_Manager;
      Var_Name  : String)
      return DSM_Manager_RACW
   is
      pragma Unreferenced (Self);
      Owner : Shared_Data_Manager_RACW;

   begin
      pragma Debug (C, O ("Initial owner request for " & Var_Name));
      Lookup_Variable (Var_Name, Owner);
      return DSM_Manager_RACW (Owner);
   end Get_Initial_Owner;

   ------------------------
   -- Invalidate_Request --
   ------------------------

   procedure Invalidate_Request
     (Self      : access DSM_Manager;
      Rqst_Node : DSM_Manager_RACW;
      Version   : Integer)
   is
      use Copy_Set_Tables;

   begin
      --  Asynchronous procedure

      --  Invalidate only if local partition have read access
      --  on the shared variable and if variable version number
      --  isn't obslete.

      if Self.Status = Read and Version >= Self.Version then
         pragma Debug (C, O ("Invalidation request received"));

         --  Send invalidation request to nodes in the copy set

         for C in First (Self.Copies) .. Last (Self.Copies) loop
            declare
               Target : constant DSM_Manager_RACW := Self.Copies.Table (C);
            begin
               if Target /= DSM_Manager_RACW (Self)
                 and then Target /= Rqst_Node then
                  Invalidate_Request
                    (Self      => Target,
                     Rqst_Node => DSM_Manager_RACW (Self),
                     Version   => Self.Version);
               end if;
            end;
         end loop;

         Self.Prob_Owner := Rqst_Node;
         Self.Status     := None;
         Initialize (Self.Copies);
         --  reestablishing table for new use

         return;
      end if;

      pragma Debug (C, O ("Invalidation request ignored"));
   end Invalidate_Request;

   ----------
   -- Lock --
   ----------

   procedure Lock (Self : access DSM_Manager) is
   begin
      Enter (Self.Synchs.Protected_Object);
      Enter (Self.Synchs.Critical_Section);

      --  If local partition isn't the owner, sending write
      --  request to become the owner and obtain write access.
      --  So the next read and write calls within the protected
      --  object critical section won't generate remote requests.

      if Self.Prob_Owner /= DSM_Manager_RACW (Self) then
         pragma Debug (C, O ("Sending write request to probable owner"));

         --  Ask for write access to probable owner.

         Enter (Self.Synchs.Wait_Mutex);
         Write_Request
           (Self      => Self.Prob_Owner,
            Rqst_Node => DSM_Manager_RACW (Self));

         --  Awaiting owner reply

         Wait  (Self.Synchs.Wait_Barrier, Self.Synchs.Wait_Mutex);
         Leave (Self.Synchs.Wait_Mutex);
      end if;

      Self.Prob_Owner := DSM_Manager_RACW (Self);
      Self.Status     := Write;
      Self.Locked     := True;
      --  Indicate that we are curently in protected object
      --  critical section.

      Leave (Self.Synchs.Critical_Section);
   end Lock;

   ----------
   -- Read --
   ----------

   procedure Read
     (Self : access DSM_Manager;
      Var  : in out SDT.Any_Container_Ptr) is
   begin
      Enter (Self.Synchs.Critical_Section);

      if Self.Status = None then
         pragma Debug (C, O ("Sending read request to probable owner"));
         Enter (Self.Synchs.Wait_Mutex);

         --  Ask for read access to probable owner.

         Read_Request
           (Self      => Self.Prob_Owner,
            Rqst_Node => DSM_Manager_RACW (Self));

         --  Awaiting reply from first node which have read
         --  access on the varaible.

         Wait  (Self.Synchs.Wait_Barrier, Self.Synchs.Wait_Mutex);
         Leave (Self.Synchs.Wait_Mutex);

         Self.Status := Read;
      end if;

      Var := AC_To_DAC (Get_Container (Self.Data));
      --  Assign out data value to container value

      Leave (Self.Synchs.Critical_Section);
   end Read;

   ----------------
   -- Read_Reply --
   ----------------

   procedure Read_Reply
     (Self        : access DSM_Manager;
      Var_Data    : SDT.Any_Container_Ptr;
      Reply_Node  : DSM_Manager_RACW;
      Version     : Integer) is
   begin
      --  Asynchronous procedure

      pragma Debug (C, O ("Receiving read reply from probable owner"));
      Enter (Self.Synchs.Wait_Mutex);

      Set_Container (Self.Data, DAC_To_AC (Var_Data));
      Self.Prob_Owner := Reply_Node;
      Self.Version    := Version;

      Broadcast (Self.Synchs.Wait_Barrier);
      --  Unlock thread waiting reply

      Leave (Self.Synchs.Wait_Mutex);
   end Read_Reply;

   ------------------
   -- Read_Request --
   ------------------

   procedure Read_Request
     (Self       : access DSM_Manager;
      Rqst_Node  : DSM_Manager_RACW)
   is
      use Copy_Set_Tables;

   begin
      --  Asynchronous procedure

      Enter (Self.Synchs.Critical_Section);
      pragma Debug (C, O ("Read request received"));

      --  Handling request if local partition has read or
      --  write access, else forwarding it.

      if Self.Status /= None then

         Self.Status := Read;
         --  Lost any write access on the variable

         Increment_Last (Self.Copies);
         Self.Copies.Table (Last (Self.Copies)) := Rqst_Node;
         --  Add request node to the copy set

         --  Replying to request node

         Self.Version := Self.Version + 1;
         Read_Reply
           (Self       => Rqst_Node,
            Var_Data   => AC_To_DAC (Get_Container (Self.Data)),
            Reply_Node => DSM_Manager_RACW (Self),
            Version    => Self.Version);
      else
         pragma Debug (C, O ("Forwarding read request to probable owner"));

         --  Forwarding request to probable owner

         Read_Request
           (Self      => Self.Prob_Owner,
            Rqst_Node => Rqst_Node);

         Self.Prob_Owner := Rqst_Node;
      end if;

      Leave (Self.Synchs.Critical_Section);
   end Read_Request;

   ------------------------------
   -- Register_passive_Package --
   ------------------------------

   procedure Register_Passive_Package
     (Pkg_Name : String;
      Is_Owner : Boolean)
   is
      Factory : constant DSM_Manager_Access := new DSM_Manager;

   begin
      pragma Debug (C, O ("Register DSM factory for package "
        & Pkg_Name));

      --  If local partition is the initial owner of the shared
      --  passive package, register it in the name server.

      if Is_Owner then
         Factory.Prob_Owner := DSM_Manager_RACW (Factory);
         System.Partition_Interface.Register_RACW_In_Name_Server
           (Addr     => DSM_Manager_To_Address (Factory),
            Type_Tag => DSM_Manager_Type'Tag,
            Name     => Pkg_Name,
            Kind     => "SP");
      end if;

      Register_Factory (Pkg_Name, Shared_Data_Manager_RACW (Factory));
   end Register_Passive_Package;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (Self : access DSM_Manager) is
   begin
      --  Terminate the protected object critical section

      Self.Locked := False;
      Leave (Self.Synchs.Protected_Object);
   end Unlock;

   -----------
   -- Write --
   -----------

   procedure Write
     (Self : access DSM_Manager;
      Var  : in out SDT.Any_Container_Ptr)
   is
      use Copy_Set_Tables;

   begin
      Enter (Self.Synchs.Critical_Section);

      if Self.Status /= Write
        and then Self.Prob_Owner /= DSM_Manager_RACW (Self)
      then
         pragma Debug (C, O ("Sending write request to probable owner"));

         Enter (Self.Synchs.Wait_Mutex);

         --  Ask for write access to probable owner

         Write_Request
           (Self      => Self.Prob_Owner,
            Rqst_Node => DSM_Manager_RACW (Self));

         --  Awaiting reply from real owner of the variable

         Wait  (Self.Synchs.Wait_Barrier, Self.Synchs.Wait_Mutex);
         Leave (Self.Synchs.Wait_Mutex);
      end if;

      --  Send invalidation request to nodes in the copy set

      for C in First (Self.Copies) .. Last (Self.Copies) loop
         declare
            Target : constant DSM_Manager_RACW := Self.Copies.Table (C);
         begin
            if Target /= DSM_Manager_RACW (Self) then
               Invalidate_Request
                 (Self      => Target,
                  Rqst_Node => DSM_Manager_RACW (Self),
                  Version   => Self.Version);
            end if;
         end;
      end loop;

      Self.Prob_Owner := DSM_Manager_RACW (Self);
      Self.Status     := Write;
      Initialize (Self.Copies);
      --  reestablishing table for new use

      Set_Container (Self.Data, DAC_To_AC (Var));
      --  Assign out data value to container value

      Leave (Self.Synchs.Critical_Section);
   end Write;

   -------------------
   -- Write_Reply --
   -------------------

   procedure Write_Reply
     (Self        : access DSM_Manager;
      Var_Data    : SDT.Any_Container_Ptr;
      Read_Copies : Copy_Set_Type;
      Version     : Integer) is
   begin
      --  Asynchronous procedure

      pragma Debug (C, O ("Receiving write reply from real owner"));
      Enter (Self.Synchs.Wait_Mutex);

      Self.Copies  := Read_Copies;
      Self.Version := Version;
      Set_Container (Self.Data, DAC_To_AC (Var_Data));

      Broadcast (Self.Synchs.Wait_Barrier);
      --  Unlock thread awaiting reply

      Leave (Self.Synchs.Wait_Mutex);
   end Write_Reply;

   -------------------
   -- Write_Request --
   -------------------

   procedure Write_Request
     (Self      : access DSM_Manager;
      Rqst_Node : DSM_Manager_RACW) is
   begin
      --  Asynchronous procedure

      Enter (Self.Synchs.Critical_Section);

      --  If variable is a protected object and is curently in
      --  use, waiting end of protected object critical section.

      if Self.Locked then
         Enter (Self.Synchs.Protected_Object);
      end if;

      pragma Debug (C, O ("Write request received"));

      --  Handling request if local partition is the owner, else
      --  forwarding it. In both case Rqst_Node node will obtain
      --  write access on the variable, so we lost any access on
      --  variable,

      Self.Status := None;
      if Self.Prob_Owner = DSM_Manager_RACW (Self) then

         --  Replying to request node

         Self.Version := Self.Version + 1;
         Write_Reply
           (Self        => Rqst_Node,
            Var_Data    => AC_To_DAC (Get_Container (Self.Data)),
            Read_Copies => Self.Copies,
            Version     => Self.Version);

      else
         pragma Debug (C, O ("Forwarding write request to probable owner"));

         --  Forwarding request to probable owner

         Write_Request
           (Self      => Self.Prob_Owner,
            Rqst_Node => Rqst_Node);
      end if;

      Self.Prob_Owner := Rqst_Node;

      Leave (Self.Synchs.Protected_Object);
      Leave (Self.Synchs.Critical_Section);
   end Write_Request;

end PolyORB.DSA_P.Storages.DSM;
