------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           S Y S T E M . P A R T I T I O N _ I N T E R F A C E            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  This version is for PolyORB.

--  $Id$

package body System.Partition_Interface is

--    pragma Warnings (Off); -- supress warnings for unreferenced formals

--    M : constant := 7;

--    type String_Access is access String;

--    --  To have a minimal implementation of U'Partition_ID.

--    type Pkg_Node;
--    type Pkg_List is access Pkg_Node;
--    type Pkg_Node is record
--       Name : String_Access;
--       Next : Pkg_List;
--    end record;

--    Pkg_Head : Pkg_List;
--    Pkg_Tail : Pkg_List;

--    function getpid return Integer;
--    pragma Import (C, getpid);

--    PID : constant Integer := getpid;

--    function Lower (S : String) return String;

--    Passive_Prefix : constant String := "SP__";
--    --  String prepended in top of shared passive packages

--    procedure Check
--      (Name    : in Unit_Name;
--       Version : in String;
--       RCI     : in Boolean := True)
--    is
--    begin
--       null;
--    end Check;

   -----------------------------
   -- Get_Active_Partition_Id --
   -----------------------------

   function Get_Active_Partition_ID
     (Name : Unit_Name)
      return RPC.Partition_ID
     renames System.PolyORB_Interface.Get_Active_Partition_ID;

--    ------------------------
--    -- Get_Active_Version --
--    ------------------------

--    function Get_Active_Version
--      (Name : Unit_Name)
--       return String
--    is
--    begin
--       return "";
--    end Get_Active_Version;

   ----------------------------
   -- Get_Local_Partition_Id --
   ----------------------------

   function Get_Local_Partition_ID return RPC.Partition_ID
     renames System.PolyORB_Interface.Get_Local_Partition_ID;

--    ------------------------------
--    -- Get_Passive_Partition_ID --
--    ------------------------------

--    function Get_Passive_Partition_ID
--      (Name : Unit_Name)
--       return System.RPC.Partition_ID
--    is
--    begin
--       return Get_Local_Partition_ID;
--    end Get_Passive_Partition_ID;

--    -------------------------
--    -- Get_Passive_Version --
--    -------------------------

--    function Get_Passive_Version
--      (Name : Unit_Name)
--       return String
--    is
--    begin
--       return "";
--    end Get_Passive_Version;

--    ------------------------------
--    -- Get_RCI_Package_Receiver --
--    ------------------------------

--    function Get_RCI_Package_Receiver
--      (Name : Unit_Name)
--       return Interfaces.Unsigned_64
--    is
--    begin
--       return 0;
--    end Get_RCI_Package_Receiver;

   -------------------------------
   -- Get_Unique_Remote_Pointer --
   -------------------------------

   procedure Get_Unique_Remote_Pointer
     (Handler : in out RACW_Stub_Type_Access)
     renames System.PolyORB_Interface.Get_Unique_Remote_Pointer;

--    -----------
--    -- Lower --
--    -----------

--    function Lower (S : String) return String is
--       T : String := S;

--    begin
--       for J in T'Range loop
--          if T (J) in 'A' .. 'Z' then
--             T (J) := Character'Val (Character'Pos (T (J)) -
--                                     Character'Pos ('A') +
--                                     Character'Pos ('a'));
--          end if;
--       end loop;

--       return T;
--    end Lower;

   -------------------------------------
   -- Raise_Program_Error_Unknown_Tag --
   -------------------------------------

   procedure Raise_Program_Error_Unknown_Tag
     (E : in Ada.Exceptions.Exception_Occurrence)
   is
   begin
      Ada.Exceptions.Raise_Exception
        (Program_Error'Identity, Ada.Exceptions.Exception_Message (E));
   end Raise_Program_Error_Unknown_Tag;

   --------------------
   -- Same_Partition --
   --------------------

   function Same_Partition
      (Left  : access RACW_Stub_Type;
       Right : access RACW_Stub_Type) return Boolean is
      pragma Unreferenced (Left, Right);
   begin
      return False;
      --  XXX not implemented yet!
   end if;

end System.Partition_Interface;
