------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        M O M A . M E S S A G E S                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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

--  $Id$

package body MOMA.Messages is

   ----------------------------
   --  Acknowledge Procedure --
   ----------------------------
   procedure Acknowledge is
   begin
      null;
   end Acknowledge;

   ---------------------------
   --  Clear_Body Procedure --
   ---------------------------
   procedure Clear_Body is
   begin
      null;
   end Clear_Body;

   ----------------------------
   --  Get_Property Function --
   ----------------------------
   function Get_Property (Name : String) return Property_Type is
   begin
      pragma Warnings (Off);
      return Get_Property (Name);
      pragma Unreferenced (Name);
      pragma Warnings (On);
   end Get_Property;

   ----------------------------------
   --  Get_Correlation_Id Function --
   ----------------------------------
   function Get_Correlation_Id return String is
   begin
      return "null";
   end Get_Correlation_Id;

   ------------------------------
   --  Get_Persistent Function --
   ------------------------------
   function Get_Persistent return Boolean is
   begin
      return False;
   end Get_Persistent;

   ------------------------------
   --  Get_Expiration Function --
   ------------------------------
   function Get_Expiration return Time is
   begin
      pragma Warnings (Off);
      return Get_Expiration;
      pragma Warnings (On);
   end Get_Expiration;

   ------------------------------
   --  Get_Message_Id Function --
   ------------------------------
   function Get_Message_Id return String is
   begin
      return "null";
   end Get_Message_Id;

   ----------------------------
   --  Get_Priority Function --
   ----------------------------
   function Get_Priority return Priority is
   begin
      pragma Warnings (Off);
      return Get_Priority;
      pragma Warnings (On);
   end Get_Priority;

   -------------------------------
   --  Get_Redelivered Function --
   -------------------------------
   function Get_Redelivered return Boolean is
   begin
      return False;
   end Get_Redelivered;

   -----------------------------
   --  Get_Timestamp Function --
   -----------------------------
   function Get_Timestamp return Time is
   begin
      pragma Warnings (Off);
      return Get_Timestamp;
      pragma Warnings (On);
   end Get_Timestamp;

   ------------------------
   --  Get_Type Function --
   ------------------------
   function Get_Type return String is
   begin
      return "null";
   end Get_Type;

   ----------------------------------
   --  Get_Property_Names Function --
   ----------------------------------
   --  ???
   function Get_Property_Names return Integer is
   begin
      return 0;
   end Get_Property_Names;

   -------------------------------
   --  Property_Exists Function --
   -------------------------------
   function Property_Exists (Name : String) return Boolean is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Name);
      pragma Warnings (On);
      return False;
   end Property_Exists;

   -----------------------------
   --  Set_Property Procedure --
   -----------------------------
   procedure Set_Property (Name : String; Value : Property_Type) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Name);
      pragma Unreferenced (Value);
      pragma Warnings (On);
      null;
   end Set_Property;

   -----------------------------------
   --  Set_Correlation_Id Procedure --
   -----------------------------------
   procedure Set_Correlation_Id (Correlation_Id : String) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Correlation_Id);
      pragma Warnings (On);
      null;
   end Set_Correlation_Id;

   -------------------------------
   --  Set_Persistent Procedure --
   -------------------------------
   procedure Set_Persistent (Is_Persistent : Boolean) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Is_Persistent);
      pragma Warnings (On);
      null;
   end Set_Persistent;

   --------------------------------
   --  Set_Destination Procedure --
   --------------------------------
   procedure Set_Destination (Destination : MOMA.Destinations.Destination) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Destination);
      pragma Warnings (On);
      null;
   end Set_Destination;

   -------------------------------
   --  Set_Expiration Procedure --
   -------------------------------
   procedure Set_Expiration (Expiration : Time) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Expiration);
      pragma Warnings (On);
      null;
   end Set_Expiration;

   -------------------------------
   --  Set_Message_Id Procedure --
   -------------------------------
   procedure Set_Message_Id (Id : String) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Id);
      pragma Warnings (On);
      null;
   end Set_Message_Id;

   -----------------------------
   --  Set_Priority Procedure --
   -----------------------------
   procedure Set_Priority (Value : Priority) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Value);
      pragma Warnings (On);
      null;
   end Set_Priority;

   --------------------------------
   --  Set_Redelivered Procedure --
   --------------------------------
   procedure Set_Redelivered (Redelivered : Boolean) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Redelivered);
      pragma Warnings (On);
      null;
   end Set_Redelivered;

   -----------------------------
   --  Set_Reply_To Procedure --
   -----------------------------
   procedure Set_Reply_To (Reply_To : MOMA.Destinations.Destination) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Reply_To);
      pragma Warnings (On);
      null;
   end Set_Reply_To;

   ------------------------------
   --  Set_Timestamp Procedure --
   ------------------------------
   procedure Set_Timestamp (Timestamp : Time) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Timestamp);
      pragma Warnings (On);
      null;
   end Set_Timestamp;

end MOMA.Messages;
