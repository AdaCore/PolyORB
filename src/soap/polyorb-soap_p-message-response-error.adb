------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.SOAP_P.MESSAGE.RESPONSE.ERROR                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2000-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Any; use PolyORB.Any;
with PolyORB.Types; use PolyORB.Types;

with PolyORB.SOAP_P.Types;

package body PolyORB.SOAP_P.Message.Response.Error is

   Version_Mismatch_Faultcode : constant String := "VersionMismatch";
   Must_Understand_Faultcode  : constant String := "MustUnderstand";
   Client_Faultcode           : constant String := "Client";
   Server_Faultcode           : constant String := "Server";

   Start_Fault_Env            : constant String := "<SOAP-ENV:Fault>";
   End_Fault_Env              : constant String := "</SOAP-ENV:Fault>";

   Start_Faultcode            : constant String := "<faultcode>";
   End_Faultcode              : constant String := "</faultcode>";
   Start_Faultstring          : constant String := "<faultstring>";
   End_Faultstring            : constant String := "</faultstring>";

   pragma Warnings (Off);
   pragma Unreferenced
     (Start_Faultcode,
      End_Faultcode,
      Start_Faultstring,
      End_Faultstring);
   pragma Warnings (On);

   function Fault_Code (Name, Subname : String) return Faultcode;
   --  Returns the Faultcode for Name and Subname. If Subname is empty it
   --  returns Name otherwise it returns Name & '.' & Subname.

   -----------
   -- Build --
   -----------

   function Build
     (Faultcode   : Error.Faultcode;
      Faultstring : String)
     return Object
   is
      use SOAP_P.Types;
      use type SOAP_P.Parameters.List;

      O : Object;
      P : SOAP_P.Parameters.List;
   begin
      --  Set Wrapper Name

      Set_Wrapper_Name (O, "Fault");

      --  Set Faultcode and Faultstring

      P :=  +NamedValue'
        (To_PolyORB_String ("faultcode"),
         To_Any (To_PolyORB_String (String (Faultcode))), ARG_IN)
        & NamedValue'
        (To_PolyORB_String ("faultstring"),
         To_Any (To_PolyORB_String (Faultstring)), ARG_IN);

      --  Set parameters for this error object

      Set_Parameters (O, P);

      return O;
   end Build;

   ------------
   -- Client --
   ------------

   function Client (Subname : String := "") return Faultcode is
   begin
      return Fault_Code (Client_Faultcode, Subname);
   end Client;

   ----------------
   -- Fault_Code --
   ----------------

   function Fault_Code (Name, Subname : String) return Faultcode is
   begin
      if Subname = "" then
         return Faultcode (Name);

      else
         return Faultcode (Name & '.' & Subname);
      end if;
   end Fault_Code;

   ----------
   -- From --
   ----------

   function From (P : Message.Payload.Object) return Object
   is
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Warnings (On);
      N : Object;
      pragma Warnings (Off, N);
      --  Not initialized.
      --  XXX check whether any information from P should
      --  be included in N.
   begin
      return N;
   end From;

   --------------
   -- Is_Error --
   --------------

   function Is_Error (E : Object) return Boolean
   is
      pragma Warnings (Off);
      pragma Unreferenced (E);
      pragma Warnings (On);
   begin
      return True;
   end Is_Error;

   ---------------------
   -- Must_Understand --
   ---------------------

   function Must_Understand (Subname : String := "") return Faultcode is
   begin
      return Fault_Code (Must_Understand_Faultcode, Subname);
   end Must_Understand;

   ------------
   -- Server --
   ------------

   function Server (Subname : String := "") return Faultcode is
   begin
      return Fault_Code (Server_Faultcode, Subname);
   end Server;

   ----------------------
   -- Version_Mismatch --
   ----------------------

   function Version_Mismatch (Subname : String := "") return Faultcode is
   begin
      return Fault_Code (Version_Mismatch_Faultcode, Subname);
   end Version_Mismatch;

   ---------------
   -- XML_Image --
   ---------------

   function XML_Image (E : Object) return Unbounded_String is
      NL           : constant String := ASCII.CR & ASCII.LF;
      Message_Body : Unbounded_String;

   begin
      --  Fault Env

      Append (Message_Body, Start_Fault_Env & NL);

      --  Fault's parameters

      declare
         P : constant SOAP_P.Parameters.List := Parameters (E);
      begin
         for K in 1 .. SOAP_P.Parameters.Argument_Count (P) loop
            declare
               P_K    : constant PolyORB.Any.NamedValue
                 := SOAP_P.Parameters.Argument (P, K);
               P_Name : constant String := SOAP_P.Types.Name (P_K);
            begin
               Append
                 (Message_Body,
                  "   "
                  & Tag (P_Name, Start => True)
                  & Types.Value_Image (P_K)
                  & Tag (P_Name, Start => False)
                  & NL);
            end;
         end loop;
      end;

      --  End Fault Env

      Append (Message_Body, End_Fault_Env & NL);

      return Message_Body;
   end XML_Image;

end PolyORB.SOAP_P.Message.Response.Error;
