------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

--  $Id$

with PolyORB.Any; use PolyORB.Any;
with PolyORB.Types; use PolyORB.Types;

with PolyORB.SOAP_P.Types;
with SOAP.Utils;

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


   function Fault_Code (Name, Subname : in String) return Faultcode;
   --  Returns the Faultcode for Name and Subname. If Subname is empty it
   --  returns Name otherwise it returns Name & '.' & Subname.

   -----------
   -- Build --
   -----------

   function Build
     (Faultcode   : in Error.Faultcode;
      Faultstring : in String)
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

   function Client (Subname : in String := "") return Faultcode is
   begin
      return Fault_Code (Client_Faultcode, Subname);
   end Client;

   ----------------
   -- Fault_Code --
   ----------------

   function Fault_Code (Name, Subname : in String) return Faultcode is
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

   function From (P : in Message.Payload.Object) return Object
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

   function Is_Error (E : in Object) return Boolean
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

   function Must_Understand (Subname : in String := "") return Faultcode is
   begin
      return Fault_Code (Must_Understand_Faultcode, Subname);
   end Must_Understand;

   ------------
   -- Server --
   ------------

   function Server (Subname : in String := "") return Faultcode is
   begin
      return Fault_Code (Server_Faultcode, Subname);
   end Server;

   ----------------------
   -- Version_Mismatch --
   ----------------------

   function Version_Mismatch (Subname : in String := "") return Faultcode is
   begin
      return Fault_Code (Version_Mismatch_Faultcode, Subname);
   end Version_Mismatch;

   ---------------
   -- XML_Image --
   ---------------

   function XML_Image (E : in Object) return Unbounded_String is
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
                  & SOAP.Utils.Tag (P_Name, Start => True)
                  & Types.Image (P_K)
                  & SOAP.Utils.Tag (P_Name, Start => False)
                  & NL);
            end;
         end loop;
      end;

      --  End Fault Env

      Append (Message_Body, End_Fault_Env & NL);

      return Message_Body;
   end XML_Image;

end PolyORB.SOAP_P.Message.Response.Error;
