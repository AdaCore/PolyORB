------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . S O A P _ P . M E S S A G E                --
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

with PolyORB.Any;

with PolyORB.SOAP_P.Types;
with PolyORB.SOAP_P.Message.Response;

package body PolyORB.SOAP_P.Message is

   ----------------
   -- Name_Space --
   ----------------

   function Name_Space (M : Object'Class) return String is
   begin
      return To_String (M.Name_Space);
   end Name_Space;

   ----------------
   -- Parameters --
   ----------------

   function Parameters (M : Object'Class) return SOAP_P.Parameters.List is
   begin
      return M.P;
   end Parameters;

   --------------------
   -- Set_Name_Space --
   --------------------

   procedure Set_Name_Space (M : in out Object'Class; Name  : String) is
   begin
      M.Name_Space := To_Unbounded_String (Name);
   end Set_Name_Space;

   --------------------
   -- Set_Parameters --
   --------------------

   procedure Set_Parameters
     (M     : in out Object'Class;
      P_Set : SOAP_P.Parameters.List) is
   begin
      M.P := P_Set;
   end Set_Parameters;

   ----------------------
   -- Set_Wrapper_Name --
   ----------------------

   procedure Set_Wrapper_Name
     (M     : in out Object'Class;
      Name  : String) is
   begin
      M.Wrapper_Name := To_Unbounded_String (Name);
   end Set_Wrapper_Name;

   ------------------
   -- Wrapper_Name --
   ------------------

   function Wrapper_Name (M : Object'class) return String is
   begin
      return To_String (M.Wrapper_Name);
   end Wrapper_Name;

   ---------------
   -- XML_Image --
   ---------------

   function XML_Image (M : Object) return Unbounded_String is
      NL           : constant String := ASCII.CR & ASCII.LF;
      Message_Body : Unbounded_String;

   begin
      --  Procedure

      Append (Message_Body,
              "<awsns:" & Wrapper_Name (M)
              & " xmlns:awsns=""http://mns.org/"">" & NL);

      --  Procedure's parameters

      declare
         P : constant SOAP_P.Parameters.List := Parameters (M);
      begin
         for K in 1 .. SOAP_P.Parameters.Argument_Count (P) loop
            declare
               Param : constant PolyORB.Any.NamedValue
                 := SOAP_P.Parameters.Argument (P, K);
               use PolyORB.Any;
            begin
               if Param.Arg_Modes = ARG_INOUT
                 or else
                 (Param.Arg_Modes = ARG_IN
                  xor
                  (SOAP_P.Message.Object'Class (M)
                   in SOAP_P.Message.Response.Object'Class))
               then
                  Append
                    (Message_Body,
                     "   "
                     & Types.XML_Image (Param)
                     & NL);
               end if;
            end;
         end loop;
      end;

      --  Close payload objects.

      Append (Message_Body, Tag ("awsns:" & Wrapper_Name (M), False) & NL);

      return Message_Body;
   end XML_Image;

end PolyORB.SOAP_P.Message;
