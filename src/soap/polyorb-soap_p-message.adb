------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . S O A P _ P . M E S S A G E                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2000-2006, Free Software Foundation, Inc.          --
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

with PolyORB.Any;

with PolyORB.SOAP_P.Types;
with SOAP.Utils;
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

      Append (Message_Body,
              SOAP.Utils.Tag ("awsns:" & Wrapper_Name (M), False) & NL);

      return Message_Body;
   end XML_Image;

end PolyORB.SOAP_P.Message;
