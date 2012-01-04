------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . Q O S . C O D E _ S E T S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Buffers;
with PolyORB.Initialization;
with PolyORB.Representations.CDR.Common;
with PolyORB.QoS.Service_Contexts;
with PolyORB.Types;
with PolyORB.Utils.Strings;

package body PolyORB.QoS.Code_Sets is

   use PolyORB.Buffers;
   use PolyORB.GIOP_P.Code_Sets;
   use PolyORB.Representations.CDR.Common;
   use PolyORB.QoS.Service_Contexts;
   use PolyORB.Types;

   function To_CodeSets_Service_Context
     (QoS : QoS_Parameter_Access)
      return Service_Context;

   function To_QoS_GIOP_Code_Sets_Parameter
     (SC : Service_Context)
      return QoS_Parameter_Access;

   ---------------------------------
   -- To_CodeSets_Service_Context --
   ---------------------------------

   function To_CodeSets_Service_Context
     (QoS : QoS_Parameter_Access)
      return Service_Context
   is
      Result : Service_Context := (CodeSets, null);

   begin
      if QoS = null then
         return Result;
      end if;

      declare
         CS     : QoS_GIOP_Code_Sets_Parameter
           renames QoS_GIOP_Code_Sets_Parameter (QoS.all);
         Buffer : Buffer_Access := new Buffer_Type;

      begin
         Start_Encapsulation (Buffer);

         Marshall (Buffer, Unsigned_Long (CS.Char_Data));
         Marshall (Buffer, Unsigned_Long (CS.Wchar_Data));
         Result.Context_Data := new Encapsulation'(Encapsulate (Buffer));

         Release (Buffer);
      end;

      return Result;
   end To_CodeSets_Service_Context;

   -------------------------------------
   -- To_QoS_GIOP_Code_Sets_Parameter --
   -------------------------------------

   function To_QoS_GIOP_Code_Sets_Parameter
     (SC : Service_Context)
      return QoS_Parameter_Access
   is
      Buffer     : aliased Buffer_Type;
      Char_Data  : Code_Set_Id;
      Wchar_Data : Code_Set_Id;

   begin
      Decapsulate (SC.Context_Data, Buffer'Access);

      Char_Data := Code_Set_Id (Unsigned_Long'(Unmarshall (Buffer'Access)));
      Wchar_Data := Code_Set_Id (Unsigned_Long'(Unmarshall (Buffer'Access)));

      return
        new QoS_GIOP_Code_Sets_Parameter'
        (Kind       => GIOP_Code_Sets,
         Char_Data  => Char_Data,
         Wchar_Data => Wchar_Data);
   end To_QoS_GIOP_Code_Sets_Parameter;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      --  CodeSets service context

      Register (GIOP_Code_Sets, To_CodeSets_Service_Context'Access);
      Register (CodeSets, To_QoS_GIOP_Code_Sets_Parameter'Access);
   end Initialize;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
   begin
      Register_Module
        (Module_Info'
         (Name      => +"request_qos.code_sets",
          Conflicts => Empty,
          Depends   => Empty,
          Provides  => Empty,
          Implicit  => False,
          Init      => Initialize'Access,
          Shutdown  => null));
   end;
end PolyORB.QoS.Code_Sets;
