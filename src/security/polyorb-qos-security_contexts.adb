------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . Q O S . S E C U R I T Y _ C O N T E X T S         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

pragma Ada_2012;

with Ada.Streams;
with PolyORB.Utils.Unchecked_Deallocation;

with PolyORB.Buffers;
with PolyORB.Errors;
with PolyORB.Initialization;
with PolyORB.QoS.Service_Contexts;
with PolyORB.Representations.CDR.Common;
with PolyORB.Utils.Strings;

package body PolyORB.QoS.Security_Contexts is

   use PolyORB.Buffers;
   use PolyORB.Representations.CDR.Common;
   use PolyORB.Security.Authorization_Elements;
   use PolyORB.Security.Identities;
   use PolyORB.Security.Types;
   use PolyORB.Types;

   function To_SASContext_Service_Context
     (QoS : QoS_Parameter_Access)
      return PolyORB.QoS.Service_Contexts.Service_Context;
   --  Convert QoS parameter to GIOP Service Context

   function To_QoS_Security_Context_Parameter
     (SC : PolyORB.QoS.Service_Contexts.Service_Context)
      return QoS_Parameter_Access;
   --  Convert GIOP Service Context to QoS parameter

   procedure Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  Register QoS <=> Service Context convertors

      PolyORB.QoS.Service_Contexts.Register
        (Compound_Security, To_SASContext_Service_Context'Access);
      PolyORB.QoS.Service_Contexts.Register
        (PolyORB.QoS.Service_Contexts.SecurityAttributeService,
         To_QoS_Security_Context_Parameter'Access);
   end Initialize;

   ----------------------
   -- Release_Contents --
   ----------------------

   overriding procedure Release_Contents
     (QoS : access QoS_Security_Context_Parameter)
   is

      procedure Free is
        new PolyORB.Utils.Unchecked_Deallocation.Free


        (Object => Ada.Streams.Stream_Element_Array,


         Name   => PolyORB.Security.Types.Stream_Element_Array_Access);

   begin
      case QoS.Context_Kind is
         when Establish_Context =>
            Release_Contents (QoS.Authorization_Token);
            Destroy (QoS.Identity_Token);
            Free (QoS.Client_Authentication_Token);

         when Complete_Establish_Context =>
            Free (QoS.Final_Context_Token);

         when Context_Error =>
            Free (QoS.Error_Token);

         when Message_In_Context =>
            null;
      end case;
   end Release_Contents;

   ---------------------------------------
   -- To_QoS_Security_Context_Parameter --
   ---------------------------------------

   function To_QoS_Security_Context_Parameter
     (SC : PolyORB.QoS.Service_Contexts.Service_Context)
      return QoS_Parameter_Access
   is
      use PolyORB.Errors;

      Buffer : aliased Buffer_Type;
      Error  : Error_Container;

      pragma Warnings (Off);

   begin
      Decapsulate (SC.Context_Data, Buffer'Access);

      case Short'(Unmarshall (Buffer'Access)) is
         when 0 =>
            declare
               Result               : constant
                 QoS_Security_Context_Parameter_Access
                 := new QoS_Security_Context_Parameter (Establish_Context);
               Client_Context_Id    : Context_Id;
               Authorization_Token  : Authorization_Element_Lists.List;
               Length               : Unsigned_Long;
               Element              : Authorization_Element_Access;
               Identity             : Identity_Access;
               Identity_Type        : Identity_Token_Type;
               Authentication_Token : Stream_Element_Array_Access;

            begin
               Client_Context_Id :=
                 Context_Id (Unsigned_Long_Long'(Unmarshall (Buffer'Access)));

               --  Authorization Token

               Length := Unmarshall (Buffer'Access);

               for J in 1 .. Length loop
                  declare
                     The_Type : constant Element_Type
                       := Element_Type
                       (Unsigned_Long'(Unmarshall (Buffer'Access)));

                  begin
                     Authorization_Element_Lists.Append
                       (Authorization_Token,
                        Create (The_Type, Unmarshall (Buffer'Access)));
                  end;
               end loop;

               --  Identity Token

               Identity_Type :=
                 Identity_Token_Type
                 (Unsigned_Long'(Unmarshall (Buffer'Access)));

               case Identity_Type is
                  when ITT_Absent | ITT_Anonymous =>
                     declare
                        Aux : Boolean;

                     begin
                        Aux := Unmarshall (Buffer'Access);
                     end;

                     Create
                       (Identity_Type,
                        Ada.Streams.Stream_Element_Array'(1 .. 0 => 0),
                        Identity,
                        Error);

                  when others =>
                     Create
                       (Identity_Type,
                        Unmarshall (Buffer'Access),
                        Identity,
                        Error);
               end case;

               if Found (Error) then
                  raise Program_Error;
               end if;

               --  Client Authentication Token

               declare
                  Aux : constant Ada.Streams.Stream_Element_Array
                    := Unmarshall (Buffer'Access);

               begin
                  if Aux'Length /= 0 then
                     Authentication_Token :=
                       new Ada.Streams.Stream_Element_Array'(Aux);
                  end if;
               end;

               Result.Client_Context_Id           := Client_Context_Id;
               Result.Authorization_Token         := Authorization_Token;
               Result.Identity_Token              := Identity;
               Result.Client_Authentication_Token := Authentication_Token;

               return QoS_Parameter_Access (Result);

--  WAG:5.03: This code cause Constraint_Error at caller side because
--  value of Result.Kind is invalid. (See EA31-006 for more details)
--               return
--                 new QoS_Security_Context_Parameter'
--                 (Context_Kind                => Establish_Context,
--                  Client_Context_Id           => Client_Context_Id,
--                  Client_Authentication_Token => Authentication_Token,
--                  Identity_Token              => Identity,
--                  Authorization_Token         => Authorization_Token);
            end;

         when 1 =>
            declare
               Result            : constant
                 QoS_Security_Context_Parameter_Access
                 := new QoS_Security_Context_Parameter
                 (Complete_Establish_Context);
               Client_Context_Id : Context_Id;
               Context_Stateful  : Boolean;
               Final_Token       : Stream_Element_Array_Access := null;

            begin
               Client_Context_Id :=
                 Context_Id (Unsigned_Long_Long'(Unmarshall (Buffer'Access)));

               Context_Stateful := Unmarshall (Buffer'Access);

               declare
                  Aux : constant Ada.Streams.Stream_Element_Array
                    := Unmarshall (Buffer'Access);

               begin
                  if Aux'Length /= 0 then
                     Final_Token :=
                       new Ada.Streams.Stream_Element_Array'(Aux);
                  end if;
               end;

               Result.Client_Context_Id   := Client_Context_Id;
               Result.Context_Stateful    := Context_Stateful;
               Result.Final_Context_Token := Final_Token;

               return QoS_Parameter_Access (Result);
            end;

         when 4 =>
            declare
               Result            : constant
                 QoS_Security_Context_Parameter_Access
                 := new QoS_Security_Context_Parameter (Context_Error);
               Client_Context_Id : Context_Id;
               Major_Status      : Long;
               Minor_Status      : Long;
               Error_Token       : Stream_Element_Array_Access;

            begin
               Client_Context_Id :=
                 Context_Id (Unsigned_Long_Long'(Unmarshall (Buffer'Access)));

               Major_Status := Unmarshall (Buffer'Access);
               Minor_Status := Unmarshall (Buffer'Access);

               declare
                  Aux : constant Ada.Streams.Stream_Element_Array
                    := Unmarshall (Buffer'Access);

               begin
                  if Aux'Length /= 0 then
                     Error_Token :=
                       new Ada.Streams.Stream_Element_Array'(Aux);
                  end if;
               end;

               Result.Client_Context_Id := Client_Context_Id;
               Result.Major_Status      := Major_Status;
               Result.Minor_Status      := Minor_Status;
               Result.Error_Token       := Error_Token;

               return QoS_Parameter_Access (Result);
            end;

         when 5 =>
            declare
               Result            : constant
                 QoS_Security_Context_Parameter_Access
                 := new QoS_Security_Context_Parameter (Message_In_Context);
               Client_Context_Id : Context_Id;
               Discard_Context   : Boolean;

            begin
               Client_Context_Id :=
                 Context_Id (Unsigned_Long_Long'(Unmarshall (Buffer'Access)));
               Discard_Context := Unmarshall (Buffer'Access);

               Result.Client_Context_Id := Client_Context_Id;
               Result.Discard_Context   := Discard_Context;

               return QoS_Parameter_Access (Result);
            end;

         when others =>
            raise Program_Error;
      end case;

      return null;
   end To_QoS_Security_Context_Parameter;

   -----------------------------------
   -- To_SASContext_Service_Context --
   -----------------------------------

   function To_SASContext_Service_Context
     (QoS : QoS_Parameter_Access)
      return PolyORB.QoS.Service_Contexts.Service_Context
   is
      Result : PolyORB.QoS.Service_Contexts.Service_Context
        := (PolyORB.QoS.Service_Contexts.SecurityAttributeService, null);

   begin
      if QoS = null then
         return Result;
      end if;

      declare
         use PS.Authorization_Elements.Authorization_Element_Lists;

         SASContext : QoS_Security_Context_Parameter
           renames QoS_Security_Context_Parameter (QoS.all);
         Buffer     : Buffer_Access := new Buffer_Type;

      begin
         Start_Encapsulation (Buffer);

         case SASContext.Context_Kind is
            when Establish_Context =>
               Marshall (Buffer, Short (0));

               Marshall
                 (Buffer, Unsigned_Long_Long (SASContext.Client_Context_Id));

               --  Authorization Token

               Marshall
                 (Buffer,
                  Unsigned_Long (Length (SASContext.Authorization_Token)));

               declare
                  Iter : Iterator := First (SASContext.Authorization_Token);

               begin
                  while not Last (Iter) loop
                     Marshall
                       (Buffer,
                        Unsigned_Long
                        (Get_Authorization_Element_Type (Value (Iter).all)));
                     Marshall (Buffer, Encode (Value (Iter).all));

                     Next (Iter);
                  end loop;
               end;

               --  Identity Token

               if SASContext.Identity_Token = null then
                  Marshall (Buffer, Unsigned_Long (ITT_Absent));
                  Marshall (Buffer, True);

               elsif Get_Token_Type (SASContext.Identity_Token)
                 = ITT_Anonymous
               then
                  Marshall (Buffer, Unsigned_Long (ITT_Anonymous));
                  Marshall (Buffer, True);

               else
                  Marshall
                    (Buffer,
                     Unsigned_Long
                     (Get_Token_Type (SASContext.Identity_Token)));
                  Marshall (Buffer, Encode (SASContext.Identity_Token));
               end if;

               --  Client Authentication Token

               if SASContext.Client_Authentication_Token = null then
                  Marshall (Buffer, Unsigned_Long (0));

               else
                  Marshall
                    (Buffer, SASContext.Client_Authentication_Token.all);
               end if;

            when Complete_Establish_Context =>
               Marshall (Buffer, Short (1));

               Marshall
                 (Buffer, Unsigned_Long_Long (SASContext.Client_Context_Id));

               Marshall (Buffer, SASContext.Context_Stateful);

               if SASContext.Final_Context_Token /= null then
                  Marshall (Buffer, SASContext.Final_Context_Token.all);

               else
                  Marshall
                    (Buffer, Ada.Streams.Stream_Element_Array'(1 .. 0 => 0));
               end if;

            when Context_Error =>
               Marshall (Buffer, Short (4));

               Marshall
                 (Buffer, Unsigned_Long_Long (SASContext.Client_Context_Id));
               Marshall (Buffer, SASContext.Major_Status);
               Marshall (Buffer, SASContext.Minor_Status);

               if SASContext.Error_Token /= null then
                  Marshall (Buffer, SASContext.Error_Token.all);

               else
                  Marshall
                    (Buffer, Ada.Streams.Stream_Element_Array'(1 .. 0 => 0));
               end if;

            when Message_In_Context =>
               Marshall (Buffer, Short (5));

               Marshall
                 (Buffer, Unsigned_Long_Long (SASContext.Client_Context_Id));
               Marshall (Buffer, SASContext.Discard_Context);
         end case;

         Result.Context_Data := new Encapsulation'(Encapsulate (Buffer));

         Release (Buffer);
      end;

      return Result;
   end To_SASContext_Service_Context;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
   begin
      Register_Module
        (Module_Info'
         (Name      => +"polyorb.qos.security_contexts",
          Conflicts => Empty,
          Depends   => Empty,
          Provides  => Empty,
          Implicit  => False,
          Init      => Initialize'Access,
          Shutdown  => null));
   end;
end PolyORB.QoS.Security_Contexts;
