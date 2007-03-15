------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . Q O S . S E R V I C E _ C O N T E X T S          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2005 Free Software Foundation, Inc.           --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

with PolyORB.Request_QoS;

package body PolyORB.QoS.Service_Contexts is

   use PolyORB.Representations.CDR.Common;
   use PolyORB.Request_QoS;
   use PolyORB.Types;
   use Service_Context_Lists;

   procedure Rebuild_Service_Contexts (QoS : in out QoS_Parameters);

   procedure Rebuild_QoS_Parameters (QoS : in out QoS_Parameters);

   function Get_Converter (Context_Id : Service_Id) return To_QoS_Parameter;

   procedure Free is
     new Ada.Unchecked_Deallocation (Encapsulation, Encapsulation_Access);

   To_Service_Context_Registry : array (QoS_Kind) of To_Service_Context
     := (others => null);

   type To_QoS_Parameter_Item is record
      Context_Id : Service_Id;
      Converter  : To_QoS_Parameter;
   end record;

   package To_QoS_Parameter_Lists is
     new PolyORB.Utils.Chained_Lists (To_QoS_Parameter_Item);

   To_QoS_Parameter_Registry : To_QoS_Parameter_Lists.List;

   -------------------
   -- Get_Converter --
   -------------------

   function Get_Converter
     (Context_Id : Service_Id)
     return To_QoS_Parameter
   is
      use To_QoS_Parameter_Lists;

      Iter : To_QoS_Parameter_Lists.Iterator
        := First (To_QoS_Parameter_Registry);

   begin
      while not Last (Iter) loop
         if Value (Iter).Context_Id = Context_Id then
            return Value (Iter).Converter;
         end if;

         Next (Iter);
      end loop;

      return null;
   end Get_Converter;

   --------------
   -- Register --
   --------------

   procedure Register (QoS : QoS_Kind; Converter : To_Service_Context) is
   begin
      To_Service_Context_Registry (QoS) := Converter;
   end Register;

   procedure Register (Id : Service_Id; Converter : To_QoS_Parameter) is
      use To_QoS_Parameter_Lists;
   begin
      Append (To_QoS_Parameter_Registry, (Id, Converter));
   end Register;

   ----------------------------
   -- Rebuild_QoS_Parameters --
   ----------------------------

   procedure Rebuild_QoS_Parameters (QoS : in out QoS_Parameters) is
      SC   : constant QoS_GIOP_Service_Contexts_Parameter_Access
        := QoS_GIOP_Service_Contexts_Parameter_Access
            (QoS (GIOP_Service_Contexts));
      Iter : Iterator;

   begin
      if SC = null then
         return;
      end if;

      Iter := First (SC.Service_Contexts);

      while not Last (Iter) loop
         declare
            Cnv : constant To_QoS_Parameter
              := Get_Converter (Value (Iter).Context_Id);
            Aux : QoS_Parameter_Access;
         begin
            if Cnv /= null then
               Aux := Cnv.all (Value (Iter).all);
               if Aux /= null then
                  Release (QoS (Aux.Kind));
                  QoS (Aux.Kind) := Aux;
               end if;
            end if;
         end;
         Next (Iter);
      end loop;
   end Rebuild_QoS_Parameters;

   ----------------------------------
   -- Rebuild_Reply_QoS_Parameters --
   ----------------------------------

   procedure Rebuild_Reply_QoS_Parameters
     (Req : PolyORB.Requests.Request_Access)
   is
      QoS  : QoS_Parameters := Get_Reply_QoS (Req);

   begin
      Rebuild_QoS_Parameters (QoS);
      Set_Reply_QoS (Req, QoS);
   end Rebuild_Reply_QoS_Parameters;

   ------------------------------------
   -- Rebuild_Reply_Service_Contexts --
   ------------------------------------

   procedure Rebuild_Reply_Service_Contexts
     (Req : PolyORB.Requests.Request_Access)
   is
      QoS : QoS_Parameters := Get_Reply_QoS (Req);

   begin
      Rebuild_Service_Contexts (QoS);
      Set_Reply_QoS (Req, QoS);
   end Rebuild_Reply_Service_Contexts;

   ------------------------------------
   -- Rebuild_Request_QoS_Parameters --
   ------------------------------------

   procedure Rebuild_Request_QoS_Parameters
     (Req : PolyORB.Requests.Request_Access)
   is
      QoS  : QoS_Parameters := Get_Request_QoS (Req);

   begin
      Rebuild_QoS_Parameters (QoS);
      Set_Request_QoS (Req, QoS);
   end Rebuild_Request_QoS_Parameters;

   --------------------------------------
   -- Rebuild_Request_Service_Contexts --
   --------------------------------------

   procedure Rebuild_Request_Service_Contexts
     (Req : PolyORB.Requests.Request_Access)
   is
      QoS : QoS_Parameters := Get_Request_QoS (Req);

   begin
      Rebuild_Service_Contexts (QoS);
      Set_Request_QoS (Req, QoS);
   end Rebuild_Request_Service_Contexts;

   ------------------------------
   -- Rebuild_Service_Contexts --
   ------------------------------

   procedure Rebuild_Service_Contexts (QoS : in out QoS_Parameters) is
      SC : QoS_GIOP_Service_Contexts_Parameter_Access
        := QoS_GIOP_Service_Contexts_Parameter_Access
        (QoS (GIOP_Service_Contexts));

      Aux   : Service_Context;
      Iter  : Iterator;
      Added : Boolean;

   begin
      if SC = null then
         SC := new QoS_GIOP_Service_Contexts_Parameter;
      end if;

      for J in QoS_Kind loop
         --  XXX We may define subtype of QoS_Kind which can't have
         --  GIOP_Service_Contexts literal.
         if J /= GIOP_Service_Contexts
           and then To_Service_Context_Registry (J) /= null
         then
            Aux := To_Service_Context_Registry (J).all (QoS (J));

            Iter  := First (SC.Service_Contexts);
            Added := False;
            while not Last (Iter) loop
               if Value (Iter).Context_Id = Aux.Context_Id then
                  if Aux.Context_Data = null then
                     Remove (SC.Service_Contexts, Iter);
                  else
                     Free (Value (Iter).Context_Data);
                     Value (Iter).Context_Data := Aux.Context_Data;
                  end if;
                  Added := True;
                  exit;
               end if;
               Next (Iter);
            end loop;

            if not Added
              and then Aux.Context_Data /= null
            then
               Append (SC.Service_Contexts, Aux);
            end if;
         end if;
      end loop;

      QoS (GIOP_Service_Contexts) := QoS_Parameter_Access (SC);
   end Rebuild_Service_Contexts;

   ----------------------
   -- Release_Contents --
   ----------------------

   procedure Release_Contents
     (QoS : access QoS_GIOP_Service_Contexts_Parameter)
   is
      Iter : Iterator := First (QoS.Service_Contexts);

   begin
      while not Last (Iter) loop
         Free (Value (Iter).Context_Data);
         Next (Iter);
      end loop;

      Deallocate (QoS.Service_Contexts);
   end Release_Contents;

end PolyORB.QoS.Service_Contexts;
