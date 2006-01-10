------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 PORTABLEINTERCEPTOR.IORINTERCEPTOR_3_0                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
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

with PortableInterceptor.IORInterceptor_3_0.Impl;

package body PortableInterceptor.IORInterceptor_3_0 is

   -----------------------------------
   -- Adapter_Manager_State_Changed --
   -----------------------------------

   procedure Adapter_Manager_State_Changed
     (Self  : Local_Ref;
      Id    : AdapterManagerId;
      State : AdapterState)
   is
      Self_Ref : CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      PortableInterceptor.IORInterceptor_3_0.Impl.Adapter_Manager_State_Changed
        (Impl.Object_Ptr (Entity_Of (Self)),
         Id,
         State);
   end Adapter_Manager_State_Changed;

--   ---------------------------
--   -- Adapter_State_Changed --
--   ---------------------------
--
--   procedure Adapter_State_Changed
--     (Self      : Local_Ref;
--      Templates : ObjectReferenceTemplate.Abstract_Value_Ref;
--      State     : AdapterState)
--   is
--      Self_Ref : CORBA.Object.Ref := CORBA.Object.Ref (Self);
--   begin
--
--      if CORBA.Object.Is_Nil (Self_Ref) then
--         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
--      end if;
--
--      PortableInterceptor.IORInterceptor_3_0.Impl.Adapter_State_Changed
--        (Impl.Object_Ptr (Entity_Of (Self)),
--         Templates,
--         State);
--   end Adapter_State_Changed;

   ----------------------------
   -- Components_Established --
   ----------------------------

   procedure Components_Established
     (Self : Local_Ref;
      Info : PortableInterceptor.IORInfo.Local_Ref)
   is
      Self_Ref : CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      PortableInterceptor.IORInterceptor_3_0.Impl.Components_Established
        (Impl.Object_Ptr (Entity_Of (Self)),
         Info);
   end Components_Established;

end PortableInterceptor.IORInterceptor_3_0;
