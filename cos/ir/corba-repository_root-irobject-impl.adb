------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  C O R B A . R E P O S I T O R Y _ R O O T . I R O B J E C T . I M P L   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2007, Free Software Foundation, Inc.          --
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

with CORBA.Repository_Root.Contained.Impl;
with CORBA.Repository_Root.Container.Impl;
with CORBA.Repository_Root.IRObject.Skel;
pragma Warnings (Off, CORBA.Repository_Root.IRObject.Skel);

with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);

package body CORBA.Repository_Root.IRObject.Impl is

   -----------
   -- Debug --
   -----------

   use PolyORB.Log;

--    package L is new PolyORB.Log.Facility_Log ("irobject.impl");
--    procedure O (Message : Standard.String; Level : Log_Level := Debug)
--      renames L.Output;
--   function C (Level : Log_Level := Debug) return Boolean
--     renames L.Enabled;
--   pragma Unreferenced (C); --  For conditional pragma Debug

   package L2 is new PolyORB.Log.Facility_Log ("irobject.impl_method_trace");
   procedure O2 (Message : Standard.String; Level : Log_Level := Debug)
     renames L2.Output;
   function C2 (Level : Log_Level := Debug) return Boolean
     renames L2.Enabled;
   pragma Unreferenced (C2); --  For conditional pragma Debug

   -------------
   -- destroy --
   -------------

   procedure destroy (Self : access Object) is
   begin
      pragma Debug (O2 ("IRObject destroy: enter"));

      --  This is overriden in each necessary defs

      case Self.Def_Kind is
         when
            dk_Repository |
            dk_Primitive  =>

            --  Can't destroy Repository or Primitive object

            CORBA.Raise_Bad_Inv_Order
              (CORBA.System_Exception_Members'
               (Minor => 2,
                Completed => CORBA.Completed_No));

         when others =>

            --  Redispatch

            --  Destroy (Object_Ptr (Self));

            --  ??? implementation is not complete

            --  FIXME memory leak, should remove the contained from the
            --  previous container.

            declare
               Cont : constant Contained.Impl.Object_Ptr :=
                        Contained.Impl.To_Contained (Get_Real_Object (Self));
            begin
               Container.Impl.Delete_From_Contents
                 (Container.Impl.To_Object
                  (Contained.Impl.get_defined_in (Cont)),
                  Cont);
            end;
      end case;
   end destroy;

   ------------------
   -- get_def_kind --
   ------------------

   function get_def_kind
     (Self : access Object) return CORBA.Repository_Root.DefinitionKind is
   begin
      return Self.Def_Kind;
   end get_def_kind;

   ---------------------
   -- Get_Real_Object --
   ---------------------

   function Get_Real_Object (Self : access Object) return Object_Ptr is
   begin
      return Self.Real_Object;
   end Get_Real_Object;

   ----------
   -- Init --
   ----------

   procedure Init
     (Self        : access Object;
      Real_Object : IRObject.Impl.Object_Ptr;
      Def_Kind    : CORBA.Repository_Root.DefinitionKind)
   is
   begin
      pragma Debug (O2 ("Init: enter"));
      Self.Def_Kind    := Def_Kind;
      Self.Real_Object := Real_Object;
      pragma Debug (O2 ("Init: end"));
   end Init;

end CORBA.Repository_Root.IRObject.Impl;
