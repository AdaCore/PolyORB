------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . B I N D I N G _ O B J E C T _ Q O S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2006, Free Software Foundation, Inc.             --
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

with PolyORB.Annotations;

package body PolyORB.Binding_Object_QoS is

   type Binding_Object_QoS_Note is new Annotations.Note with record
      QoS : PolyORB.QoS.QoS_Parameters;
   end record;

   procedure Destroy (N : in out Binding_Object_QoS_Note);

   Empty_Binding_Object_QoS_Note : constant Binding_Object_QoS_Note
     := (Annotations.Note with QoS => (others => null));

   Registry : array (PolyORB.QoS.QoS_Kind) of QoS_Compatibility_Check_Proc;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (N : in out Binding_Object_QoS_Note) is
   begin
      for J in PolyORB.QoS.QoS_Kind loop
         PolyORB.QoS.Release (N.QoS (J));
      end loop;
   end Destroy;

   ----------------------------
   -- Get_Binding_Object_QoS --
   ----------------------------

   function Get_Binding_Object_QoS
     (BO  : access PolyORB.Binding_Objects.Binding_Object'Class)
      return PolyORB.QoS.QoS_Parameters
   is
      Note : Binding_Object_QoS_Note;

   begin
      PolyORB.Annotations.Get_Note
        (PolyORB.Binding_Objects.Notepad_Of
         (PolyORB.Binding_Objects.Binding_Object_Access (BO)).all,
         Note,
         Empty_Binding_Object_QoS_Note);

      return Note.QoS;
   end Get_Binding_Object_QoS;

   -------------------
   -- Is_Compatible --
   -------------------

   function Is_Compatible
     (BO  : access PolyORB.Binding_Objects.Binding_Object'Class;
      QoS : PolyORB.QoS.QoS_Parameters) return Boolean
   is
      BO_QoS : constant PolyORB.QoS.QoS_Parameters
        := Get_Binding_Object_QoS (BO);

   begin
      for J in PolyORB.QoS.QoS_Kind loop
         if Registry (J) /= null then
            if not Registry (J) (BO_QoS (J), QoS (J)) then
               return False;
            end if;
         end if;
      end loop;

      return True;
   end Is_Compatible;

   --------------
   -- Register --
   --------------

   procedure Register
     (Kind : PolyORB.QoS.QoS_Kind;
      Proc : QoS_Compatibility_Check_Proc)
   is
   begin
      Registry (Kind) := Proc;
   end Register;

   ----------------------------
   -- Set_Binding_Object_QoS --
   ----------------------------

   procedure Set_Binding_Object_QoS
     (BO  : access PolyORB.Binding_Objects.Binding_Object'Class;
      QoS :        PolyORB.QoS.QoS_Parameters)
   is
      Note : Binding_Object_QoS_Note;

   begin
      Note.QoS := QoS;
      PolyORB.Annotations.Set_Note
        (PolyORB.Binding_Objects.Notepad_Of
         (PolyORB.Binding_Objects.Binding_Object_Access (BO)).all,
         Note);
   end Set_Binding_Object_QoS;

   procedure Set_Binding_Object_QoS
     (BO   : access PolyORB.Binding_Objects.Binding_Object'Class;
      Kind :        PolyORB.QoS.QoS_Kind;
      QoS  :        PolyORB.QoS.QoS_Parameter_Access)
   is
      Note : Binding_Object_QoS_Note;

   begin
      PolyORB.Annotations.Get_Note
        (PolyORB.Binding_Objects.Notepad_Of
         (PolyORB.Binding_Objects.Binding_Object_Access (BO)).all,
         Note,
         Empty_Binding_Object_QoS_Note);

      Note.QoS (Kind) := QoS;

      PolyORB.Annotations.Set_Note
        (PolyORB.Binding_Objects.Notepad_Of
         (PolyORB.Binding_Objects.Binding_Object_Access (BO)).all,
         Note);
   end Set_Binding_Object_QoS;

end PolyORB.Binding_Object_QoS;
