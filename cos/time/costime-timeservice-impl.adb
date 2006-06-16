------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             C O S T I M E . T I M E S E R V I C E . I M P L              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2006, Free Software Foundation, Inc.          --
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

with PolyORB.CORBA_P.Server_Tools;

with Time_Utils;

with CosTime.TIO.Impl;
with CosTime.UTO.Impl;

with CosTime.TimeService.Skel;
pragma Warnings (Off, CosTime.TimeService.Skel);

package body CosTime.TimeService.Impl is

   use TimeBase;
   use Time_Utils;

   type UTO_Ptr is access UTO.Impl.Object;
   type TIO_Ptr is access TIO.Impl.Object;

   ------------------
   -- new_interval --
   ------------------

   function new_interval
     (Self  : access Object;
      lower : TimeBase.TimeT;
      upper : TimeBase.TimeT)
     return CosTime.TIO.Ref
   is
      pragma Unreferenced (Self);

      Result : constant TIO_Ptr := new TIO.Impl.Object;
      R      : CosTime.TIO.Ref;
   begin
      Result.Interval := (lower_bound => lower, upper_bound => upper);
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Result), R);
      return R;
   end new_interval;

   ------------------------
   -- new_universal_time --
   ------------------------

   function new_universal_time
     (Self       : access Object;
      time       : TimeBase.TimeT;
      inaccuracy : TimeBase.InaccuracyT;
      tdf        : TimeBase.TdfT)
     return CosTime.UTO.Ref
   is
      pragma Unreferenced (Self);

      Result : constant UTO_Ptr := new UTO.Impl.Object;
      R      : CosTime.UTO.Ref;
   begin
      Result.Time := time;
      Result.Inaccuracy := inaccuracy;
      Result.Tdf := tdf;
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Result), R);
      return R;
   end new_universal_time;

   ---------------------------
   -- secure_universal_time --
   ---------------------------

   function secure_universal_time
     (Self : access Object)
     return CosTime.UTO.Ref is
   begin
      raise TimeUnavailable;
      return universal_time (Self);
   end secure_universal_time;

   --------------------
   -- universal_time --
   --------------------

   function universal_time
     (Self : access Object)
     return CosTime.UTO.Ref is
   begin
      return new_universal_time
        (Self       => Self,
         time       => Current_Time,
         inaccuracy => Current_Inaccuracy,
         tdf        => Current_Tdf);
   end universal_time;

   ------------------
   -- uto_from_utc --
   ------------------

   function uto_from_utc
     (Self : access Object;
      utc : TimeBase.UtcT)
     return CosTime.UTO.Ref
   is
      use CORBA;
   begin
      return new_universal_time
        (Self       => Self,
         time       => utc.time,
         inaccuracy => InaccuracyT (utc.inacchi * 2 ** 32) +
                       InaccuracyT (utc.inacclo),
         tdf        => utc.tdf);
   end uto_from_utc;

end CosTime.TimeService.Impl;
