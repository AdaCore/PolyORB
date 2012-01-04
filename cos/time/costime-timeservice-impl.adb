------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             C O S T I M E . T I M E S E R V I C E . I M P L              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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
