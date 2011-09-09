------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     C O S T I M E . U T O . I M P L                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2010, Free Software Foundation, Inc.          --
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

with Time_Utils;

with PolyORB.CORBA_P.Server_Tools;

with CosTime.TIO.Impl;

with CosTime.UTO.Skel;
pragma Warnings (Off, CosTime.UTO.Skel);

package body CosTime.UTO.Impl is

   use TimeBase;
   use Time_Utils;

   type UTO_Ptr is access Object;
   type TIO_Ptr is access CosTime.TIO.Impl.Object;

   -------------------
   -- absolute_time --
   -------------------

   function absolute_time
     (Self : access Object)
     return Ref'Class
   is
      Result : constant UTO_Ptr := new Object;
      R      : Ref;
   begin
      Result.Time := Self.Time + Current_Time;
      Result.Inaccuracy := Self.Inaccuracy + Current_Inaccuracy;
      Result.Tdf := Self.Tdf + Current_Tdf;
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Result), R);
      return R;
   end absolute_time;

   ------------------
   -- compare_time --
   ------------------

   function compare_time
     (Self            : access Object;
      comparison_type : ComparisonType;
      uto             : Ref)
     return TimeComparison
   is
      Other_Time : constant TimeT := Get_time (uto);
      Other_Tdf  : constant TdfT  := Get_tdf (uto);
   begin
      if comparison_type = MidC then
         return Compare (Self.Time + Self.Tdf, Other_Time + Other_Tdf);
      else
         declare
            Other_Inaccuracy : constant InaccuracyT := Get_inaccuracy (uto);
            Comp_Low         : constant TimeComparison :=
              Compare (Self.Time - Self.Inaccuracy + Self.Tdf,
                       Other_Time - Other_Inaccuracy + Other_Tdf);
            Comp_High        : constant TimeComparison :=
              Compare (Self.Time + Self.Inaccuracy + Self.Tdf,
                       Other_Time + Other_Inaccuracy + Other_Tdf);
         begin
            if Comp_Low = Comp_High then
               return Comp_Low;
            else
               return TCIndeterminate;
            end if;
         end;
      end if;
   end compare_time;

   --------------------
   -- get_inaccuracy --
   --------------------

   function get_inaccuracy
     (Self : access Object)
     return InaccuracyT
   is
   begin
      return Self.Inaccuracy;
   end get_inaccuracy;

   -------------
   -- get_tdf --
   -------------

   function get_tdf
     (Self : access Object)
     return TdfT
   is
   begin
      return Self.Tdf;
   end get_tdf;

   --------------
   -- get_time --
   --------------

   function get_time
     (Self : access Object)
     return TimeT
   is
   begin
      return Self.Time;
   end get_time;

   ------------------
   -- get_utc_time --
   ------------------

   function get_utc_time
     (Self : access Object)
     return UtcT
   is
   begin
      return (time    => Self.Time,
              inacclo => CORBA.Unsigned_Long (Self.Inaccuracy rem (2 ** 32)),
              inacchi => CORBA.Unsigned_Short (Self.Inaccuracy / (2 ** 32)),
              tdf     => Self.Tdf);
   end get_utc_time;

   --------------
   -- interval --
   --------------

   function interval
     (Self : access Object)
     return TIO_Forward.Ref
   is
      Result : constant TIO_Ptr := new CosTime.TIO.Impl.Object;
      R      : TIO_Forward.Ref;
   begin
      Result.Interval.lower_bound := Self.Time - Self.Tdf;
      Result.Interval.upper_bound := Self.Time + Self.Tdf;
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Result), R);
      return R;
   end interval;

   ----------------------
   -- time_to_interval --
   ----------------------

   function time_to_interval
     (Self : access Object;
      uto  : Ref)
     return TIO_Forward.Ref
   is
      Other_Time : constant TimeT   := Get_time (uto);
      Result     : constant TIO_Ptr := new CosTime.TIO.Impl.Object;
      R          : TIO_Forward.Ref;
   begin
      Result.Interval.lower_bound := TimeT'Min (Self.Time, Other_Time);
      Result.Interval.upper_bound := TimeT'Max (Self.Time, Other_Time);
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Result), R);
      return R;
   end time_to_interval;

end CosTime.UTO.Impl;
