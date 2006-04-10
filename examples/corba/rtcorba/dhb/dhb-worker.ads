-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.adacore.com/polyorb/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
-------------------------------------------------
pragma Style_Checks ("NM32766");

with RTCORBA;
with CORBA.Sequences.Unbounded;
pragma Elaborate_All (CORBA.Sequences.Unbounded);
with CORBA;
pragma Elaborate_All (CORBA);
with CORBA.Object;

package DHB.Worker is

   type Ref is new CORBA.Object.Ref with null record;

   package IDL_SEQUENCE_unsigned_long is
     new CORBA.Sequences.Unbounded
       (CORBA.Unsigned_Long);

   type U_sequence is
  new DHB.Worker.IDL_SEQUENCE_unsigned_long.Sequence;

   U_sequence_Repository_Id : constant Standard.String
     := "IDL:DHB/Worker/U_sequence:1.0";

   procedure Do_Some_Work
     (Self : in Ref;
      Kilo_Whetstone : in DHB.KWIPS);

   Do_Some_Work_Repository_Id : constant Standard.String
     := "IDL:DHB/Worker/Do_Some_Work:1.0";

   procedure Do_Some_Work_With_Payload
     (Self : in Ref;
      Kilo_Whetstone : in DHB.KWIPS;
      Payload : in DHB.Worker.U_sequence);

   Do_Some_Work_With_Payload_Repository_Id : constant Standard.String
     := "IDL:DHB/Worker/Do_Some_Work_With_Payload:1.0";

   function Get_KWIPS
     (Self : in Ref)
     return DHB.KWIPS;

   Get_KWIPS_Repository_Id : constant Standard.String
     := "IDL:DHB/Worker/Get_KWIPS:1.0";

   function Running_Priority
     (Self : in Ref)
     return RTCORBA.Priority;

   Running_Priority_Repository_Id : constant Standard.String
     := "IDL:DHB/Worker/Running_Priority:1.0";

   function Round_Trip
     (Self : in Ref;
      data : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long;

   Round_Trip_Repository_Id : constant Standard.String
     := "IDL:DHB/Worker/Round_Trip:1.0";

   function Round_Trip_With_Payload
     (Self : in Ref;
      Payload : in DHB.Worker.U_sequence)
     return DHB.Worker.U_sequence;

   Round_Trip_With_Payload_Repository_Id : constant Standard.String
     := "IDL:DHB/Worker/Round_Trip_With_Payload:1.0";

   procedure Ping
     (Self : in Ref;
      data : in CORBA.Unsigned_Long);

   Ping_Repository_Id : constant Standard.String
     := "IDL:DHB/Worker/Ping:1.0";

   Repository_Id : constant Standard.String
     := "IDL:DHB/Worker:1.0";

   function Is_A
     (Self : Ref;
      Logical_Type_Id : Standard.String)
     return CORBA.Boolean;

private

   function Is_A
     (Logical_Type_Id : Standard.String)
     return CORBA.Boolean;

end DHB.Worker;
