with MOMA.Destinations;
with Ada.Calendar; use Ada.Calendar;
with Temp; use Temp;

package MOMA.Messages is

   ------------------------------
   --  Abstract Message Object --
   ------------------------------
   type Message is abstract tagged private;

   ----------------------------
   --  Acknowledge Procedure --
   ----------------------------
   procedure Acknowledge;

   ---------------------------
   --  Clear_Body Procedure --
   ---------------------------
   procedure Clear_Body;

   ----------------------------
   --  Get_Property Function --
   ----------------------------
   function Get_Property (Name : String) return Property_Type;

   ----------------------------------
   --  Get_Correlation_Id Function --
   ----------------------------------
   function Get_Correlation_Id return String;

   ------------------------------
   --  Get_Persistent Function --
   ------------------------------
   function Get_Persistent return Boolean;

   -------------------------------
   --  Get_Destination Function --
   -------------------------------
   function Get_Destination
     return MOMA.Destinations.Destination is abstract;

   ------------------------------
   --  Get_Expiration Function --
   ------------------------------
   function Get_Expiration return Time;

   ------------------------------
   --  Get_Message_Id Function --
   ------------------------------
   function Get_Message_Id return String;

   ----------------------------
   --  Get_Priority Function --
   ----------------------------
   function Get_Priority return Priority;

   -------------------------------
   --  Get_Redelivered Function --
   -------------------------------
   function Get_Redelivered return Boolean;

   ----------------------------
   --  Get_Reply_To Function --
   ----------------------------
   function Get_Reply_To
     return MOMA.Destinations.Destination is abstract;

   -----------------------------
   --  Get_Timestamp Function --
   -----------------------------
   function Get_Timestamp return Time;

   ------------------------
   --  Get_Type Function --
   ------------------------
   function Get_Type return String;

   ----------------------------------
   --  Get_Property_Names Function --
   ----------------------------------
   --  ??? return
   function Get_Property_Names
     return Integer;

   -------------------------------
   --  Property_Exists Function --
   -------------------------------
   function Property_Exists (Name : String) return Boolean;

   -----------------------------
   --  Set_Property Procedure --
   -----------------------------
   procedure Set_Property (Name : String; Value : Property_Type);

   -----------------------------------
   --  Set_Correlation_Id Procedure --
   -----------------------------------
   procedure Set_Correlation_Id (Correlation_Id : String);

   -------------------------------
   --  Set_Persistent Procedure --
   -------------------------------
   procedure Set_Persistent (Is_Persistent : Boolean);

   --------------------------------
   --  Set_Destination Procedure --
   --------------------------------
   procedure Set_Destination (Destination : MOMA.Destinations.Destination);

   -------------------------------
   --  Set_Expiration Procedure --
   -------------------------------
   procedure Set_Expiration (Expiration : Time);

   -------------------------------
   --  Set_Message_Id Procedure --
   -------------------------------
   procedure Set_Message_Id (Id : String);

   -----------------------------
   --  Set_Priority Procedure --
   -----------------------------
   procedure Set_Priority (Value : Priority);

   ---------------------------------
   --  Set_Redellivered Procedure --
   ---------------------------------
   procedure Set_Redelivered (Redelivered : Boolean);

   -----------------------------
   --  Set_Reply_To Procedure --
   -----------------------------
   procedure Set_Reply_To (Reply_To : MOMA.Destinations.Destination);

   ------------------------------
   --  Set_Timestamp Procedure --
   ------------------------------
   procedure Set_Timestamp (Timestamp : Time);


   -- Abstract Functions and Procedures --

   ----------------------------------
   --  Abstract Set_Type Procedure --
   ----------------------------------
   procedure Set_Type (Message_Type : String) is abstract;

private
   type Message is abstract tagged null record;

end MOMA.Messages;
