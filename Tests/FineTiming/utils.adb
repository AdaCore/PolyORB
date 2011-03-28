with Ada.Exceptions;      use Ada.Exceptions;
with System.Garlic.TCP;
pragma Warnings (Off, System.Garlic.TCP);
pragma Elaborate_All (System.Garlic.TCP);
with System.Garlic.Utils; use System.Garlic.Utils;

package body Utils is

   use Ada.Streams, Interfaces.C;

   procedure Physical_Receive
     (Peer  : in int;
      Data  : access Stream_Element_Array;
      Error : in out Error_Type);
   pragma Import (Ada, Physical_Receive, "GLADE_Physical_Receive");

   procedure Physical_Send
     (Peer   : in int;
      Stream : access Stream_Element_Array;
      First  : in Stream_Element_Count;
      Error  : in out Error_Type);
   pragma Import (Ada, Physical_Send, "GLADE_Physical_Send");

   -------------
   -- Receive --
   -------------

   procedure Receive
     (Sock : in int;
      Data : access Stream_Element_Array)
   is
      Error : Error_Type;
   begin
      Physical_Receive (Sock, Data, Error);
      if Found (Error) then
         Raise_Exception (Program_Error'Identity,
                          "Could not receive expected data");
      end if;
   end Receive;

   ----------
   -- Send --
   ----------

   procedure Send
     (Sock : in int;
      Data : access Stream_Element_Array)
   is
      Error : Error_Type;
   begin
      Physical_Send (Sock, Data, Data'First, Error);
      if Found (Error) then
         Raise_Exception (Program_Error'Identity,
                          "Could not send whole data");
      end if;
   end Send;

end Utils;
