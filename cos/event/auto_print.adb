with Ada.Exceptions; use Ada.Exceptions;

with CosEventComm.PushConsumer.Impl;
use CosEventComm.PushConsumer.Impl;

with CosEventComm.PushConsumer;

with PortableServer; use PortableServer;
pragma Warnings (Off, PortableServer);
with Ada.Text_IO;

with Ada.Exceptions; use Ada.Exceptions;

with CORBA;
use CORBA;
package body Auto_Print is


   ---------------------------
   -- Ensure_Initialization --
   ---------------------------

   procedure Ensure_Initialization is
   begin
      if T_Initialized then
         return;
      end if;
      Create (Session_Mutex);
      Create (Session_Taken);
      T_Initialized := True;
   end Ensure_Initialization;


   -------------------------
   -- Thread Auto_Display --
   -------------------------


   procedure  Auto_Display is
   begin
      declare
         B : CORBA.Boolean;
         A : CORBA.Any;
         Ptr : PushConsumer.Impl.Object_Ptr;
      begin
         Ada.Text_IO.Put ("début");
         Ensure_Initialization;
         Ptr := PushConsumer.Impl.Object_Ptr (A_S);
         --  A_S is a global variable used to pass an argument to this task
         --  Ptr is initialized
         --  we can let For_Consumers go
         Enter (Session_Mutex);
         Signal (Session_Taken);
         Leave (Session_Mutex);
         loop
            exit when EndDisplay = True;
            Try_Pull (Ptr, B, A);
            if B then
               Ada.Text_IO.Put_Line (
                    To_Standard_String (From_Any (A)));
            else
               Ada.Text_IO.Put ("");
            end if;
         end loop;
         EndDisplay := False;
      exception
         when E : others =>
            Ada.Text_IO.Put_Line ("raise "& Exception_Name (E));
            Ada.Text_IO.Put_Line (Exception_Message (E));
            Ada.Text_IO.Put_Line (Exception_Information (E));
      end;
   end Auto_Display;
end Auto_Print;
