with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with System.Garlic.Thin;    use System.Garlic.Thin;


package body System.Garlic.Sockets.Selectors is

   use type C.int;

   type Socket_Set_Record is mod 2 ** 32;

   procedure Free is
      new Ada.Unchecked_Deallocation
     (Socket_Set_Record, Socket_Set_Type);

   function To_Fd_Set is
      new Ada.Unchecked_Conversion
        (Socket_Set_Record, Fd_Set);

   function To_Socket_Record is
      new Ada.Unchecked_Conversion
        (Fd_Set, Socket_Set_Record);

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Set    : in out Socket_Set_Type;
      Socket : in Socket_Type)
   is
      Mask : constant Socket_Set_Record := 2 ** Natural (Socket);
   begin
      if Set = null then
         Set := new Socket_Set_Record'(0);
      elsif (Set.all and Mask) /= 0 then
         Set.all := Set.all xor Mask;
      end if;
   end Clear;

   ---------
   -- Set --
   ---------

   procedure Set   (Set : in out Socket_Set_Type; Socket : in Socket_Type) is
   begin
      if Set = null then
         Set := new Socket_Set_Record'(0);
      end if;
      Set.all := Set.all or 2 ** Natural (Socket);
   end Set;

   ----------
   -- Zero --
   ----------

   procedure Zero  (Set : in out Socket_Set_Type) is
   begin
      if Set /= null then
         Free (Set);
      end if;
   end Zero;

   -----------
   -- Empty --
   -----------

   function Empty
     (Set : Socket_Set_Type) return Boolean is
   begin
      return Set = null or else Set.all = 0;
   end Empty;

   ------------
   -- Is_Set --
   ------------

   function Is_Set
     (Set    : Socket_Set_Type;
      Socket : Socket_Type) return Boolean is
   begin
      return Set /= null
        and then (Set.all and 2 ** Natural (Socket)) /= 0;
   end Is_Set;

   ---------------------
   -- Create_Selector --
   ---------------------

   function Create_Selector return Selector_Access
   is
      Selector : Selector_Access := new Selector_Type;
      Sockets  : aliased Two_Int;
      Result   : C.int;
      Protocol : C.int := 0; --  ???

   begin
      Result :=
        C_Socketpair
        (Constants.Af_Inet,
         Constants.Sock_Stream,
         Protocol,
         Sockets'Access);

      if Result = Failure then
         raise Constraint_Error;
      end if;

      Selector.R_Sig_Socket := Socket_Type (Sockets (Sockets'First));
      Selector.W_Sig_Socket := Socket_Type (Sockets (Sockets'Last));
      return Selector;
   end Create_Selector;

   -------------------
   -- Select_Socket --
   -------------------

   procedure Select_Socket
     (Selector     : access Selector_Type;
      R_Socket_Set : in out Socket_Set_Type;
      W_Socket_Set : in out Socket_Set_Type;
      Status       : out Selector_Status;
      Timeout      : in Microseconds := Forever)
   is
      Ready  : C.int;
      Length : C.int;
      ASet   : Socket_Set_Record;
      RSet   : aliased Socket_Set_Record;
      WSet   : aliased Socket_Set_Record;
      TVal   : aliased Timeval := (0, Timeval_Unit (Timeout));
      TPtr   : Timeval_Access;

   begin
      Status := Completed;

      if Timeout = Forever then
         TPtr := null;
      else
         TPtr := TVal'Unchecked_Access;
      end if;

      if R_Socket_Set = null then
         RSet := 0;
      else
         RSet := R_Socket_Set.all;
      end if;
      RSet := RSet and 2 ** Natural (Selector.R_Sig_Socket);
      ASet := RSet;

      if W_Socket_Set = null then
         WSet := 0;
      else
         WSet := W_Socket_Set.all;
         if WSet > ASet then
            ASet := WSet;
         end if;
      end if;

      if ASet = 0 then
         raise Socket_Error;
      end if;

      Length := 0;
      while ASet /= 0 loop
         Length := Length + 1;
         ASet   := ASet / 2;
      end loop;

      Selector.In_Progress := True;

      --  Temporary code - should fix thin

      declare
         RS : aliased Fd_Set := To_Fd_Set (RSet);
         WS : aliased Fd_Set := To_Fd_Set (WSet);
      begin
         Ready := C_Select
           (Length,
            RS'Unchecked_Access,
            WS'Unchecked_Access,
            null, TPtr);
         RSet := To_Socket_Record (RS);
         WSet := To_Socket_Record (WS);
      end;

      Selector.In_Progress := False;

      if Is_Set (RSet'Unchecked_Access, Selector.R_Sig_Socket) then
         RSet := RSet xor 2 ** Natural (Selector.R_Sig_Socket);
         declare
            Dummy : Ada.Streams.Stream_Element_Array (0 .. 0);
            Last  : Ada.Streams.Stream_Element_Offset;
         begin
            Receive_Socket (Selector.R_Sig_Socket, Dummy, Last);
         end;

         Status := Aborted;
      elsif Ready = 0 then
         Status := Expired;
      end if;

      if R_Socket_Set /= null then
         R_Socket_Set.all := Socket_Set_Record (RSet);
      end if;

      if W_Socket_Set /= null then
         W_Socket_Set.all := Socket_Set_Record (WSet);
      end if;
   end Select_Socket;

   ------------------
   -- Select_Abort --
   ------------------

   procedure Abort_Select (Selector : access Selector_Type)
   is
      Dummy : Ada.Streams.Stream_Element_Array (0 .. 0) := (others => 0);
      Last  : Ada.Streams.Stream_Element_Offset;

   begin
      if Selector.In_Progress then
         --  Is it supposed to be an empty array ???
         Send_Socket (Selector.W_Sig_Socket, Dummy, Last);
      end if;
   end Abort_Select;

end System.Garlic.Sockets.Selectors;
