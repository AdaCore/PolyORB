with Broca.Debug;

package body Broca.Exceptions.Stack is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.exceptions.stack");
   procedure O is new Broca.Debug.Output (Flag);

   ---------------------
   -- Raise_Exception --
   ---------------------
   procedure Raise_Exception (Excp : in Ada.Exceptions.Exception_Id;
                              Excp_Memb : in IDL_Exception_Members'Class) is
      ID : Exception_Occurrence_ID;
      Ex_Mb : IDL_Exception_Members_Ptr
        := new IDL_Exception_Members'Class' (Excp_Memb);
   begin
      The_Stack.Get_Next_Id (ID);
      The_Stack.Put (Ex_Mb, ID);
      Ada.Exceptions.Raise_Exception (Excp,
                                      Exception_Occurrence_ID'Image (ID));
      --  should never reach this point
      raise Program_Error;
   end Raise_Exception;

   -----------------
   -- Get_Members --
   -----------------
   procedure Get_Members (From : in CORBA.Exception_Occurrence;
                          To : out IDL_Exception_Members'Class) is
   begin
      The_Stack.Get (From, To);
   end Get_Members;



   --------------------
   -- The_Stack body --
   --------------------

   protected body The_Stack is

      --------------
      --  Is_Full --
      --------------
      function Is_Full return Boolean is
      begin
         return (Value.Youngest = Value.Oldest + 1);
      end Is_Full;


      ----------
      --  Put --
      ----------
      procedure Put (Excp_Mb : in IDL_Exception_Members_Ptr;
                     Excp_Id : in Exception_Occurrence_ID) is
      begin
         pragma Debug (O ("Put start, ID="
                          & Exception_Occurrence_ID'Image (Excp_Id)
                          & ", Youngest="
                          & Index_Type'Image (Value.Youngest)
                          & ", Oldest="
                          & Index_Type'Image (Value.Oldest)
                          & ", Is_Empty="
                          & Boolean'Image (Value.Is_Empty)));

         if Value.Is_Empty then
            Value.Is_Empty := False;
         else
            if (Is_Full) then
               --  free previous member
               Free (Value.Cells (Value.Oldest).Member_Ptr);
               Value.Youngest := Value.Youngest - 1;
               Value.Oldest := Value.Oldest - 1;
            else
               Value.Youngest := Value.Youngest - 1;
            end if;
         end if;

         --  insert the new member at the youngest position
         Value.Cells (Value.Youngest).ID := Excp_Id;
         Value.Cells (Value.Youngest).Member_Ptr := Excp_Mb;

         pragma Debug (O ("Put end, ID="
                          & Exception_Occurrence_ID'Image (Excp_Id)
                          & ", Youngest="
                          & Index_Type'Image (Value.Youngest)
                          & ", Oldest="
                          & Index_Type'Image (Value.Oldest)
                          & ", Is_Empty="
                          & Boolean'Image (Value.Is_Empty)));
      end Put;


      -----------
      --  Get  --
      -----------
      procedure Get (From : in CORBA.Exception_Occurrence;
                     Result : out IDL_Exception_Members'Class) is
         I : Index_Type := Value.Youngest;
         Found : Boolean := False;
         Expected_ID : Exception_Occurrence_ID
           := Exception_Occurrence_ID'Value
           (Ada.Exceptions.Exception_Message (From));
         J : Index_Type := Value.Youngest;
      begin
         pragma Debug (O ("Get start, ID="
                          & Exception_Occurrence_ID'Image (Expected_ID)
                          & ", Youngest="
                          & Index_Type'Image (Value.Youngest)
                          & ", Oldest="
                          & Index_Type'Image (Value.Oldest)
                          & ", Is_Empty="
                          & Boolean'Image (Value.Is_Empty)));

         if Value.Is_Empty then
            Broca.Exceptions.Raise_Imp_Limit;
         end if;

         --  loop to find the expected ID
         loop
            if Value.Cells (I).ID = Expected_ID then
               Found := True;
            else
               I := I + 1;
            end if;
            exit when ((Found) or (I = Value.Youngest));
         end loop;

         if (not Found) then
            Broca.Exceptions.Raise_Imp_Limit;
         end if;

         --  found : return this cell
         Result := Value.Cells (I).Member_Ptr.all;

         --  and free all the previous ones
         loop
            Free (Value.Cells (J).Member_Ptr);
            J := J + 1;
            exit when (J = I + 1);
         end loop;

         --  shift the youngest index to remove the bottom of the stack
         if (I = Value.Oldest) then
            --  remove everything
            Value.Youngest := Value.Oldest;
            Value.Is_Empty := True;
         else
            Value.Youngest := I + 1;
         end if;

         pragma Debug (O ("Get ends, ID="
                          & Exception_Occurrence_ID'Image (Expected_ID)
                          & ", Youngest="
                          & Index_Type'Image (Value.Youngest)
                          & ", Oldest="
                          & Index_Type'Image (Value.Oldest)
                          & ", Is_Empty="
                          & Boolean'Image (Value.Is_Empty)));

      end Get;


      ------------------
      --  Get_Next_Id --
      ------------------
      procedure Get_Next_Id (Result : out Exception_Occurrence_ID) is
      begin
         Next_Id := Next_Id + 1;
         Result := Next_Id;
      end Get_Next_Id;

   end The_Stack;

end Broca.Exceptions.Stack;
