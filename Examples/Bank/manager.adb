with Bank; use Bank;
with Types; use Types;
with Text_IO; use Text_IO;

procedure Manager is

   function Get_Password return Password_Type;
   function Get_Customer return Customer_Type;
   function Get_Deposit  return Integer;

   function Get_Customer return Customer_Type is
      C : Customer_Type (1 .. 16);
      L : Natural;
   begin
      Put ("Customer : ");
      Get_Line (String (C), L);
      return C (1 .. L);
   end Get_Customer;

   function Get_Password return Password_Type is
      P : Password_Type (1 .. 8);
      L : Natural;
   begin
      Put ("Password : ");
      Get_Line (String (P), L);
      return P (1 .. L);
   end Get_Password;

   function Get_Deposit return Integer is
      D : String (1 .. 8);
      L : Natural;
   begin
      Put ("Deposit  : ");
      Get_Line (String (D), L);
      return Integer'Value (D (1 .. L));
   end Get_Deposit;

   Request  : String (1 .. 16);
   Length   : Natural;
   Shortcut : Character;

begin

   loop

      New_Line;
      Put_Line ("Menu :");
      New_Line;
      Put_Line ("   Create");
      Put_Line ("   Load");
      Put_Line ("   Print");
      Put_Line ("   Quit");
      Put_Line ("   Save");
      New_Line;

      Put ("Request : ");
      Get_Line (Request, Length);
      New_Line;

      if Length > 0 then
         Shortcut := Request (1);

         exit when Shortcut = 'Q' or Shortcut = 'q';
         case Shortcut is
            when 'P' | 'p' =>
               Put_Line ("=> Print");
               New_Line;
               for ID in Customer_ID (1) .. Customer_ID (N_Customer_IDs) loop
                  if Is_Activated (ID) then
                     Put ("   Customer : ");
                     Put_Line (String (Get_Customer (ID)));
                     Put ("   Password : ");
                     Put_Line (String (Get_Password (ID)));
                     Put ("   Balance  : ");
                     Put_Line (Integer'Image (Get_Balance (ID)));
                     New_Line;
                  end if;
               end loop;

            when 'S' | 's' =>
               Put_Line ("=> Save");
               New_Line;
               declare
                  Name : String (1 .. 11);
                  Last : Natural;
                  File : File_Type;
               begin
                  loop
                     Put ("Filename : ");
                     Get_Line (Name, Last);
                     if Last > 0 then
                        begin
                           Create (File, Out_File, Name (1 .. Last));
                           exit;
                        exception when others =>
                           Put_Line ("File not created");
                        end;
                     end if;
                  end loop;
                  for ID in Customer_ID (1) ..
                            Customer_ID (N_Customer_IDs) loop
                     if Is_Activated (ID) then
                        Put_Line (File, String (Get_Customer (ID)));
                        Put_Line (File, String (Get_Password (ID)));
                        Put_Line (File, Integer'Image (Get_Balance (ID)));
                        Put_Line (File, "--");
                    end if;
                  end loop;
                  Close (File);
               end;

            when 'C' | 'c' =>
               Put_Line ("=> Create");
               begin
                  Create (Get_Customer, Get_Password, Get_Deposit);
               exception
                  when Wrong_Customer =>
                     Put_Line ("Customer already exists");
                  when Wrong_Password =>
                     Put_Line ("Illegal password");
               end;

            when 'L' | 'l' =>
               Put_Line ("=> Load");
               declare
                  S1, S2, S3 : String (1 .. 16);
                  L1, L2, L3 : Natural;

                  Name : String (1 .. 11);
                  Last : Natural;
                  File : File_Type;
               begin
                  loop
                     Put ("Filename : ");
                     Get_Line (Name, Last);
                     if Last > 0 then
                        begin
                           Open (File, In_File, Name (1 .. Last));
                           exit;
                        exception when others =>
                           Put_Line ("File doesn't exist");
                        end;
                     end if;
                  end loop;
                  while not End_Of_File (File) loop
                     Get_Line (File, S1, L1);
                     Get_Line (File, S2, L2);
                     Get_Line (File, S3, L3);
                     Create
                       (Customer_Type (S1 (1 .. L1)),
                        Password_Type (S2 (1 .. L2)),
                        Integer'Value (S3 (1 .. L3)));
                     Get_Line (File, S1, L1); -- Comments
                  end loop;
                  Close (File);
               end;

            when others =>
               Put_Line ("Illegal operation");

         end case;

      end if;

   end loop;

end Manager;



