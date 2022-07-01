with Ada.Text_IO; use ada.Text_IO;

with System;

with Interfaces.C;

with Epoll;


procedure Epoll_Example is

   use type Interfaces.C.int;

   package C renames Interfaces.C;

   Standard_In : constant C.int := 1;

   Max_Events : constant C.int := 10;

   Epoll_FD : constant C.int := Epoll.Epoll_Create1 (Flags => 0);

   --------------------------
   --  Consume_Standard_In --
   --------------------------

   procedure Consume_Standard_In is
      User_Input : constant String := Get_Line;
   begin
      Put_Line ("You entered: """ & User_Input & """");
   end Consume_Standard_In;

   Event : Epoll.Struct_Epoll_Event :=
     (Events => Epoll.Event_Kind_Values (Epoll.EK_Read),
      Data   => Integer (Standard_In));

   Events : array (1 .. Max_Events) of Epoll.Struct_Epoll_Event;

   Ready_Count : C.int;

begin
   if Epoll_FD = -1 then
      Put_Line ("Failed to create epoll instance.");
      return;
   end if;

   Put_Line ("Adding the file descriptor"
     & Integer'Image (Event.Data)
     & " to watch list.");

   if Epoll.Epoll_Ctl
     (EpFd  => Epoll_FD,
      Op    => Epoll.Add'Enum_Rep,
      Fd    => Standard_In,
      Event => Event'Address) = -1
   then
      Put_line ("Failed to add descriptor to epoll instance.");
      return;
   end if;

   Forever : loop
      Ready_Count := Epoll.Epoll_Wait (Epoll_FD, Events'Address, 10, 1000);
      exit when Ready_Count = -1;
      Put_Line (C.int'Image (Ready_Count) & " event(s) ready.");

      for Index in Events'First .. Ready_Count loop
         Put_Line ("Structure contained descriptor"
           & Integer'Image (Events (Index).Data));

         if Events (Index).Data = Integer (Standard_In) then
            Consume_Standard_In;
            Put_Line ("Consumed standard input.");
         end if;
      end loop;

   end loop Forever;

   Put_Line ("Failed to wait for events on epoll instance.");
end Epoll_Example;
