with Ada.Text_IO;    use ada.Text_IO;
with Ada.Exceptions;

with Epoll;


procedure Epoll_Example is

   use type Epoll.Event_Kind_Type;

   Standard_In : constant Integer := 1;

   -------------------
   -- Abortion_Task --
   -------------------

   task Abortion_Task is
      entry Start (Epoll_Acc : in Epoll.Epoll_Access);
   end Abortion_Task;

   task body Abortion_Task is
      Ep : Epoll.Epoll_Access;

   begin
      accept Start (Epoll_Acc : in Epoll.Epoll_Access) do
         Ep := Epoll_Acc;
      end Start;

      Forever : loop
         Put_Line ("Aborting call to `Wait` in 5 seconds...");
         delay 5.0;
         Epoll.Abort_Wait (Ep.all);
      end loop Forever;

   exception
      when Error : others =>
        Put_Line (Ada.Exceptions.Exception_Information (Error));
   end Abortion_Task;

   --------------------------
   --  Consume_Standard_In --
   --------------------------

   procedure Consume_Standard_In is
      User_Input : constant String := Get_Line;
   begin
      Put_Line ("You entered: """ & User_Input & """");
   end Consume_Standard_In;

   Ep : Epoll.Epoll_Type;

begin
   Epoll.Create (Ep, Close_On_Exec => True);

   Epoll.Add
     (Epoll      => Ep,
      Descriptor => Standard_In,
      Event_Mask => Epoll.EK_Read + EPoll.EK_Error + Epoll.EK_Closed,
      Meta_Data  => Standard_In);

   Abortion_Task.Start (Epoll_Acc => Ep'Unrestricted_Access);

   Forever : loop
      Put_Line ("Waiting for events...");

      declare
         Events : constant Epoll.Event_Array := Epoll.Wait
           (Ep, Timeout => Epoll.Forever);

      begin
         Put_Line (Integer'Image (Events'Length) & " event(s) ready.");

         for Index in Events'Range loop
            Put_Line ("Structure contained descriptor"
              & Integer'Image (Epoll.Get_Meta_Data (Events (Index))));

            if Epoll.Get_Meta_Data  (Events (Index)) = Standard_In and then
               Epoll.Get_Event_Mask (Events (Index)) & Epoll.EK_Read
            then
               Consume_Standard_In;
               Put_Line ("Consumed standard input.");
            end if;
         end loop;

      end;
   end loop Forever;

exception
   when Error : others =>
     Put_Line (Ada.Exceptions.Exception_Information (Error));
     Epoll.Close (Ep);

end Epoll_Example;
