--with Ada.Text_IO; use Ada.Text_IO;

with System;

with GNAT.OS_Lib;


package body Epoll is

   ----------------------------------------------
   -- Start of private (body-only) subprograms --
   ----------------------------------------------

   ------------------------
   -- Form_Error_Message --
   ------------------------

   function Form_Error_Message return String
   is (Integer'Image (GNAT.OS_Lib.Errno) & ": "
         & GNAT.OS_Lib.Errno_Message (GNAT.OS_Lib.Errno, "Unknown."));

   ---------------------
   -- Private_Control --
   ---------------------

   procedure Private_Control
     (Epoll      : in out Epoll_Type;
      Operation  : in     Operation_Kind;
      Descriptor : in     Integer;
      Event_Mask : in     Event_Kind_Type;
      Meta_Data  : in     Integer)
   is
      -----------------
      -- C_Epoll_Ctl --
      -----------------

      function C_Epoll_Ctl
        (EpFd  : in C.int;
         Op    : in C.int;
         Fd    : in C.int;
         Event : in System.Address) return C.int
      with
        Import     => True,
        Convention => C,
        Link_Name  => "epoll_ctl";

      Event : constant Event_Type := (Event_Mask, Meta_Data);

   begin
      if C_Epoll_Ctl
        (EpFd  => Epoll.Descriptor,
         Op    => Operation'Enum_Rep,
         Fd    => C.int (Descriptor),
         Event => Event'Address) = -1
      then
         raise Epoll_Error with Form_Error_Message;
      end if;

      case Operation is
         when Add    => Epoll.Interest_Count := Epoll.Interest_Count + 1;
         when Delete => Epoll.Interest_Count := Epoll.Interest_Count - 1;
         when Modify => null;
      end case;
   end Private_Control;

   ----------------------------------
   -- Start of public subprograms. --
   ----------------------------------

   ------------
   -- Create --
   ------------

   procedure Create
     (Epoll         : in out Epoll_Type;
      Close_On_Exec : in     Boolean := False)
   is
      EPOLL_CLOEXEC : constant C.int := 8#2000000#;

      ---------------------
      -- C_Epoll_Create1 --
      ---------------------

      function C_Epoll_Create1 (Flags : in C.int) return C.int with
        Import     => True,
        Convention => C,
        Link_Name  => "epoll_create1";

   begin
      if Epoll.Descriptor /= -1 then
         Close (Epoll);
      end if;

      Epoll.Descriptor := C_Epoll_Create1
        ((if Close_On_Exec then EPOLL_CLOEXEC else 0));

      if Epoll.Descriptor = -1 then
         raise Epoll_Error with Form_Error_Message;
      end if;

      GNAT.Sockets.Create_Socket_Pair
        (Left   => Epoll.Read_Sock,
         Right  => Epoll.Write_Sock,
         Family => GNAT.Sockets.Family_Unix,
         Mode   => GNAT.Sockets.Socket_Raw,
         Level  => GNAT.Sockets.Socket_Level);
      --  ! Catch exception and turn into our custom epoll-related one?

      Private_Control
        (Epoll      => Epoll,
         Operation  => Add,
         Descriptor => GNAT.Sockets.To_C (Epoll.Read_Sock),
         Event_Mask => EK_Read,
         Meta_Data  => Read_Sock_Unique_ID);

   end Create;

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : in Event_Kind_Type) return Event_Kind_Type
   is (Left or Right);

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : in Event_Kind_Type) return Boolean
   is ((Left and Right) = (Right and Right));

   -------------
   -- Control --
   -------------

   procedure Control
     (Epoll      : in out Epoll_Type;
      Operation  : in     Operation_Kind;
      Descriptor : in     Integer;
      Event_Mask : in     Event_Kind_Type;
      Meta_Data  : in     Meta_Data_Range)
   is
   begin
      Private_Control (Epoll, Operation, Descriptor, Event_Mask, Meta_Data);
   end Control;

   -------------------
   -- Get_Meta_Data --
   -------------------

   function Get_Meta_Data (Event : in Event_Type) return Meta_Data_Range
   is (Event.Meta_Data);

   --------------------
   -- Get_Event_Mask --
   --------------------

   function Get_Event_Mask (Event : in Event_Type) return Event_Kind_Type
   is (Event.Event_Mask);

   ----------
   -- Wait --
   ----------

   function Wait
     (Epoll   : in out Epoll_Type;
      Timeout : in     Milliseconds) return Event_Array
   is
      ------------------
      -- C_Epoll_Wait --
      ------------------

      function C_Epoll_Wait
        (EpFd      : in C.int;
         Events    : in System.Address;
         MaxEvents : in C.int;
         Timeout   : in C.int) return C.int
      with
        Import     => True,
        Convention => C,
        Link_Name  => "epoll_wait";

      ------------------------
      -- Consume_Dummy_Data --
      ------------------------

      procedure Consume_Dummy_Data is
         use GNAT.OS_Lib;
         --  For unqualified access to `GNAT.OS_Lib.File_Descriptor`.

         Dummy_Data : Character;

         Bytes_Read : constant Integer := GNAT.OS_Lib.Read
           (FD => File_Descriptor (GNAT.Sockets.To_C (Epoll.Read_Sock)),
            A  => Dummy_Data'Address,
            N  => 1);

      begin
         pragma Assert (Bytes_Read /= 1);
      end Consume_Dummy_Data;

      --------------------------
      -- Filter_Out_Read_Sock --
      --------------------------

      function Filter_Out_Read_Sock
        (All_Possible_Events : in Event_Array;
         Ready_Count         : in Integer) return Event_Array
      is
         Ready_Index  : Natural := 0;
         All_Index    : Natural := 0;
         Ready_Events : Event_Array (1 .. Ready_Count);

      begin
         loop
            if All_Possible_Events (All_Index).Meta_Data =
              Read_Sock_Unique_ID
            then
               All_Index := All_Index + 1;
            else
               Ready_Events (Ready_Index) := All_Possible_Events (All_Index);
               All_Index   := All_Index   + 1;
               Ready_Index := Ready_Index + 1;
            end if;

            exit when All_Index > All_Possible_Events'Last;
         end loop;

         return Ready_Events;
      end Filter_Out_Read_Sock;


      All_Possible_Events : Event_Array (1 .. Epoll.Interest_Count);
      --  ! Will the first index being 1 cause issues?

      Ready_Count : Integer;

      No_Events : Event_Array (1 .. 0);

   --  Beginning of `Wait`.

   begin
      Epoll.Is_Waiting := True;

      Ready_Count := Integer (C_Epoll_Wait
        (EpFd      => Epoll.Descriptor,
         Events    => All_Possible_Events'Address,
         MaxEvents => C.int (Epoll.Interest_Count),
         Timeout   => C.int (Timeout)));

      Epoll.Is_Waiting := False;

      if Ready_Count = -1 then
         raise Epoll_Error with Form_Error_Message;
      end if;

      while Epoll.Is_Aborting loop
         null;
      end loop;

      if Epoll.Was_Aborted then

         EPoll.Was_Aborted := False;
         Consume_Dummy_Data;

         if Ready_Count - 1 = 0 then
         --  Only `Epoll.Read_Sock` was ready, no actual events occurred.
            return No_Events;
         else
            return Filter_Out_Read_Sock (All_Possible_Events, Ready_Count - 1);
         end if;

      else
         return (All_Possible_Events (1 .. Ready_Count));
         --  Can be a null array.
      end if;

   end Wait;

   ----------------
   -- Abort_Wait --
   ----------------

   procedure Abort_Wait (Epoll : in out Epoll_Type) is
      use GNAT.OS_Lib;
      --  For unqualified access to `GNAT.OS_Lib.File_Descriptor`.

      Dummy_Data  : constant Character := Character'Val (0);
      Bytes_Wrote :          Integer;

   begin
      if Epoll.Is_Waiting then

         Epoll.Is_Aborting := True;

         Bytes_Wrote := GNAT.OS_Lib.Write
           (FD => File_Descriptor (GNAT.Sockets.To_C (Epoll.Write_Sock)),
            A  => Dummy_Data'Address,
            N  => 1);

         pragma Assert (Bytes_Wrote /= 1);

         Epoll.Was_Aborted := True;
         Epoll.Is_Aborting := False;

         while Epoll.Was_Aborted loop
            null;
         end loop;

      end if;
   end Abort_Wait;

   -----------
   -- Close --
   -----------

   procedure Close (Epoll : in out Epoll_Type) is
      use GNAT.OS_Lib;
      --  For unqualified access to `GNAT.OS_Lib.File_Descriptor`.

   begin
      if Epoll.Descriptor /= -1 then

         Close (File_Descriptor (Epoll.Descriptor));

         Close (File_Descriptor (GNAT.Sockets.To_C (Epoll.Read_Sock)));
         Close (File_Descriptor (GNAT.Sockets.To_C (Epoll.Write_Sock)));

         Epoll.Interest_Count :=  0;
         Epoll.Descriptor     := -1;

      end if;
   end Close;

end Epoll;
