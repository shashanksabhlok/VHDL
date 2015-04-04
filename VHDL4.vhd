LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

--
-- 7-segment display driver. It displays a 4-bit number on 7-segments 
-- This is created as an entity so that it can be reused many times easily
--

ENTITY SevenSegment IS PORT (
   
   dataIn      :  IN  std_logic_vector(3 DOWNTO 0);   -- The 4 bit data to be displayed
   blanking    :  IN  std_logic;                      -- This bit turns off all segments
   segmentsOut :  OUT std_logic_vector(6 DOWNTO 0)    -- 7-bit outputs to a 7-segment
); 
END SevenSegment;

ARCHITECTURE Behavioral OF SevenSegment IS

-- 
-- The following statements convert a 4-bit input, called dataIn to a pattern of 7 bits
-- The segment turns on when it is '0' otherwise '1'
-- The blanking input is added to turns off the all segments
--

BEGIN

   with blanking & dataIn SELECT --  gfedcba        b3210      -- D7S
      segmentsOut(6 DOWNTO 0) <=    "1000000" WHEN "00000",    -- [0]
                                    "1111001" WHEN "00001",    -- [1]
                                    "0100100" WHEN "00010",    -- [2]      +---- a ----+
                                    "0110000" WHEN "00011",    -- [3]      |           |
                                    "0011001" WHEN "00100",    -- [4]      |           |
                                    "0010010" WHEN "00101",    -- [5]      f           b
                                    "0000010" WHEN "00110",    -- [6]      |           |
                                    "1111000" WHEN "00111",    -- [7]      |           |
                                    "0000000" WHEN "01000",    -- [8]      +---- g ----+
                                    "0010000" WHEN "01001",    -- [9]      |           |
                                    "0001000" WHEN "01010",    -- [A]      |           |
                                    "0000011" WHEN "01011",    -- [b]      e           c
                                    "0100111" WHEN "01100",    -- [c]      |           |
                                    "0100001" WHEN "01101",    -- [d]      |           |
                                    "0000110" WHEN "01110",    -- [E]      +---- d ----+
                                    "0001110" WHEN "01111",    -- [F]
                                    "1111111" WHEN OTHERS;     -- [ ]

END Behavioral;

--------------------------------------------------------------------------------
-- Main entity
--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY Lab4 IS
   PORT(
      
      clock_50   : IN  STD_LOGIC;
      sw         : IN  STD_LOGIC_VECTOR(17 DOWNTO 0); -- 18 dip switches on the board

      ledr       : OUT STD_LOGIC_VECTOR(17 DOWNTO 0); -- LEDs, many Red ones are available
      ledg       : OUT STD_LOGIC_VECTOR( 8 DOWNTO 0); -- LEDs, many Green ones are available
      hex0, hex2 : OUT STD_LOGIC_VECTOR( 6 DOWNTO 0)  -- seven segments to display numbers
);
END Lab4;

ARCHITECTURE SimpleCircuit OF Lab4 IS

--
-- In order to use the "SevenSegment" entity, we should declare it with first
-- 

   COMPONENT SevenSegment PORT(        -- Declare the 7 segment component to be used
      dataIn      : IN  STD_LOGIC_VECTOR(3 DOWNTO 0);
      blanking    : IN  STD_LOGIC;
      segmentsOut : OUT STD_LOGIC_VECTOR(6 DOWNTO 0)
   );
   END COMPONENT;
----------------------------------------------------------------------------------------------------
   CONSTANT CLK_DIV_SIZE: INTEGER := 25;     -- size of vectors for the counters

   SIGNAL Main1HzCLK:   STD_LOGIC; -- main 1Hz clock to drive FSM
   SIGNAL OneHzBinCLK:  STD_LOGIC; -- binary 1 Hz clock
   SIGNAL OneHzModCLK:  STD_LOGIC; -- modulus1 Hz clock

   SIGNAL bin_counter:  UNSIGNED(3 DOWNTO 0) := to_unsigned(0,4); -- reset binary counter to zero
   signal tenmod_counter: unsigned(24 downto 0);
   SIGNAL mod_counter:  UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE); -- reset modulus counter to zero
   SIGNAL mod_terminal: UNSIGNED(CLK_DIV_SIZE-1 DOWNTO 0) := to_unsigned(0,CLK_DIV_SIZE); -- reset terminal count of modulus counter to zero
   
   TYPE STATES IS (STATE0, STATE1, STATE2, STATE3);   -- list all the STATES
   SIGNAL state, next_state:  STATES;                 -- current and next state signals od type STATES
   
   SIGNAL state_number: STD_LOGIC_VECTOR(3 DOWNTO 0); -- binary state number to display on seven-segment

   SIGNAL state_counter: UNSIGNED(3 DOWNTO 0);        -- binary state counter to display on seven-segment
----------------------------------------------------------------------------------------------------

BEGIN

   -- nBinCLK: PROCESS(clock_50)
   --BEGIN
     -- IF (rising_edge(clock_50)) THEN -- binary counter increments on rising clock edge
       --  bin_counter <= bin_counter + 1;
      --END IF;
   --END PROCESS;
   --OneHzBinCLK <= std_logic(bin_counter(CLK_DIV_SIZE-1)); -- binary counter MSB
   --LEDG(2) <= OneHzBinCLK;
----------------------------------------------------------------------------------------------------
   WITH sw(2 DOWNTO 0) SELECT -- terminal count for modulus counter for F0= 50 MHz clock input (T0 = 20 ns) 
      mod_terminal <= "1011111010111100000111111" WHEN "000",  -- F =   1 Hz, T/2 = 25000000 * T0
                      "0010011000100101100111111" WHEN "001",  -- F =   5 Hz, T/2 =  5000000 * T0
                      "0001001100010010110011111" WHEN "010",  -- F =  10 Hz, T/2 =  2500000 * T0
                      "0000000111101000010001111" WHEN "011",  -- F = 100 Hz, T/2 =   250000 * T0
                      "1011111010111100000111111" WHEN OTHERS; -- *** default ***

   ModCLK: PROCESS(clock_50) 
   BEGIN
      IF (rising_edge(clock_50)) THEN -- modulus counter increments on rising clock edge
         IF (mod_counter = 25000000) THEN       -- half period
            OneHzModCLK <= NOT OneHzModCLK;                 -- toggle
            mod_counter <= to_unsigned(0,CLK_DIV_SIZE); -- reset counter
         ELSE
            mod_counter <= mod_counter + 1;
         END IF;
      END IF;
   END PROCESS;
   --LEDG(1) <= OneHzModCLK;
   
    -- ModCLK : PROCESS (clock_50)
		--if (tenmod_counter = 1000000)

----------------------------------------------------------------------------------------------------
   Main1HzCLK <= OneHzModCLK; --WHEN (sw(17) = '0') ELSE OneHzBinCLK; -- assigns either binary or modulus clock as main clock
   --LEDG(0) <= Main1HzCLK; --tried moving into fsm
----------------------------------------------------------------------------------------------------
   FSM: PROCESS(state, sw) -- main FSM
   BEGIN
--      next_state <= state; 	-- The only purpose of this line is to give initial value to the signal 'next_state' in order to avoid latch creation. 
      ledr(15 DOWNTO 0) <= "0000000000000000";
      CASE state IS
         WHEN STATE0 =>
            state_number <= "0000";
            ledr(15 DOWNTO 0) <= "0000000000001111";
            --mod_terminal <= "1011111010111100000111111"
            LEDG(0) <= Main1HzCLK;
            LEDG(1) <= OneHzModCLK;
            IF (state_counter = 2) THEN 
               next_state <= STATE1;
            ELSE
               next_state <= STATE0;
            END IF;
         WHEN STATE1 =>
            state_number <= "0001";
            ledr(15 DOWNTO 0) <= "0000000011110000";
            mod_terminal <= "0010011000100101100111111";
            LEDG(0) <= Main1HzCLK;
            LEDG(1) <= OneHzModCLK;
            IF (state_counter = 7) THEN
               next_state <= STATE2;
            ELSE
               next_state <= STATE1;
            END IF;
         WHEN STATE2 =>
            state_number <= "0010";
            ledr(15 DOWNTO 0) <= "0000111100000000";
            IF (state_counter = 10) THEN
               next_state <= STATE3;
            ELSE
               next_state <= STATE2;
            END IF;
         WHEN OTHERS => -- STATE3
            state_number <= "0011";
            ledr(15 DOWNTO 0) <= "1111000000000000";
            IF (state_counter = 15) THEN
               next_state <= STATE0;
            ELSE
               next_state <= STATE3;
            END IF;
      END CASE;
   END PROCESS;
----------------------------------------------------------------------------------------------------
   SeqLogic: PROCESS(Main1HzCLK, state) -- creats sequential logic to latch the state
   BEGIN
      IF (rising_edge(Main1HzCLK)) THEN
         
         state <= next_state;                      -- on the rising edge of clock the current state is updated with next state
         
         
            state_counter <= state_counter + 1;    -- on the rising edge of clock the current counter is incremented if state is STATE1
         
      END IF;
   END PROCESS;
----------------------------------------------------------------------------------------------------
   D7S0: SevenSegment PORT MAP( state_number, '0', hex0 );
   D7S4: SevenSegment PORT MAP( std_logic_vector(state_counter), '0', hex2 );

END SimpleCircuit;
