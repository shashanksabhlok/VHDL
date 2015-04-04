library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--
-- 7-segment display driver. It displays a 4-bit number on 7-segments 
-- This is created as an entity so that it can be reused many times easily
--

entity SevenSegment is port (
   
   dataIn      :  in  std_logic_vector(3 downto 0);   -- The 4 bit data to be displayed
   blanking    :  in  std_logic;                      -- This bit turns off all segments
   
   segmentsOut :  out std_logic_vector(6 downto 0)    -- 7-bit outputs to a 7-segment
); 
end SevenSegment;

architecture Behavioral of SevenSegment is

-- 
-- The following statements convert a 4-bit input, called dataIn to a pattern of 7 bits
-- The segment turns on when it is '0' otherwise '1'
-- The blanking input is added to turns off the all segments
--

begin

   with blanking & dataIn select --  gfedcba        b3210      -- D7S
      segmentsOut(6 downto 0) <=    "1000000" when "00000",    -- [0]
                                    "1111001" when "00001",    -- [1]
                                    "0100100" when "00010",    -- [2]      +---- a ----+
                                    "0110000" when "00011",    -- [3]      |           |
                                    "0011001" when "00100",    -- [4]      |           |
                                    "0010010" when "00101",    -- [5]      f           b
                                    "0000010" when "00110",    -- [6]      |           |
                                    "1111000" when "00111",    -- [7]      |           |
                                    "0000000" when "01000",    -- [8]      +---- g ----+
                                    "0010000" when "01001",    -- [9]      |           |
                                    "0001000" when "01010",    -- [A]      |           |
                                    "0000011" when "01011",    -- [b]      e           c
                                    "0100111" when "01100",    -- [c]      |           |
                                    "0100001" when "01101",    -- [d]      |           |
                                    "0000110" when "01110",    -- [E]      +---- d ----+
                                    "0001110" when "01111",    -- [F]
                                    "1111111" when others;     -- [ ]

end Behavioral;

--------------------------------------------------------------------------------
-- Main entity
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Lab2 is port (
      
      key              :   in  std_logic_vector( 3 downto 0); -- 4 push buttons
      sw               :   in  std_logic_vector(17 downto 0); -- 18 dip switches

      ledr             :   out std_logic_vector(17 downto 0); -- 18 red LEDs
      hex0, hex1, hex2, hex3, hex7, hex6, hex4, hex5 :   out std_logic_vector( 6 downto 0)  -- 7-segment desplays
);
end Lab2;

architecture SimpleCircuit of Lab2 is

--
-- In order to use the "SevenSegment" entity, we should declare it with first
-- 

   component SevenSegment port (
      dataIn      :  in    std_logic_vector(3 downto 0);
      blanking    :  in    std_logic;
      segmentsOut :  out   std_logic_vector(6 downto 0)
   );
   end component;
  
-- Create any signals, or temporary variables to be used
--
-- Note that there are two basic types and mixing them is difficult
--  unsigned is a signal which can be used to perform math operations such as +, -, *
--  std_logic_vector is a signal which can be used for logic operations such as OR, AND, NOT, XOR
--
   signal A, B: std_logic_vector(7 downto 0); -- 3-bit intermediate signals (wires)
   signal R: std_logic_vector(11 downto 0);
	signal S: std_logic_vector(1 downto 0);
-- Here the circuit begins

begin

-- intermediate signal assignments

   A  <= sw(7 downto 0);  -- connect the lowest 3 switches to A
   B <= sw(15 downto 8);  -- connect the highest 3 switches to B
   S  <= sw (17 downto 16);
   
   
-- 2x1 multiplexer
   
  -- with key(0) select         -- key(0) selects A or B to be assigned to S
   --   S <= A   when '0',      -- A is assigned when key(0) = 0
    --       B   when others;   -- A is assigned when key(0) = 1
           
-- signal is assigned to LED

   ledr(11 downto  0) <= R;
   
   ledr(17 downto 16) <= S;

	--R<=std_logic_vector(unsigned("0000"&A)+unsigned("0000"&B));
	--R<= "0000"&A xor "0000"&B;
-- signal is sidplayed on seven-segment. '0' is concatenated with signal to make a 4-bit input
   --sel <= sw17 & sw16;
   WITH S  SELECT
    R<=       "0000"&A and "0000"&B WHEN "00",
              "0000"&A or "0000"&B WHEN  "01",
              "0000"&A xor "0000"&B WHEN "10",
              std_logic_vector(unsigned("0000"&A) + unsigned("0000"&B)) WHEN OTHERS;
			  
			  
   D7SH4: SevenSegment port map(A(3 downto 0), '0', hex4);
   D7SH5: SevenSegment port map(A(7 downto 4), '0', hex5);
   D7SH6: SevenSegment port map(B(3 downto 0), '0', hex6);
   D7SH7: SevenSegment port map(B(7 downto 4), '0', hex7 ); -- A is diplayed on HEX0, blanking is disabled
   D7SH0: SevenSegment port map(R(3 downto 0), '0', hex0 ); -- A is diplayed on HEX0, blanking is disabled
   D7SH1: SevenSegment port map(R(7 downto 4), '0', hex1 ); -- B is diplayed on HEX1, blanking is disabled
   D7SH3: SevenSegment port map(R(11 downto 8), '0', hex2 ); -- S is diplayed on HEX3, blanking is disabled

end SimpleCircuit;
