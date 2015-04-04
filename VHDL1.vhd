library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.ALL;

-- The above libaries lines must be included in every VHDL file, before EVERY ENTITY!

--
-- This is the main circuit Entity, which connects all wires to the FPGA pins (lights and switches)
-- First we have a PORT mapping - naming all wires going to the outside world and if they are INputs or OUTputs
-- Note that all signal names here are fixed by the "DE2_pins.csv" file which you must use for every lab
--   

entity Lab1 is port(
      
      key   : in  std_logic_vector(3 downto 0); -- 4 push buttons on the board - normally HIGH or '1' when not pressed
      sw    : in  std_logic_vector(5 downto 0); -- use 6 out of 18 switches on the board - '0' (LOW) when down towards edge of board

      ledr  : out std_logic_vector(1 downto 0); -- Red LEDs, only 2 used
      ledg  : out std_logic_vector(1 downto 0)  -- Green LEDs, only 2 used
);
end Lab1;

architecture SimpleCircuit of Lab1 is

signal gas, brake, clutch, override: std_logic; --four signals for input
signal red_led0, red_led1: std_logic;  -- two signals for Red LED outputs

-- The function of Lab1 entity is defined here

begin

  --MAIN LOGIC
  --gas control is defined to be on only when the gas is on and everything else is off
  --taking into consideration that 'on' for keys is 0, and off is 1
  --and employing the and gate ,since it is only true for one case
   
   ledg(0) <= (not gas)  and brake and clutch and  (not override);
   ledr(0) <=  (not brake) or (override);

-- assign  input variables to hardware
   
    -- key(0) is the gas switch
	gas <= key(0);
	-- key(1) is the clutch switch
	clutch <= key(1);
	-- key(2) is the brake switch
	brake <= key(2);
	-- sw(1) is the override switch
	override <= sw(1);
	


end SimpleCircuit;
