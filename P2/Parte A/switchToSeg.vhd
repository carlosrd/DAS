----------------------------------------------------------------------------------
-- ESQUEMA LEDS 8 SEGMENTOS:

--
--			    A	
--			   ---
--		   F |   | B
--			   -G-
--		   E |   | C
--			   ---     . DP
--			    D

----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity switchToSeg is
port (
   switch: in std_logic_vector(7 downto 0);
	rightSeg: out std_logic_vector(7 downto 0);		
	leftSeg: out std_logic_vector(7 downto 0)		-- 7 = A / 6 = B / ... / 0 = H
);
end switchToSeg;

architecture Behavioral of switchToSeg is

	signal rightSwitch: std_logic_vector(3 downto 0);
	signal leftSwitch: std_logic_vector(3 downto 0);		
	
begin

	-- Partimos los 8 switch en 2 bloques de 4. 
	-- Recordamos de la practica anterior que es necesario usar
	-- un negador con los switch para que funcionen de la manera esperada

	leftSwitch(3) <= not(switch(0));
	leftSwitch(2) <= not(switch(1));
	leftSwitch(1) <= not(switch(2));
	leftSwitch(0) <= not(switch(3));
	
	rightSwitch(3) <= not(switch(4));
	rightSwitch(2) <= not(switch(5));
	rightSwitch(1) <= not(switch(6));
	rightSwitch(0) <= not(switch(7));
		
	with leftSwitch select
		leftSeg <= "11111100" when "0000",		-- 0 => Leds: A,B,C,D,E,F
				     "01100000" when "0001",		-- 1 => Leds: B,C
				     "11011010" when "0010",		-- 2 => Leds: A,B,G,E,D
				     "11110010" when "0011",		-- 3 => Leds: A,B,C,D,G
				     "01100110" when "0100",		-- 4 => Leds: B,C,F,G
				     "10110110" when "0101",		-- 5 => Leds: A,C,D,F,G
				     "10111110" when "0110",		-- 6 => Leds: A,C,D,E,F,G
				     "11100000" when "0111",		-- 7 => Leds: A,B,C
				     "11111110" when "1000",		-- 8 => Leds: A,B,C,D,E,F,G
				      "11110110" when "1001",		-- 9 => Leds: A,B,C,D,F,G	
						"11101110" when "1010",		-- A(10) => Leds: A,B,C,E,F,G
						"00111110" when "1011",		-- B(11) => Leds: A,B,C,D,E,F,G
						"10011100" when "1100",		-- C(12) => Leds: A,D,E,F
						"01111010" when "1101",		-- D(13) => Leds: A,B,C,D,E,F
						"10011110" when "1110",		-- E(14) => Leds: A,D,E,F,G
						"10001110" when "1111",		-- F(15) => Leds: A,E,F,G
				      "00000001" when others;		-- En cualquier otro caso encendemos el "."		  	   
	
	with rightSwitch select
		rightSeg <= "11111100" when "0000",		-- 0 => Leds: A,B,C,D,E,F
				      "01100000" when "0001",		-- 1 => Leds: B,C
				      "11011010" when "0010",		-- 2 => Leds: A,B,G,E,D
				      "11110010" when "0011",		-- 3 => Leds: A,B,C,D,G
				      "01100110" when "0100",		-- 4 => Leds: B,C,F,G
				      "10110110" when "0101",		-- 5 => Leds: A,C,D,F,G
				      "10111110" when "0110",		-- 6 => Leds: A,C,D,E,F,G
				      "11100000" when "0111",		-- 7 => Leds: A,B,C
				      "11111110" when "1000",		-- 8 => Leds: A,B,C,D,E,F,G
				      "11110110" when "1001",		-- 9 => Leds: A,B,C,D,F,G	
						"11101110" when "1010",		-- A(10) => Leds: A,B,C,E,F,G
						"00111110" when "1011",		-- B(11) => Leds: A,B,C,D,E,F,G
						"10011100" when "1100",		-- C(12) => Leds: A,D,E,F
						"01111010" when "1101",		-- D(13) => Leds: A,B,C,D,E,F
						"10011110" when "1110",		-- E(14) => Leds: A,D,E,F,G
						"10001110" when "1111",		-- F(15) => Leds: A,E,F,G
				      "00000001" when others;		-- En cualquier otro caso encendemos el "."		   		
 
end Behavioral;

