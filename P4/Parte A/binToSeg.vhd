----------------------------------------------------------------------------------
-- ESQUEMA LEDS 8 SEGMENTOS:

--
--			    A	
--			   ---
--		    F |   | B
--			   -G-
--		    E |   | C
--			   ---     . DP
--			    D

----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity binToSeg is
port (
    bin: in std_logic_vector(3 downto 0);
	displaySeg: out std_logic_vector(7 downto 0)		-- 7 = A / 6 = B / ... / 0 = H
);
end binToSeg;

architecture Converter of binToSeg is
begin
	
	with bin select
		displaySeg <= "11111100" when "0000",	-- 0 => Leds: A,B,C,D,E,F
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
 
end Converter;

