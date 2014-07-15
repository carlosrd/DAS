----------------------------------------------------------------------------------
-- ESQUEMA LEDS 8 SEGMENTOS:

--
--			     A	
--			    ---
--		    F |   | B
--			    -G-
--		    E |   | C
--			    ---     . DP
--			     D

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

	-- Lista de componentes que se van a utilizar dentro de esta arquitectura
	component binToSeg
		port( bin: in std_logic_vector(3 downto 0);
			 displaySeg: out std_logic_vector(7 downto 0));
	end component;
	
	-- Señales (cables) auxiliares
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
		
	-- Creamos 2 instancias del componente "binToSeg" y como entrada
	-- uno de los bloques dividido antes y salida una de la arquitectura
	
	leftDisplay: binToSeg port map (leftSwitch,leftSeg);	
	rightDisplay: binToSeg port map (rightSwitch,rightSeg);
	
 
end Behavioral;

