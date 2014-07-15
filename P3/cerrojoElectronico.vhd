----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    13:13:29 11/08/2013 
-- Design Name: 
-- Module Name:    cerrojoElectronico - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity cerrojoElectronico is
port (
	-- Entradas
	intro: in std_logic;						-- Boton para confirmar la introduccion de clave
	switch: in std_logic_vector(7 downto 0);	-- Switches para escribir la clave
	clk: in std_logic;							-- Reloj
	rst: in std_logic;							-- Reset
	
	-- Salidas
	segs: out std_logic_vector(7 downto 0);		-- Display 8-Segmentos para visualizar numero de intentos restantes
	lock: out std_logic						-- Encederemos el primer LED para indicar que esta LOCK
);
end cerrojoElectronico;

architecture Behavioral of cerrojoElectronico is
	
	-- CERROJO ELECTRONICO
	signal xDebounced,xDebRiseEdge,xDebFallingEdge: std_logic;
	signal load: std_logic;
	signal salidaRegistro: std_logic_vector(7 downto 0);
	signal rightSeg: std_logic_vector(7 downto 0);
	
	-- MAQUINA DE ESTADOS
	type ESTADOS is (INICIAL,S0, S1, S2, S3);
	signal ESTADO, SIG_ESTADO: ESTADOS;
	signal st: std_logic_vector(3 downto 0);
	--
	signal clave: std_logic_vector(7 downto 0);
	
	component debouncer
		port(rst: in std_logic;
			  clk: in std_logic;
			  x: in std_logic;
			  xDeb: out std_logic;
			  xDebFallingEdge: out std_logic;
			  xDebRisingEdge: out std_logic);
	end component;
	
	component binToSeg 
	port (
		bin: in std_logic_vector(3 downto 0);
		displaySeg: out std_logic_vector(7 downto 0)		-- 7 = A / 6 = B / ... / 0 = H
	);
	end component;

begin

	 segs <= rightSeg;
	 clave <= switch;
	 
	 button: debouncer port map (rst,clk,intro,xDebounced,xDebFallingEdge,xDebRiseEdge);
	 
	 registro: 
	 process (clk, rst)
	 begin
		-- Los Push Buttons, como los Switches, tienen logica negativa, es decir, es al reves.
		if rst='0' then
			salidaRegistro <= (others=>'0');
		elsif (clk'event and clk='1') then 
			if load='1' then
				salidaRegistro <= switch;
			end if;
		end if;
	 end process;

	 -- MAQUINA ESTADOS: SINCRONO
	 maqEstadosSyn: 
    process(clk,rst)
      begin
        if rst ='0' then
            ESTADO <= INICIAL;
          elsif clk'event and clk='1' then
            ESTADO <= SIG_ESTADO;
        end if;
    end process;
	 
	 -- MAQUINA ESTADOS: COMBINACIONAL
	 maqEstadosComb:
    process(ESTADO,rst,xDebFallingEdge,salidaRegistro,clave)
      begin
		  SIG_ESTADO <= ESTADO;
		  load <= '0';
		  lock <= '1';
        case ESTADO is
			 when INICIAL => 
			 
			 		lock <= '0';
					st <= "1010";

					if xDebFallingEdge = '1' then
							load <= '1';
							SIG_ESTADO <= S3;
					end if;

			 when S3 => 
			 
					st <= "0011";

			 		if xDebFallingEdge = '1' and (salidaRegistro = clave) then
							SIG_ESTADO <= INICIAL;
					elsif xDebFallingEdge = '1' and (salidaRegistro /= clave) then
							SIG_ESTADO <= S2;
					end if;
						
			 when S2 => 
			 			
					st <= "0010";

					if xDebFallingEdge = '1' and (salidaRegistro = clave) then
							SIG_ESTADO <= INICIAL;
					elsif xDebFallingEdge = '1' and (salidaRegistro /= clave) then
							SIG_ESTADO <= S1;
					end if;
			 
			 when S1 => 
			 
					st <= "0001";
					
			 		if xDebFallingEdge = '1' and (salidaRegistro = clave) then
							SIG_ESTADO <= INICIAL;
					elsif xDebFallingEdge = '1' and (salidaRegistro /= clave) then
							SIG_ESTADO <= S0;
					end if;
					
			 when S0 => 
               
					st <= "0000";

					
        end case;
      end process;   
	  
	 displayCuenta: binToSeg port map (st,rightSeg);


end Behavioral;

