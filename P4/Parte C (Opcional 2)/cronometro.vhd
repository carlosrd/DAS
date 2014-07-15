library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
USE IEEE.std_logic_1164.ALL;
USE IEEE.std_logic_unsigned.ALL;
USE IEEE.std_logic_arith.ALL;

entity cronometro is
port (
	-- Entradas
	startStop: in std_logic;
	puestaCero: in std_logic;
	clk: in std_logic;
	rst: in std_logic;
	shift: in std_logic;
	
	-- Salidas
	rightSegs: out std_logic_vector(7 downto 0);		
	leftSegs: out std_logic_vector(7 downto 0);
	decimasSegs: out std_logic_vector(7 downto 0);
	puntoDec: out std_logic
);
end cronometro;

architecture Behavioral of cronometro is
	
	signal ssDebounced,ssDebFallEdge,ssDebRiseEdge: std_logic;
	signal pODebounced,pODebFallEdge,pODebRiseEdge: std_logic;
	signal t: std_logic;
	signal salidaCont5kk: std_logic_vector(22 downto 0);
	signal salidaContDecimas, salidaContUnidades,salidaContDecenas: std_logic_vector(3 downto 0);
	signal cuentaDecimas, cuentaUnidades, cuentaDecenas: std_logic;
	
	signal cs1 : std_logic_vector( 22 downto 0 );
	signal cs2,cs3,cs4,cs5,cs6 : std_logic_vector( 3 downto 0 );
	
	-- Se�ales adicionales para los contadores y visores de Minutos
	signal salidaContMinUnidades,salidaContMinDecenas: std_logic_vector(3 downto 0);
	signal muxRight, muxLeft: std_logic_vector(3 downto 0);
	signal cuentaUnidadesMinutos, cuentaDecenasMinutos: std_logic;

	signal lit1,lit2,lit3: std_logic;
	
	component debouncer
		port(rst: in std_logic;
			  clk: in std_logic;
			  x: in std_logic;
			  xDeb: out std_logic;
			  xDebFallingEdge: out std_logic;
			  xDebRisingEdge: out std_logic);
	end component;
	
	component binToSeg is
	port (
		bin: in std_logic_vector(3 downto 0);
		displaySeg: out std_logic_vector(7 downto 0)		-- 7 = A / 6 = B / ... / 0 = H
	);
	end component;
	
begin
	
	-- ELIMINACION DE REBOTES EN LOS PUSHBUTTONS
	debouncerStartStop: debouncer port map (rst,clk,startStop,ssDebounced,ssDebFallEdge,ssDebRiseEdge);
	debouncerPuestaCero: debouncer port map (rst,clk,puestaCero,pODebounced,pODebFallEdge,pODebRiseEdge);
	
	-- CONTADORES
	contMod5kk:
	process( clk, cs1, rst, pODebFallEdge, ssDebFallEdge )	
	begin
		salidaCont5kk <= cs1;
		if rst = '0' then
			cs1 <= conv_std_logic_vector( 0 , 23 );
		elsif clk'event and clk = '1' then
			if pODebFallEdge = '1' then
				cs1 <= conv_std_logic_vector( 0 , 23 );
			elsif  t = '1' then
				if cs1 = conv_std_logic_vector( 4999999 , 23 ) then
					cs1 <= conv_std_logic_vector( 0 , 23 );
				else
					cs1 <= cs1 + 1;
				end if;
			end if;
		end if;
	end process;
	
	-- contDecimas cuenta cuando se tenga 4.999.999 = 10011000100101100111111 en cont5Millones
	cuentaDecimas <= salidaCont5kk(22) and salidaCont5kk(19) and salidaCont5kk(18) and salidaCont5kk(14) and salidaCont5kk(11) and
					 salidaCont5kk(9) and salidaCont5kk(8) and salidaCont5kk(5) and salidaCont5kk(4) and salidaCont5kk(3) and
					 salidaCont5kk(2) and salidaCont5kk(1) and salidaCont5kk(0);

	contDecimas: 
	process( clk, cs2, rst, pODebFallEdge, cuentaDecimas )	
	begin
		salidaContDecimas <= cs2;
		if rst = '0' then
			cs2 <= conv_std_logic_vector( 0 , 4 );
		elsif clk'event and clk = '1' then
			if pODebFallEdge = '1' then
				cs2 <= conv_std_logic_vector( 0 , 4 );
			elsif  cuentaDecimas = '1' then
				if cs2 = conv_std_logic_vector( 9 , 4 ) then
					cs2 <= conv_std_logic_vector( 0 , 4 );
				else
					cs2 <= cs2 + 1;
				end if;
			end if;
		end if;
	end process;

	-- contUnidades cuenta cuando se tenga 9 = 1001 en contDecimas
	cuentaUnidades <= (salidaContDecimas(3) and salidaContDecimas(0)) and cuentaDecimas;
	
	contUnidades: 
	process( clk, cs3, rst, pODebFallEdge, cuentaUnidades )	
	begin
		salidaContUnidades <= cs3;
		if rst = '0' then
			cs3 <= conv_std_logic_vector( 0 , 4 );
		elsif clk'event and clk = '1' then
			if pODebFallEdge = '1' then
				cs3 <= conv_std_logic_vector( 0 , 4 );
			elsif  cuentaUnidades = '1' then
				if cs3 = conv_std_logic_vector( 9 , 4 ) then
					cs3 <= conv_std_logic_vector( 0 , 4 );
				else
					cs3 <= cs3 + 1;
				end if;
			end if;
		end if;
	end process;
	
	cuentaDecenas <= (salidaContUnidades(3) and salidaContUnidades(0)) and (cuentaDecimas and cuentaUnidades);
	
	contDecenas: 
	process( clk, cs4, rst, pODebFallEdge, cuentaDecenas )	
	begin
		salidaContDecenas <= cs4;
		if rst = '0' then
			cs4 <= conv_std_logic_vector( 0 , 4 );
		elsif clk'event and clk = '1' then
			if pODebFallEdge = '1' then
				cs4 <= conv_std_logic_vector( 0 , 4 );
			elsif  cuentaDecenas = '1' then
				if cs4 = conv_std_logic_vector( 5 , 4 ) then
					cs4 <= conv_std_logic_vector( 0 , 4 );
				else
					cs4 <= cs4 + 1;
				end if;
			end if;
		end if;
	end process;
	
	-- Cuenta minutos cuando Unidades = 9 y Decenas = 5
	cuentaUnidadesMinutos <= ((salidaContUnidades(3) and salidaContUnidades(0) and
					 salidaContDecenas(2) and salidaContDecenas(0)) and (cuentaDecimas and cuentaUnidades and cuentaDecenas));
																 			 
	contMinutosUnidades: 
	process( clk, cs5, rst, pODebFallEdge, cuentaUnidadesMinutos )	
	begin
		salidaContMinUnidades <= cs5;
		if rst = '0' then
			cs5 <= conv_std_logic_vector( 0 , 4 );
		elsif clk'event and clk = '1' then
			if pODebFallEdge = '1' then
				cs5 <= conv_std_logic_vector( 0 , 4 );
			elsif  cuentaUnidadesMinutos = '1' then
				if cs5 = conv_std_logic_vector( 9 , 4 ) then
					cs5 <= conv_std_logic_vector( 0 , 4 );
				else
					cs5 <= cs5 + 1;
				end if;
			end if;
		end if;
	end process;
	
	cuentaDecenasMinutos <= salidaContMinUnidades(3) and salidaContMinUnidades(0) and cuentaUnidadesMinutos;
	
	contMinutosDecenas: 
	process( clk, cs5, rst, pODebFallEdge, cuentaDecenasMinutos )	
	begin
		salidaContMinDecenas <= cs6;
		if rst = '0' then
			cs6 <= conv_std_logic_vector( 0 , 4 );
		elsif clk'event and clk = '1' then
			if pODebFallEdge = '1' then
				cs6 <= conv_std_logic_vector( 0 , 4 );
			elsif  cuentaDecenasMinutos = '1' then
				if cs6 = conv_std_logic_vector( 5 , 4 ) then
					cs6 <= conv_std_logic_vector( 0 , 4 );
				else
					cs6 <= cs6 + 1;
				end if;
			end if;
		end if;
	end process;
	
	-- BIESTABLE T
	-- Usamos el flanco de bajada (fall) porque la placa tiene logica negativa
	biestableT:
	process(clk)
	begin
		if (rst = '0') then
			t <= '0';
		elsif (clk'event and clk='1') then
			if (ssDebFallEdge = '1') then
				t <= not (t);
			
				end if;
		end if;
	end process;
	
	parpadeoPuntoDecimal:
	process(salidaContDecimas)
	begin
		lit1 <= not salidaContDecimas(3) and not salidaContDecimas(2);
		lit2 <= not salidaContDecimas(3) and not salidaContDecimas(1) and not salidaContDecimas(0);
		lit3 <= lit1 or lit2;
		if (lit3 = '1') then
			puntoDec <= '1';
		else
			puntoDec <= '0';
		end if;
		
	end process;

	muxLeft <= salidaContDecenas when shift = '1' else salidaContMinDecenas;
	muxRight <= salidaContUnidades when shift = '1' else salidaContMinUnidades;
	
	-- DISPLAYS 8 SEGMENTOS
	deciSeg: binToSeg port map (salidaContDecimas,decimasSegs);
	
	decenasSegm: binToSeg port map (muxLeft,leftSegs);
	unidadesSegm: binToSeg port map (muxRight,rightSegs);
	
end Behavioral;


--	x3 x2 x1 x0   z
--------------------
--0	 0  0  0  0	  1
--1	 0  0  0  1   1
--2	 0  0  1  0   1
--3	 0  0  1  1   1
--4	 0  1  0  0   1

--5	 0  1  0  1   0
--6	 0  1  1  0   0
--7	 0  1  1  1   0
--8	 1  0  0  0   0
--9	 1  0  0  1   0

-- Mapa de Karnaugh ==> FLASH = (¬ x3)(¬ x2) + (¬ x3)(¬ x1)(¬ x0)
