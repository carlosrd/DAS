library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
USE IEEE.std_logic_1164.ALL;
USE IEEE.std_logic_unsigned.ALL;
USE IEEE.std_logic_arith.ALL;

entity casioTone is
port (
	-- Entradas
	ps2Clk: in std_logic;
	ps2Data: in std_logic;
	clk: in std_logic;
	rst: in std_logic;
	
	-- Salidas
	altavoz: out std_logic
);
end casioTone;

architecture Behavioral of casioTone is

	signal musica: std_logic;
	
	-- MEMORIA ROM
	signal nota : std_logic_vector(16 downto 0);
 	signal scanCode : std_logic_vector (7 downto 0);

	-- OSCILADOR
	signal salidaContOscilador,cs : std_logic_vector(16 downto 0);
	
	-- INTERFAZ TECLADO PS/2
	signal data : std_logic_vector (7 DOWNTO 0);  	-- Salida de datos paralela
	signal newData : std_logic;                   	-- Indica la recepción de un nuevo dato por la línea PS2  
	signal newDataAck : std_logic;                  -- Reconoce la recepción del nuevo dato
	signal ldData, validData, lastBitRcv, ps2ClkSync, ps2ClkFallingEdge: std_logic;
	signal ps2DataRegOut: std_logic_vector(10 downto 0);
	signal goodParity: std_logic;
	
	
	-- MAQUINA DE ESTADOS
	type ESTADOS is (WAITING_PRESS, WAITING_F0, WAITING_RELEASE, IGNORE_RELEASE);
	signal ESTADO, SIG_ESTADO: ESTADOS;
	
	-- REGISTRO SCANCODE
	signal loadScanCode, clearScanCode : std_logic;
	signal regOut : std_logic_vector (7 downto 0);
	
begin
	-- SALIDA DE AUDIO
	-- *******************************************************************************************
	
	altavoz <= musica;
	
	-- TABLA DE NOTAS (ESCALA LA = 440Hz)
	-- *******************************************************************************************
	-- Calculo del semiperiodo de oscilacion:
	-- ( 1 / Frec Nota (Hz) ) / ( 1 / Frec CLK (Hz) )
	-- Ej DO: (1 / 261.6 Hz ) / ( 1 / 50 Mhz) = 191131,498 ~ 191132 Ciclos/Perido => Semiperiodo: (Ciclos/Periodo) / 2 = 95566
	
	tablaNotas:
	with scanCode select
		nota <= "10111010101001110" when "00011100",			-- A => DO / 95566 ciclos semiperiodo when A = 1C				  
						"10110000001001100" when "00011101",		-- W => DO# / 90188 semiperiodo when W = 1D	
				  "10100110010000001" when "00011011",			-- S => RE / 85121 semiperiodo when S = 1B
						"10011100111101000" when "00100100",		-- E => RE# / 80360 semiperiodo when E = 24
				  "10010100001001010" when "00100011",			-- D => MI / 75850 semiperiodo when D = 23
				  "10001011110101000" when "00101011",			-- F => FA / 71592 semiperiodo when F = 2B
				  		"10000011111110000" when "00101100",		-- T => FA# / 67568 semiperiodo when T = 2C
				  "01111100100100000" when "00110100",			-- G => SOL / 63776 semiperiodo when G = 34
						"01110101100100101" when "00110101",		-- Y => SOL# / 60197 semiperiodo when Y = 35
				  "01101110111110010" when "00110011",			-- H => LA / 56818 semiperiodo when H = 33
						"01101000101111001" when "00111100",		-- U => LA# / 53625 semiperiodo when U = 3C
				  "01100010110111010" when "00111011",			-- J => SI / 50618 semiperiodo when J = 3B
				  "01011101010011110" when "01000010",			-- K => DO / 47774 semiperiodo when K = 42  
				  "00000000000000000" when others;


	-- OSCILADOR
	-- *******************************************************************************************

	oscilador:
	process( clk, rst )	
	begin
		salidaContOscilador <= cs;
		if rst = '0' then
			cs <= conv_std_logic_vector( 0 , 17 );
			musica <= '0';
		elsif clk'event and clk = '1' then
			if  salidaContOscilador = nota then
				cs <= conv_std_logic_vector( 0 , 17 );
				musica <= not (musica);
			elsif cs = conv_std_logic_vector( 131071 , 17 ) then
					cs <= conv_std_logic_vector( 0 , 17 );
				else
					cs <= cs + 1;
			end if;
		end if;
	end process;


	
	-- INTERFAZ TECLADO PS/2
	-- *******************************************************************************************

	synchronizer:
	PROCESS (rst, clk)
		VARIABLE aux1: std_logic;
	BEGIN
		IF (rst='0') THEN
			aux1 := '1';
			ps2ClkSync <= '1';
		ELSIF (clk'EVENT AND clk='1') THEN
				ps2ClkSync <= aux1;
				aux1 := ps2Clk;           
		END IF;
	END PROCESS synchronizer;

	edgeDetector: 
	PROCESS (rst, clk)
		VARIABLE aux1, aux2: std_logic;
	BEGIN
		ps2ClkFallingEdge <= (NOT aux1) AND aux2;
		IF (rst='0') THEN
			aux1 := '1';
			aux2 := '1';
		ELSIF (clk'EVENT AND clk='1') THEN
				aux2 := aux1;
				aux1 := ps2ClkSync;           
		END IF;
	END PROCESS edgeDetector;

	ps2DataReg:
	PROCESS (rst, clk)
	BEGIN
		IF (rst='0') THEN
			ps2DataRegOut <= (OTHERS =>'1');    
		ELSIF (clk'EVENT AND clk='1') THEN
			IF (lastBitRcv='1') THEN
				ps2DataRegOut <= (OTHERS=>'1'); 	
			ELSIF (ps2ClkFallingEdge='1') THEN
				ps2DataRegOut <= ps2Data & ps2DataRegOut(10 downto 1);
			END IF;
		END IF;
	END PROCESS ps2DataReg;

	oddParityCheker:
	goodParity <= 
		((ps2DataRegOut(9) XOR ps2DataRegOut(8)) XOR (ps2DataRegOut(7) XOR ps2DataRegOut(6)))
		XOR ((ps2DataRegOut(5) XOR ps2DataRegOut(4)) XOR (ps2DataRegOut(3) XOR ps2DataRegOut(2)))
		XOR ps2DataRegOut(1);

	lastBitRcv <= NOT ps2DataRegOut(0);	

	validData <= lastBitRcv AND goodParity;

	dataReg:
	PROCESS (rst, clk)
	BEGIN
		IF (rst='0') THEN
			data <= (OTHERS=>'0');
		ELSIF (clk'EVENT AND clk='1') THEN
			IF (ldData='1') THEN
				data <= ps2DataRegOut(8 downto 1);
			END IF;
		END IF;
	END PROCESS dataReg;

	controller:
	PROCESS (validData, rst, clk)
		TYPE states IS (waitingData, waitingNewDataAck); 
		VARIABLE state: states;
	BEGIN
		ldData <= '0';
		newData <= '0';
		
		CASE state IS
			WHEN waitingData =>
				IF (validData='1') THEN
					ldData <= '1';
				END IF;
			WHEN waitingNewDataAck =>
				newData <= '1';
			WHEN OTHERS => NULL;
		END CASE;
		
		IF (rst='0') THEN
			state := waitingData;
		ELSIF (clk'EVENT AND clk='1') THEN
				CASE state IS
					WHEN waitingData =>
						IF (validData='1') THEN
							state := waitingNewDataAck;
						END IF;
					WHEN waitingNewDataAck =>
						IF (newDataAck='1') THEN
							state := waitingData;
						END IF;
					WHEN OTHERS => NULL;
				END CASE;
		END IF;
	END PROCESS controller;
	
	
	-- MAQUINA DE ESTADOS PARA DETECCION DE TECLAS (TECLADO PS/2)
	-- *******************************************************************************************

	-- MAQUINA ESTADOS: SINCRONO
	maqEstadosSyn: 
    process(clk,rst)
      begin
        if rst ='0' then
            ESTADO <= WAITING_PRESS;
          elsif clk'event and clk='1' then
            ESTADO <= SIG_ESTADO;
        end if;
    end process;
	 
	-- MAQUINA ESTADOS: COMBINACIONAL
	maqEstadosComb:
    process(ESTADO,rst,newData,data,scanCode)
      begin
		  
		  SIG_ESTADO <= ESTADO;
		  loadScanCode <= '0';
		  clearScanCode <= '0';
		  
        case ESTADO is
			 when WAITING_PRESS => 

			 		newDataAck <= '1';
					
					if newData = '1' then
						if data /= "11110000" then
							loadScanCode <= '1';
							SIG_ESTADO <= WAITING_F0;
						else
							SIG_ESTADO <= IGNORE_RELEASE;
						end if;
					end if;

			 when WAITING_F0 => 
			 
					newDataAck <= '1';
					
					if newData = '1' then
						if data = "11110000" then
							SIG_ESTADO <= WAITING_RELEASE;
						end if;
					end if;
						
						
			 when WAITING_RELEASE => 
			 			
					newDataAck <= '1';

					if newData = '1' then
						if scanCode = data then
							clearScanCode <= '1';
							SIG_ESTADO <= WAITING_PRESS;
						else
							SIG_ESTADO <= WAITING_F0;
						end if;
					end if;
					
			 when IGNORE_RELEASE => 
			 			
					newDataAck <= '1';

					if newData = '1' then
						SIG_ESTADO <= WAITING_PRESS;
					end if;
					
        end case;
      end process;   
	
	-- REGISTRO SCANCODE
	-- *******************************************************************************************

	registroScanCode:
	process( clk, rst , loadScanCode, clearScanCode)	
	begin
		scanCode <= regOut;
		if rst = '0' then
			regOut <= (others=>'0');
		elsif clk'event and clk = '1' then
			if loadScanCode = '1' then
				regOut <= data;
			elsif clearScanCode = '1' then
					regOut <= (others=>'0');
			end if;
		end if;
	end process;
	
end Behavioral;
