-------------------------------------------------------------------
--
--  Fichero:
--    damero.vhd  12/7/2013
--
--    (c) J.M. Mendias
--    Diseño Automático de Sistemas
--    Facultad de Informática. Universidad Complutense de Madrid
--
--  Propósito:
--    Muestra un damero sobre un monitor compatible VGA 
--
--  Notas de diseño:
--    La sincronización con la pantalla VGA presupone que la 
--    frecuencia de reloj del sistema es de 50 MHz 
--
-------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
USE IEEE.std_logic_1164.ALL;
USE IEEE.std_logic_unsigned.ALL;
USE IEEE.std_logic_arith.ALL;

ENTITY pong IS
  PORT (
    rst: IN std_logic;
    clk: IN std_logic;
	ps2Data : IN std_logic;
	ps2Clk : IN std_logic;
	 
    hSyncQ: OUT std_logic;
    vSyncQ: OUT std_logic;
    RGBQ: OUT std_logic_vector(8 DOWNTO 0);
	
	altavoz: OUT std_logic
  );
END pong;

ARCHITECTURE pongArch OF pong IS

	signal pixelCntOut: std_logic_vector(10 downto 0);
	signal lineCntOut: std_logic_vector(9 downto 0);
	signal blanking, valor: std_logic;
	
	-- SEÑALES PARA PINTAR
	signal bordeArriba, bordeAbajo,redCentral: std_logic;
	signal raquetaIzq,raquetaDcha, pelota : std_logic;
	signal hSync, vSync : std_logic;
	signal RGB : std_logic_vector(8 downto 0);
  	
	-- CONTADORES: RAQUETAS Y PELOTAS
	signal csIzq, raquetaIzqTam: std_logic_vector(6 downto 0);
	signal csDcha, raquetaDchaTam: std_logic_vector(6 downto 0);
	
	-- CONTADORES: PELOTA
	signal csPelotaY: std_logic_vector(6 downto 0);
	signal csPelotaX: std_logic_vector(7 downto 0);
	
	-- RALENTIZADOR
	signal csRalentizador : std_logic_vector(22 downto 0);
	signal mueve : std_logic;
	
	-- VARIABLES DE JUEGO
	signal hayGanador, partidaEnCurso, centrarPelota, centrarPelota_sig, partidaEnCurso_sig: std_logic;
	
	-- VARIABLES SONIDO REBOTAR
	signal csOscilador, csCiclosSonido : std_logic_vector(22 downto 0);
	signal musica, sonido, sonidoRebotar : std_logic;
	
  	-- INTERFAZ TECLADO PS/2
	signal data : std_logic_vector (7 DOWNTO 0);  	-- Salida de datos paralela
	signal newData : std_logic;                   	-- Indica la recepción de un nuevo dato por la línea PS2  
	signal newDataAck : std_logic;                  -- Reconoce la recepción del nuevo dato
	signal ldData, validData, lastBitRcv, ps2ClkSync, ps2ClkFallingEdge: std_logic;
	signal ps2DataRegOut: std_logic_vector(10 downto 0);
	signal goodParity: std_logic;
	
	-- FLAGS DE TECLAS PULSADAS
	signal flagQ,flagA,flagP,flagL,flagSPC : std_logic;
	signal flagQnext,flagAnext,flagPnext,flagLnext,flagSPCnext : std_logic;	
	
	-- MAQUINA DE ESTADOS PARA EL CONTROL DE TECLAS
	type ESTADOS is (WAITING_PRESS, RELEASE_BUTTON);
	signal ESTADO, SIG_ESTADO: ESTADOS;
	
	-- MAQUINA DE ESTADOS PARA EL CONTROL DEL JUEGO
	type GAME_STATES is (WAITING_SPACE, INITIALIZING_GAME, WAITING_WINNER);
	signal GAME, NEXT_GAME: GAME_STATES;

BEGIN

  pixelCnt:
  PROCESS( rst, clk )
  BEGIN
    IF (rst='0') THEN
      pixelCntOut <= (OTHERS=>'0');
    ELSIF(clk'EVENT AND clk='1') THEN
      IF (pixelCntOut=1588) THEN
        pixelCntOut <= (OTHERS=>'0');
      ELSE
        pixelCntOut <= pixelCntOut+1;
      END IF;
    END IF;
  END PROCESS pixelCnt;

  lineCnt:
  PROCESS( rst, clk )
  BEGIN
    IF (rst='0') THEN
      lineCntOut <= (OTHERS=>'0');
    ELSIF (clk'EVENT AND clk='1') THEN
      IF (pixelCntOut=1588) THEN
        IF (lineCntOut=527) THEN
          lineCntOut <= (others=>'0');
        ELSE
          lineCntOut <= lineCntOut+1;
        END IF;
      END IF;
    END IF;
  END PROCESS lineCnt;
  
  hSync <= '0' WHEN (pixelCntOut > 1304) AND (pixelCntOut <= 1493) ELSE '1';
  vSync <= '0' WHEN (lineCntOut > 493) AND (lineCntOut <= 495) ELSE '1';   
  
  blanking <= '1' WHEN (pixelCntOut > 1257) OR (lineCntOut > 479) ELSE '0';

  RGB <= valor & valor & valor & valor & valor & valor & valor & valor & valor;
  
  -------------------------------------------------------------------------------------
  -- 		INTRODUCIR CODIGO DESDE AQUI
  --  	|		|		|		|		|
  -- 	v		v		v		v		v
  
  -- COMO VA EL LINECOUNT PARA HACER LINEAS MAS GORDAS:
  
  -- 0100
  -- 0101
  -- 0110
  -- 0111
  -- 1100	-> Hay que fijarse cuales son los bits comunes para ir
  -- 1101		diviendo entre 2. (Con esto, son numeros modulo 2, 4, 8 .. etc)
  -- 1110
  -- 1111
  
	-- PINTAR EL CAMPO DE JUEGO:
	-- *******************************************************************************
	  
	-- ! : Si la linea a pintar es la linea 8, pondremos 7 porque empieza a contar desde el 0
	  
	-- El borde de arriba es de 3 pix de anchura y en la linea 8
	bordeArriba <= '1' WHEN (lineCntOut(9 downto 2) = 7) ELSE '0';
	  
	-- El borde de abajo es de 3 px de anchura y esta en la linea 111 = 8 + 1 + 102 + 1
	bordeAbajo <= '1' WHEN (lineCntOut(9 downto 2) = 111) ELSE '0';
	  
	-- La red central se pinta en la columna 75 = (8 + 1 + 67) y para el tamaño de alternar (es discontinua) usamos el 5 bit del lineCount. 
	-- Despues aseguramos que se pinta entre los bordes arriba y abajo
	redCentral <= '1' WHEN (pixelCntOut(10 downto 3) = 75) AND (lineCntOut(5) = '1') AND (lineCntOut(9 downto 2) > 7) AND (lineCntOut(9 downto 2) < 111) ELSE '0';
	  
	-- PINTAR RAQUETAS
	-- *******************************************************************************
  
  	contRaquetaIzq:
	process( clk, rst , flagQ, flagA, csIzq, mueve)
	begin
		if rst = '0' then
			csIzq <= conv_std_logic_vector( 51 , 7 );		-- Situar la raqueta centrada
		elsif clk'event and clk = '1' then
			if mueve = '1' then
				if flagQ = '1' and csIzq > 7  then
					csIzq <= csIzq - 1;
				elsif flagA = '1' and csIzq < 95 then
						csIzq <= csIzq + 1;
				end if;
			end if;
		end if;
	end process;
	
	-- La raqueta izquierda tiene un tamaño de 16 px y esta en la columna 9. Las lineas se pintan entre el valor del contador y el del tamaño
	raquetaIzqTam <= csIzq + 16;
	raquetaIzq <= '1' WHEN (pixelCntOut(10 downto 3) = 8) AND (lineCntOut(9 downto 2) > csIzq) AND (lineCntOut(9 downto 2) < raquetaIzqTam) ELSE '0';

   contRaquetaDcha:
	process( clk, rst, flagP, flagL, csDcha, mueve)
	begin
		if rst = '0' then
			csDcha <= conv_std_logic_vector( 51 , 7 );		-- Situar la raqueta centrada
		elsif clk'event and clk = '1' then
			if mueve = '1' then
				if flagP = '1' and csDcha > 7  then
					csDcha <= csDcha - 1;
				elsif flagL = '1' and csDcha < 95 then
						csDcha <= csDcha + 1;
				end if;
			end if;
		end if;
	end process;
	
	-- La raqueta derecha tiene un tamaño de 16 px y esta en la columna 145. Las lineas se pintan entre el valor del contador y el del tamaño
	raquetaDchaTam <= csDcha + 16;
	raquetaDcha <= '1' WHEN (pixelCntOut(10 downto 3) = 144) AND (lineCntOut(9 downto 2) > csDcha) AND (lineCntOut(9 downto 2) < raquetaDchaTam) ELSE '0';
	
	-- PINTAR PELOTA
	-- *******************************************************************************
  
  	pelotaEjeY:
	process( clk, rst , csPelotaY, mueve, centrarPelota)
		variable arriba : std_logic;
	begin
		if rst = '0' then
			csPelotaY <= conv_std_logic_vector( 59 , 7 );	-- Centro de la pantalla (7 bits)
			arriba := '1';
		elsif clk'event and clk='1' then

			if centrarPelota = '1' then
				csPelotaY <= conv_std_logic_vector( 59 , 7 );
			elsif mueve = '1' then
				
				-- Rebotar si choca contra los bordes
				if csPelotaY = 7 or csPelotaY = 111 then
					arriba := not arriba;
				end if;
				
				-- Mover la pelota
				if arriba = '1' then
					csPelotaY <= csPelotaY - 1;
				else
					csPelotaY <= csPelotaY + 1;
				end if;
				
			end if;
		end if;
	end process;
	
	pelotaEjeX:
	process( clk, rst, csPelotaX, mueve, centrarPelota)
		variable izquierda : std_logic;
	begin
		
		if rst = '0' then
			csPelotaX <= conv_std_logic_vector( 76 , 8 );	-- Cento de la pantalla (8 Bits)
			izquierda := '1';
			hayGanador <= '0';
		elsif clk'event and clk='1' then

			if centrarPelota = '1' then
				csPelotaX <= conv_std_logic_vector( 76 , 8 );
				hayGanador <= '0';
			elsif mueve = '1' then
			
				-- Rebotar si choca contra la raqueta izquierda o la derecha
				if (csPelotaX = 8 and csIzq < csPelotaY and csPelotaY < raquetaIzqTam) or (csPelotaX = 144 and csDcha < csPelotaY and csPelotaY < raquetaDchaTam) then
					izquierda := not izquierda;
					sonidoRebotar <= '1';
				else
					sonidoRebotar <= '0';
				end if;
				
				-- Si se salio del campo es que marcaron un tanto
				if (csPelotaX = 1 or csPelotaX = 152) then
					hayGanador <= '1';
				end if;

				-- Mover la pelota
				if izquierda = '1' then
					csPelotaX <= csPelotaX - 1;
				else
					csPelotaX <= csPelotaX + 1;
				end if;
			end if;
		end if;
	end process;
	
	-- La pelota se pinta donde marquen los contadores de los ejes X e Y
	pelota <= '1' WHEN (pixelCntOut(10 downto 3) = csPelotaX) AND (lineCntOut(9 downto 2) = csPelotaY) ELSE '0';
	
	-- RALENTIZADOR DE MOVIMIENTO: Evita que las raquetas o la pelota se muevan demasiado deprisa por la pantalla
	-- *******************************************************************************
	
	ralentizador:
	process( clk, rst, csRalentizador, partidaEnCurso)	
	begin
		if rst = '0' then
			mueve <= '0';
			csRalentizador <= conv_std_logic_vector( 0, 23 );
		elsif clk'event and clk = '1' then
			if partidaEnCurso = '1' then
				if csRalentizador = 1000000 then
					mueve <= '1';
					csRalentizador <= conv_std_logic_vector( 0, 23 );
				else
					csRalentizador <= csRalentizador + 1;
					mueve <= '0';
				end if;
			end if;
		end if;
	end process;
	
	
	-- SONIDO REBOTAR
	-- *******************************************************************************
	
	-- Oscilador que continuamente emite un DO
	oscilador:
	process( clk, rst )	
	begin
		if rst = '0' then
			csOscilador <= conv_std_logic_vector( 0 , 17 );
			musica <= '0';
		elsif clk'event and clk = '1' then
			if csOscilador = "10111010101001110" then
				csOscilador <= conv_std_logic_vector( 0 , 17 );
				musica <= not (musica);
			elsif csOscilador = conv_std_logic_vector( 95566 , 17 ) then
					csOscilador <= conv_std_logic_vector( 0 , 17 );
				else
					csOscilador <= csOscilador + 1;
			end if;
		end if;
	end process;
	
	-- El sonido de rebotar debe sonar durante un breve tiempo
	ciclosSonidoReboteCnt:
	process( clk, rst, csCiclosSonido, sonidoRebotar)	
	begin
		if rst = '0' then
			sonido <= '0';
			csCiclosSonido <= conv_std_logic_vector( 0, 23 );
		elsif clk'event and clk = '1' then
		
			if sonidoRebotar = '1' then
				csCiclosSonido <= conv_std_logic_vector( 0, 23 );
			elsif csCiclosSonido = 5000000 then
					sonido <= '0';
				else
					csCiclosSonido <= csCiclosSonido + 1;
					sonido <= '1';
				end if;
			
		end if;
	end process;
	
	altavoz <= musica AND sonido;
	
	-- RESULTADO FINAL EN MONITOR VGA
	-- *******************************************************************************
  
	valor <= (bordeArriba OR bordeAbajo OR redCentral OR raquetaIzq OR raquetaDcha OR pelota) AND NOT blanking;
  
   process(clk)
	begin
		if rst = '0' then
			hSyncQ <= '0';
			vSyncQ <= '0';
			RGBQ <= "000000000";
		elsif clk'event and clk = '1' then
			hSyncQ <= hSync;
			vSyncQ <= vSync;
			RGBQ <= RGB;
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
	maqestadosSyn: 
    process(clk,rst)
      begin
        if rst ='0' then
            
				flagQ <= '0';
				flagA <= '0';
				flagP <= '0';
				flagL <= '0';
				flagSPC <= '0';
				
				ESTADO <= WAITING_PRESS;
				
          elsif clk'event and clk='1' then
        
				flagQ <= flagQnext;
				flagA <= flagAnext;
				flagP <= flagPnext;
				flagL <= flagLnext;
				flagSPC <= flagSPCnext;
				
				ESTADO <= SIG_ESTADO;
				
        end if;
    end process;
	 
	-- MAQUINA ESTADOS: COMBINACIONAL
	maqestadosComb:
    process(ESTADO,rst,newData,data)
	  begin
		  
		flagQnext <= flagQ;
		flagAnext <= flagA;
		flagPnext <= flagP;
		flagLnext <= flagL;
		flagSPCnext <= flagSPC;
		  
		SIG_ESTADO <= ESTADO;
		  
        case ESTADO is
			 when WAITING_PRESS => 

			 		newDataAck <= '1';
					
					if newData = '1' then
							
						case data is
							when "11110000" => SIG_ESTADO <= RELEASE_BUTTON;	-- Si es F0, es una liberacion de tecla
							when "00010101" => flagQnext <= '1';				-- Si es Q = 15 (hex), activamos flag de Q
							when "00011100" => flagAnext <= '1';				-- Si es A = 1C (hex), activamos flag de A				
							when "01001101" => flagPnext <= '1';				-- Si es P = 4D (hex), activamos flag de P
							when "01001011" => flagLnext <= '1';				-- Si es L = 4B (hex), activamos flag de L		
							when "00101001" => flagSPCnext <= '1';				-- Si es SPACE = 29 (hex), activamos flag de SPACE	
							when others => SIG_ESTADO <= WAITING_PRESS;
						end case;
						
					end if;
						
			 when RELEASE_BUTTON => 
			 			
					newDataAck <= '1';

					if newData = '1' then
						
						case data is
							when "00010101" => flagQnext <= '0';				-- Si es Q = 15 (hex), desactivamos flag de Q
							when "00011100" => flagAnext <= '0';				-- Si es A = 1C (hex), desactivamos flag de A				
							when "01001101" => flagPnext <= '0';				-- Si es P = 4D (hex), desactivamos flag de P
							when "01001011" => flagLnext <= '0';				-- Si es L = 4B (hex), desactivamos flag de L		
							when "00101001" => flagSPCnext <= '0';				-- Si es SPACE = 29 (hex), desactivamos flag de SPACE	
							when others => SIG_ESTADO <= WAITING_PRESS;
						end case;
							
						SIG_ESTADO <= WAITING_PRESS;
							
					end if;
					
        end case;
      end process;   
  
  
  
  	-- MAQUINA DE ESTADOS PARA EL JUEGO
	-- *******************************************************************************************
	
	-- MAQUINA ESTADOS: SINCRONO
	maqEstadosJuegoSyn: 
    process(clk,rst)
      begin
        if rst ='0' then           
				--hayGanador <= '0';	
				GAME <= WAITING_SPACE;
				centrarPelota <= '0';
				partidaEnCurso <= '0';				
          elsif clk'event and clk='1' then	
			 
				GAME <= NEXT_GAME;	
				partidaEnCurso <= partidaEnCurso_sig;
				centrarPelota <= centrarPelota_sig;
				
        end if;
    end process;
	 
	-- MAQUINA ESTADOS: COMBINACIONAL
	maqEstadosJuegoComb:
    process(GAME,rst,newData,data,flagSPC,hayGanador)
	  begin
		  
		NEXT_GAME <= GAME;
		partidaEnCurso_sig <= partidaEnCurso;
		centrarPelota_sig <= centrarPelota;
		  
        case GAME is
			 when WAITING_SPACE => 
					
					partidaEnCurso_sig <= '0';
					centrarPelota_sig <= '0';
					
					if flagSPC = '1' then
						NEXT_GAME <= INITIALIZING_GAME;
					end if;
					
			 when INITIALIZING_GAME => 
			 
					--partidaEnCurso_sig <= '0';
					centrarPelota_sig <= '1';
					
					NEXT_GAME <= WAITING_WINNER;
						
			 when WAITING_WINNER => 
			 			
					partidaEnCurso_sig <= '1';
					centrarPelota_sig  <= '0';
					
					if hayGanador = '1'then
						NEXT_GAME <= WAITING_SPACE;
					end if;
					
        end case;
      end process;   
  
  
  
  
END pongArch;
