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

library UNISIM;
use UNISIM.vcomponents.all;
 

ENTITY tron IS
  PORT (
    rst: IN std_logic;
    clk: IN std_logic;
	 ps2Data : IN std_logic;
	 ps2Clk : IN std_logic;
	 
    hSyncQ : OUT std_logic;
    vSyncQ : OUT std_logic;
    RGBQ : OUT std_logic_vector(8 DOWNTO 0)
  );
END tron;

ARCHITECTURE tronArch OF tron IS
	
	-- MEMORIA DE REFRESCO
	component RAMB16_S1_s1
		generic(
			WRITE_MODE_A : string := "READ_FIRST";
			WRITE_MODE_B : string := "READ_FIRST"
		);
		port(
			DOA0: out std_logic;								-- Salida de datos 
			DOB0: out std_logic;							
			ADDRA: in std_logic_vector(13 downto 0);	-- Direccion
			ADDRB: in std_logic_vector(13 downto 0);
			CLKA: in std_ulogic;								-- Reloj
			CLKB: in std_ulogic;
			DIA0: in std_logic;								-- Entrada de datos
			DIB0: in std_logic;
			ENA: in std_ulogic;								-- Entrada capacitacion
			ENB: in std_ulogic;
			SSRA: in std_ulogic;								-- Inicializacion sincrona para los latches de salida 
			SSRB: in std_ulogic;
			WEA: in std_ulogic;								-- Entrada capacitacion escritura
			WEB: in std_ulogic
			);
	end component;
	
	-- SEÑALES VGA
	signal pixelCntOut: std_logic_vector(10 downto 0);
	signal lineCntOut: std_logic_vector(9 downto 0);
	signal blanking, valor: std_logic;
	
	-- SEÑALES PARA PINTAR
	signal hSync, vSync : std_logic;
	signal RGB : std_logic_vector(8 downto 0);
	signal salidaRojo, salidaAzul : std_logic;
	signal motoAzul, estelaAzul, motoRoja, estelaRoja : std_logic;
	
	-- MEMORIA DE REFRESCO
	signal dirRefrescoVGA, dirEscrituraAzul, dirTrayectoAzul, dirEscrituraRoja, dirTrayectoRoja: std_logic_vector(14 downto 0);
	signal readFirstAzul1,DOB0,readFirstAzul2,DOB1,readFirstRoja1,DOB2,readFirstRoja2,DOB3 : std_logic;
	signal valorEscritura , enableEscritura: std_logic;
	
	-- LIMPIEZA DE LA MEMORIA
	signal limpiarMemoria,limpiezaCompletada : std_logic;	
	signal csCiclosLimpieza: std_logic_vector(22 downto 0);
	
	-- CONTADORES MOTOS: PELOTA
	signal csMotoAzulY: std_logic_vector(6 downto 0);	--	 7 bits
	signal csMotoAzulX: std_logic_vector(7 downto 0);	--  8 bits
	signal csMotoRojaY: std_logic_vector(6 downto 0);
	signal csMotoRojaX: std_logic_vector(7 downto 0);
	
	-- CONTROL DIRECCION DE LAS MOTOS
	type DIR_MOTO is (ARRIBA, ABAJO, IZQUIERDA, DERECHA);
	signal dirMotoAzul, dirMotoAzulNext, dirMotoRoja, dirMotoRojaNext: DIR_MOTO;
	
	-- RALENTIZADOR
	signal csRalentizador : std_logic_vector(22 downto 0);
	signal mueve : std_logic;
	
	-- VARIABLES DE JUEGO
	signal hayGanador, partidaEnCurso, iniciarMotos : std_logic;
	signal choqueContrario, choquePropioAzul, choquePropioRoja : std_logic;
	
  	-- INTERFAZ TECLADO PS/2
	signal data : std_logic_vector (7 DOWNTO 0);  	-- Salida de datos paralela
	signal newData : std_logic;                   	-- Indica la recepción de un nuevo dato por la línea PS2  
	signal newDataAck : std_logic;                  -- Reconoce la recepción del nuevo dato
	signal ldData, validData, lastBitRcv, ps2ClkSync, ps2ClkFallingEdge: std_logic;
	signal ps2DataRegOut: std_logic_vector(10 downto 0);
	signal goodParity: std_logic;
	
	-- MAQUINA DE ESTADOS PARA EL CONTROL DE TECLAS
	type ESTADOS is (WAITING_PRESS, RELEASE_BUTTON);
	signal ESTADO, SIG_ESTADO: ESTADOS;
	signal flagSPC, flagSPCnext : std_logic;
	
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

  --RGB <= salidaRojo & salidaRojo & salidaRojo & '0' & '0' & '0' & salidaAzul & salidaAzul & salidaAzul;
  RGB <= salidaAzul & salidaAzul & salidaAzul & '0' & '0' & '0' & salidaRojo & salidaRojo & salidaRojo; 
  
  -------------------------------------------------------------------------------------
  -- 		INTRODUCIR CODIGO DESDE AQUI
  --  	|		|		|		|		|
  -- 		v		v		v		v		v
  
  -- COMO VA EL LINECOUNT PARA HACER LINEAS MAS GORDAS:
  
  -- 0100
  -- 0101
  -- 0110
  -- 0111
  -- 1100	-> Hay que fijarse cuales son los bits comunes para ir
  -- 1101		diviendo entre 2. (Con esto, son numeros modulo 2, 4, 8 .. etc)
  -- 1110
  -- 1111
  
	-- INSTACIAMOS MEMORIA REFRESCO
	-- *******************************************************************************
	
	-- Usar una memoria de doble puerto:
	--		* los coches almacenan su estela por un puerto (úsese también para borrar estelas)
	--		* el refresco se realiza leyendo desde el otro => (Escritura => A y Lectura => B)
	--		* para evitar conflictos de lectura y escritura simultánea configurarla en modo READ_FIRST
	-- Para simplificar la lógica de direccionamiento (a costa de desperdiciar memoria) usar
	-- una de 32Kx2b organizada en 2 slices: uno para el coche rojo y otro para el azul
	--		* la dirección de un pixel se obtiene concatenado parte de lineCnt con parte de pixelCnt
	--		* usar 4 RAM Blocks de 16 K×1b
	
	--		* Usaremos 2 memorias para cada moto para poder direccionar toda la pantalla.
	--		* PxCountMax => 152 px = 8 bits / LineCountMax => 119 px = 7 bits 	==> 15 bits
	--		* Direccionamos hasta la linea 63 en la primera memoria y de la 64 a la 119 en la segunda memoria
	--		* Para distinguir la primera memoria de la segunda usamos bit mas significativo (el de la pos 14 que indica el 64)

	-- Para el refresco de la pantalla, leeremos por el puerto B la direccion resultante de 
	-- concatenar una parte de lineCnt con parte de PixelCnt 
	dirRefrescoVGA <=  lineCntOut(8 downto 2) & pixelCntOut(10 downto 3);	
	
	-- Para guardar en memoria la estela azul, usamos como direccion la concatenacion de la
	-- coordenada Y (lineCnt) y la X (pixelCnt)
	dirTrayectoAzul <= csMotoAzulY & csMotoAzulX;
	dirTrayectoRoja <= csMotoRojaY & csMotoRojaX;
	
	-- MEMORIAS MOTO AZUL
	-- ···········································································		
	
	enableEscritura <= mueve OR limpiarMemoria;
	
  	memAzul_1: RAMB16_S1_s1
		port map(
			readFirstAzul1,					-- Salida de Datos 			(A - Escritura) - Sirve para saber si chocamos contra nuestra propia estela.
			DOB0,									-- Salida de Datos 			(B - Lectura)
			dirEscrituraAzul(13 downto 0),	-- Direccion de escritura 	(A): Durante el juego es la posicion de la moto y durante la limpieza el Px/ln count
			dirRefrescoVGA(13 downto 0),	-- Direccion de lectura 	(B)
			clk, 									-- Mismo reloj para ambos puertos
			clk,
			valorEscritura,					-- Sera 1 durante el juego y 0 durante la limpieza de la memoria
			'0',									-- Nunca escribimos por el puerto B, asi que ponemos 0 como valor Din por poner algo
			not dirEscrituraAzul(14),		-- ENABLE A: El modulo de escritura estara activo cuando el lineCnt sea menor que 64
			not dirRefrescoVGA(14),			-- ENABLE B: El modulo de lectura estara activo cuando el lineCnt sea menor que 64
			'0', 									-- Desactivamos la limpieza de los latches de salida
			'0',
			enableEscritura,			-- Escritura habilitada en puerto A cuando se ha realizado movimiento o durante limpieza
			'0'									-- Escritura NO habilitada en puerto B
			);
	
  	memAzul_2: RAMB16_S1_s1
		port map(
			readFirstAzul2,					-- Salida de Datos 			(A - Escritura) - Sirve para saber si chocamos contra nuestra propia estela.
			DOB1,									-- Salida de Datos 			(B - Lectura)
			dirEscrituraAzul(13 downto 0),-- Direccion de escritura 	(A): Durante el juego es la posicion de la moto y durante la limpieza el Px/ln count
			dirRefrescoVGA(13 downto 0),	-- Direccion de lectura 	(B)
			clk, 									-- Mismo reloj para ambos puertos
			clk,
			valorEscritura,					-- Sera 1 durante el juego y 0 durante la limpieza de la memoria
			'0',									-- Nunca escribimos por el puerto B, asi que ponemos 0 como valor Din por poner algo
			dirEscrituraAzul(14),			-- ENABLE A: El modulo de escritura estara activo cuando el lineCnt sea mayor o igual que 64
			dirRefrescoVGA(14),				-- ENABLE B: El modulo de lectura estara activo cuando el lineCnt sea mayor o igual que 64
			'0',									-- Desactivamos la limpieza de los latches de salida
			'0',
			enableEscritura,			-- Escritura habilitada en puerto A cuando se ha realizado movimiento o durante limpieza
			'0'									-- Escritura NO habilitada en puerto B
			);	
			
	estelaAzul <= DOB0 OR DOB1;	
	
	-- MEMORIAS MOTO ROJA
	-- ···········································································		
			
	memRoja_1: RAMB16_S1_s1
		port map(
			readFirstRoja1,					-- Salida de Datos 			(A - Escritura) - Sirve para saber si chocamos contra nuestra propia estela.
			DOB2,									-- Salida de Datos 			(B - Lectura)
			dirEscrituraRoja(13 downto 0),-- Direccion de escritura 	(A): Durante el juego es la posicion de la moto y durante la limpieza el Px/ln count
			dirRefrescoVGA(13 downto 0),	-- Direccion de lectura 	(B)
			clk, 									-- Mismo reloj para ambos puertos
			clk,
			valorEscritura,					-- Sera 1 durante el juego y 0 durante la limpieza de la memoria
			'0',									-- Nunca escribimos por el puerto B, asi que ponemos 0 como valor Din por poner algo
			not dirEscrituraRoja(14),		-- ENABLE A: El modulo de escritura estara activo cuando el lineCnt sea menor que 64
			not dirRefrescoVGA(14),			-- ENABLE B: El modulo de lectura estara activo cuando el lineCnt sea menor que 64
			'0', 									-- Desactivamos la limpieza de los latches de salida
			'0',
			enableEscritura,			-- Escritura habilitada en puerto A cuando se ha realizado movimiento o durante limpieza
			'0'									-- Escritura NO habilitada en puerto B
			);
	
		
  	memRoja_2: RAMB16_S1_s1
		port map(
			readFirstRoja2,					-- Salida de Datos 			(A - Escritura) - Sirve para saber si chocamos contra nuestra propia estela.
			DOB3,									-- Salida de Datos 			(B - Lectura)
			dirEscrituraRoja(13 downto 0),-- Direccion de escritura 	(A): Durante el juego es la posicion de la moto y durante la limpieza el Px/ln count
			dirRefrescoVGA(13 downto 0),	-- Direccion de lectura 	(B)
			clk, 									-- Mismo reloj para ambos puertos
			clk,
			valorEscritura,					-- Sera 1 durante el juego y 0 durante la limpieza de la memoria
			'0',									-- Nunca escribimos por el puerto B, asi que ponemos 0 como valor Din por poner algo
			dirEscrituraRoja(14),			-- ENABLE A: El modulo de escritura estara activo cuando el lineCnt sea mayor o igual que 64
			dirRefrescoVGA(14),				-- ENABLE B: El modulo de lectura estara activo cuando el lineCnt sea mayor o igual que 64
			'0',									-- Desactivamos la limpieza de los latches de salida
			'0',
			enableEscritura,			-- Escritura habilitada en puerto A cuando se ha realizado movimiento o durante limpieza
			'0'									-- Escritura NO habilitada en puerto B
			);	
	
	estelaRoja <= DOB2 OR DOB3;

	-- LIMPIEZA DE LA MEMORIA: Contador para esperar durante la limpieza. Cuando el contador finalice la memoria estará limpia.
	-- *******************************************************************************
	
	ciclosLimpiezaCnt:
	process( clk, rst, csCiclosLimpieza, limpiarMemoria)	
	begin
		if rst = '0' then
			limpiezaCompletada <= '0';
			csCiclosLimpieza <= (others => '0');
		elsif clk'event and clk = '1' then
			if limpiarMemoria = '1' then
				if csCiclosLimpieza = 5000000 then
					limpiezaCompletada <= '1';
				else
					csCiclosLimpieza <= csCiclosLimpieza + 1;
					limpiezaCompletada <= '0';
				end if;
			else
				csCiclosLimpieza <= (others => '0');
				limpiezaCompletada <= '0';
			end if;	
		end if;
	end process;
	
	-- Durante la limpieza la direccion de escritura es la de refrescoVGA para limpiar (valor 0) toda la pantalla
	-- Durante el juego la direccion de escritura es la de los contadores de posicion (valor 1) de las motos.
	
	dirEscrituraAzul <= dirRefrescoVGA WHEN limpiarMemoria = '1' ELSE dirTrayectoAzul;
	dirEscrituraRoja <= dirRefrescoVGA WHEN limpiarMemoria = '1' ELSE dirTrayectoRoja;
	valorEscritura <= '0' WHEN limpiarMemoria = '1' ELSE '1';
	
	-- DETECTOR DE COLISIONES
	-- *******************************************************************************
  
	choqueContrario <= '1' WHEN (DOB0 = '1' AND DOB2 = '1') OR (DOB1 = '1' AND DOB3 = '1') ELSE '0';
	
	choquePropioRoja <= '1' WHEN ((readFirstRoja1 = '1' AND dirEscrituraRoja(14) = '0') OR 
								 (readFirstRoja2 = '1' AND dirEscrituraRoja(14) = '1')) AND 
								 csRalentizador = 0 ELSE '0';
	choquePropioAzul <= '1' WHEN ((readFirstAzul1 = '1' AND dirEscrituraAzul(14) = '0') OR 
								 (readFirstAzul2 = '1' AND dirEscrituraAzul(14) = '1')) AND 
								 csRalentizador = 0 ELSE '0';
								 
	hayGanador <= choqueContrario OR choquePropioRoja OR choquePropioAzul;							 

	-- PINTAR MOTO AZUL
	-- *******************************************************************************
  
  	motoAzulY:
	process( clk, rst, csMotoAzulY, mueve, dirMotoAzul, iniciarMotos)
	begin
		if rst = '0' then
			csMotoAzulY <= conv_std_logic_vector( 111 , 7 );	-- Abajo de la pantalla (7 bits)
		elsif clk'event and clk='1' then

			if iniciarMotos = '1' then
				csMotoAzulY <= conv_std_logic_vector( 111 , 7 );
			elsif mueve = '1' then
			
							-- Ajustar conteo en funcion de la direccion
				if dirMotoAzul = ARRIBA then
					csMotoAzulY <= csMotoAzulY - 1;
				elsif dirMotoAzul = ABAJO then
					  csMotoAzulY <= csMotoAzulY + 1;
				end if;
			
				-- Si se sale por el borde de arriba (0 px) o el de abajo (119 px) aparecer por el contrario
				if csMotoAzulY = 0 and dirMotoAzul = ARRIBA then
					csMotoAzulY <= conv_std_logic_vector( 119 , 7 );
				elsif csMotoAzulY = 119 and dirMotoAzul = ABAJO then
					  csMotoAzulY <= conv_std_logic_vector( 0 , 7 );
				end if;
				

		
				
			end if;
		end if;
	end process;
	
	motoAzulX:
	process( clk, rst , csMotoAzulX, mueve, dirMotoAzul, iniciarMotos)
	begin
		
		if rst = '0' then
			csMotoAzulX <= conv_std_logic_vector( 152 , 8 );	
		elsif clk'event and clk='1' then

			if iniciarMotos = '1' then
				csMotoAzulX <= conv_std_logic_vector( 152 , 8 );
			elsif mueve = '1' then
			
			
							-- Ajustar conteo en funcion de la direccion
				if dirMotoAzul = IZQUIERDA then
					csMotoAzulX <= csMotoAzulX - 1;
				elsif dirMotoAzul = DERECHA then
					  csMotoAzulX <= csMotoAzulX + 1;
				end if;
				
				-- Si se sale por el borde de arriba (0 px) o el de abajo (119 px) aparecer por el contrario
				if csMotoAzulX = 0 and dirMotoAzul = IZQUIERDA then
					csMotoAzulX <= conv_std_logic_vector( 152 , 8 );
				elsif csMotoAzulX = 152 and dirMotoAzul = DERECHA then
					  csMotoAzulX <= conv_std_logic_vector( 0 , 8 );
				end if;
				

				
			end if;
		end if;
	end process;
	
	-- La moto azul se pinta donde marquen los contadores de los ejes X e Y
	motoAzul <= '1' WHEN (pixelCntOut(10 downto 3) > csMotoAzulX-2) AND (pixelCntOut(10 downto 3) < csMotoAzulX+2) AND
								(lineCntOut(8 downto 2) > csMotoAzulY-2) AND (lineCntOut(8 downto 2) < csMotoAzulY+2) ELSE '0';
	
	
	-- PINTAR MOTO ROJA
	-- *******************************************************************************
  
  	motoRojaY:
	process( clk, rst , mueve, csMotoRojaY, mueve, dirMotoRoja, iniciarMotos)
	begin
		if rst = '0' then
			csMotoRojaY <= conv_std_logic_vector( 7, 7 );	
		elsif clk'event and clk='1' then

			if iniciarMotos = '1' then
				csMotoRojaY <= conv_std_logic_vector( 7 , 7 );
			elsif mueve = '1' then
				
								-- Ajustar conteo en funcion de la direccion
				if dirMotoRoja = ARRIBA then
					csMotoRojaY <= csMotoRojaY - 1;
				elsif dirMotoRoja = ABAJO then
					  csMotoRojaY <= csMotoRojaY + 1;
				end if;
				
				-- Si se sale por el borde de arriba (0 px) o el de abajo (119 px) aparecer por el contrario
				if csMotoRojaY = 0 and dirMotoRoja = ARRIBA then
					csMotoRojaY <= conv_std_logic_vector( 119 , 7 );
				elsif csMotoRojaY = 119 and dirMotoRoja = ABAJO then
					  csMotoRojaY <= conv_std_logic_vector( 0 , 7 );
				end if;
				

		
				
			end if;
		end if;
	end process;
	
	motoRojaX:
	process( clk, rst , csMotoRojaX, mueve, dirMotoRoja, iniciarMotos)
	begin
		
		if rst = '0' then
			csMotoRojaX <= conv_std_logic_vector( 2 , 8 );	
		elsif clk'event and clk='1' then

			if iniciarMotos = '1' then
				csMotoRojaX <= conv_std_logic_vector( 2 , 8 );
			elsif mueve = '1' then
			
							-- Ajustar conteo en funcion de la direccion
				if dirMotoRoja = IZQUIERDA then
					csMotoRojaX <= csMotoRojaX - 1;
				elsif dirMotoRoja = DERECHA then
					  csMotoRojaX <= csMotoRojaX + 1;
				end if;
			
				-- Si se sale por el borde de arriba (0 px) o el de abajo (119 px) aparecer por el contrario
				if csMotoRojaX = 0 and dirMotoRoja = IZQUIERDA then
					csMotoRojaX <= conv_std_logic_vector( 152 , 8 );
				elsif csMotoRojaX = 152 and dirMotoRoja = DERECHA then
					  csMotoRojaX <= conv_std_logic_vector( 0 , 8 );
				end if;
				 

				
			end if;
		end if;
	end process;
	
	-- La moto roja se pinta donde marquen los contadores de los ejes X e Y
	motoRoja <= '1' WHEN (pixelCntOut(10 downto 3) > csMotoRojaX-2) AND (pixelCntOut(10 downto 3) < csMotoRojaX+2) AND
							(lineCntOut(8 downto 2) > csMotoRojaY-2) AND (lineCntOut(8 downto 2) < csMotoRojaY+2) ELSE '0';
	
	-- RALENTIZADOR DE MOVIMIENTO: Evita que las motos se muevan demasiado deprisa
	-- *******************************************************************************
	
	ralentizador:
	process( clk, rst, csRalentizador, partidaEnCurso)	
	begin
		if rst = '0' then
			mueve <= '0';
			csRalentizador <= conv_std_logic_vector( 0, 23 );
		elsif clk'event and clk = '1' then
			if partidaEnCurso = '1' then
				if csRalentizador = 2000000 then
					mueve <= '1';
					csRalentizador <= conv_std_logic_vector( 0, 23 );
				else
					csRalentizador <= csRalentizador + 1;
					mueve <= '0';
				end if;
			end if;
		end if;
	end process;
	
	-- RESULTADO FINAL EN MONITOR VGA
	-- *******************************************************************************
  
	valor <= NOT blanking;
	salidaRojo <= (motoRoja OR estelaRoja) AND NOT blanking;
	salidaAzul <= (motoAzul OR estelaAzul) AND NOT blanking;
	
   -- Este biestable evita los glitches en la salida VGA
   biestableVGASync:
   process(rst, clk)
	begin
		if rst = '0' then
			hSyncQ <= '0';
			vSyncQ <= '0';
			RGBQ <= valor & valor & valor & valor & valor & valor & valor & valor & valor;
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
	maqEstadosTecladoSyn: 
    process(clk,rst)
      begin
        if rst ='0' then
            
				dirMotoAzul <= IZQUIERDA;
				dirMotoRoja <= DERECHA;
				flagSPC <= '0';
			
				ESTADO <= WAITING_PRESS;
				
        elsif clk'event and clk='1' then
        
				dirMotoAzul <= dirMotoAzulNext;
				dirMotoRoja <= dirMotoRojaNext;
				flagSPC <= flagSPCnext;
			
				ESTADO <= SIG_ESTADO;
				
        end if;
    end process;
	 
	-- MAQUINA ESTADOS: COMBINACIONAL
	maqEstadosTecladoComb:
    process(ESTADO, newData, data, dirMotoAzul, dirMotoRoja)
	  begin
	 
		dirMotoAzulNext <= dirMotoAzul;
		dirMotoRojaNext <= dirMotoRoja;
		flagSPCnext <= flagSPC;
		
		SIG_ESTADO <= ESTADO;
		  
      case ESTADO is
		
			when WAITING_PRESS => 
				
				newDataAck <= '1';
				
				if newData = '1' then
						
					case data is
					
						-- Si es F0, es una liberacion de tecla 
						when "11110000" => SIG_ESTADO <= RELEASE_BUTTON;	
						
						-- CONTROL MOTO ROJA
						-- ···········································································
						
						-- Si es Q = 15 (hex) y NO va hacia abajo
						when "00010101" => if dirMotoRoja = IZQUIERDA OR dirMotoRoja = DERECHA then
												dirMotoRojaNext <= ARRIBA;
												end if;
												
						-- Si es A = 1C (hex) y NO va hacia arriba						
						when "00011100" => if dirMotoRoja = IZQUIERDA OR dirMotoRoja = DERECHA then
												dirMotoRojaNext <= ABAJO;	
												end if;
												
						-- Si es Z = 1A (hex) y NO va hacia la derecha						
						when "00011010" => if dirMotoRoja = ARRIBA OR dirMotoRoja = ABAJO then
												dirMotoRojaNext <= IZQUIERDA;	
												end if;
												
						-- Si es X = 22 (hex) y NO va hacia izquierda					
						when "00100010" => if dirMotoRoja = ARRIBA OR dirMotoRoja = ABAJO then
												dirMotoRojaNext <= DERECHA;
												end if;
												
						-- CONTROL MOTO AZUL
						-- ···········································································		
						
						-- Si es P = 4D (hex) y NO va hacia abajo					
						when "01001101" => if dirMotoAzul = IZQUIERDA OR dirMotoAzul = DERECHA then
												dirMotoAzulNext <= ARRIBA;	
												end if;
												
						-- Si es L = 4B (hex) y NO va hacia arriba														
						when "01001011" => if dirMotoAzul = IZQUIERDA OR dirMotoAzul = DERECHA then
												dirMotoAzulNext <= ABAJO;	
												end if;
												
						-- Si es N = 31 (hex) y NO va hacia la derecha						
						when "00110001" => if dirMotoAzul = ARRIBA OR dirMotoAzul = ABAJO then
												dirMotoAzulNext <= IZQUIERDA;	
												end if;
												
						-- Si es M = 3A (hex) y NO va hacia izquierda					
						when "00111010" => if dirMotoAzul = ARRIBA OR dirMotoAzul = ABAJO then
												dirMotoAzulNext <= DERECHA;
												end if;
												
						when "00101001" => flagSPCnext <= '1';		-- Si es SPACE = 29 (hex), activamos flag de SPACE	
						
						when others => SIG_ESTADO <= WAITING_PRESS;
						
					end case;
					
				end if;
					
			when RELEASE_BUTTON => 
					
				newDataAck <= '1';
				flagSPCnext <= '0';
				
				-- No hacemos nada; consumimos liberacion de tecla para no confundirla con una pulsacion
				if newData = '1' then	
					-- Si es el SPC y antes de empezar la partida
					if data = "00101001" AND partidaEnCurso = '0' then
						dirMotoAzulNext <= IZQUIERDA;
						dirMotoRojaNext <= DERECHA;
					end if;
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
		
				GAME <= WAITING_SPACE;				
				
          elsif clk'event and clk='1' then	
							
				GAME <= NEXT_GAME;	
				
        end if;
    end process;
	 
	-- MAQUINA ESTADOS: COMBINACIONAL
	maqEstadosJuegoComb:
    process(GAME, flagSPC, hayGanador, limpiezaCompletada)
	  begin
		  
        case GAME is
			 when WAITING_SPACE => 
					
					partidaEnCurso <= '0';
					iniciarMotos <= '0';
					limpiarMemoria <= '0';	
					NEXT_GAME <= WAITING_SPACE;				
					if flagSPC = '1' then
						limpiarMemoria <= '1';
						NEXT_GAME <= INITIALIZING_GAME;
					end if;
					
			 when INITIALIZING_GAME => 
					partidaEnCurso <= '0';					
					iniciarMotos <= '1';
					limpiarMemoria <= '1';
					NEXT_GAME <= INITIALIZING_GAME;	
					
					if limpiezaCompletada = '1' then
						limpiarMemoria <= '0';
						NEXT_GAME <= WAITING_WINNER;
					end if;
					
			 when WAITING_WINNER => 
			 			
					partidaEnCurso <= '1';
					iniciarMotos  <= '0';
					limpiarMemoria <= '0';
					NEXT_GAME <= WAITING_WINNER;	
					
					if hayGanador = '1'then
						NEXT_GAME <= WAITING_SPACE;
					end if;
					
        end case;
      end process;   
  
  
  
  
END tronArch;
