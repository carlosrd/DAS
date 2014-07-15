-------------------------------------------------------------------
--
--  Fichero:
--    debouncer.vhd  12/7/2013
--
--    (c) J.M. Mendias
--    Diseño Automático de Sistemas
--    Facultad de Informática. Universidad Complutense de Madrid
--
--  Propósito:
--    Elimina los rebotes de una línea binaria mediante la espera 
--    de 50 ms tras cada flanco detectado
--
--  Notas de diseño:
--    El timer usado para medir 50 ms esta dimensionado para
--    funcionar con un reloj de 50 MHz
--
-------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.std_logic_unsigned.ALL;

ENTITY debouncer IS
  PORT (
    rst: IN std_logic;                  -- Reset asíncrono del sistema
    clk: IN std_logic;                  -- Reloj del sistema
    x: IN std_logic;                    -- Entrada binaria a la que deben eliminars los rebotes
    xDeb: OUT std_logic;                -- Salida que sique a la entrada pero sin rebotes
    xDebFallingEdge: OUT std_logic;     -- Se activa durante 1 ciclo cada vez que detecta un flanco de subida en x
    xDebRisingEdge: OUT std_logic       -- Se activa durante 1 ciclo cada vez que detecta un flanco de bajada en x
  );
END debouncer;

ARCHITECTURE debouncerArch of debouncer is

  SIGNAL xSync: std_logic;
  SIGNAL startTimer, timerEnd: std_logic;
  
BEGIN

  synchronizer:
  PROCESS (rst, clk)
    VARIABLE aux1: std_logic;
  BEGIN
    IF (rst='0') THEN
      aux1 := '1';
      xSync <= '1';
    ELSIF (clk'EVENT AND clk='1') THEN
      xSync <= aux1;
      aux1 := x;           
    END IF;
  END PROCESS synchronizer;

  timer:
  PROCESS (rst, clk)
    CONSTANT timeOut: std_logic_vector (21 DOWNTO 0) := "1001100010010110100000"; -- 2500000/(50MHz) = 50 ms
    VARIABLE count: std_logic_vector (21 DOWNTO 0);
  BEGIN
    IF (count=timeOut) THEN
      timerEnd <= '1';
    ELSE 
      timerEnd <= '0';
    END IF;
    IF (rst='0') THEN
      count := timeOut;
    ELSIF (clk'EVENT AND clk='1') THEN
      IF (startTimer='1') THEN
        count := (OTHERS=>'0');
      ELSIF (timerEnd='0') THEN
        count := count + 1;
      END IF;
    END IF;
  END PROCESS timer;
    
  controller:
  PROCESS (xSync, rst, clk)
    TYPE states IS (waitingKeyDown, keyDownDebouncing, waitingKeyUp, KeyUpDebouncing); 
    VARIABLE state: states;
  BEGIN 
    xDeb <= '1';
    xDebFallingEdge <= '0';
    xDebRisingEdge <= '0';
    startTimer <= '0';
    CASE state IS
      WHEN waitingKeyDown =>
        IF (xSync='0') THEN
          xDebFallingEdge <= '1';
          startTimer <= '1';
        END IF;
      WHEN keyDownDebouncing =>
        xDeb <= '0';
      WHEN waitingKeyUp =>
        xDeb <= '0';
        IF (xSync='1') THEN
          xDebRisingEdge <= '1';
          startTimer <= '1';
        END IF;
      WHEN KeyUpDebouncing =>
        NULL;
      END CASE;
    IF (rst='0') THEN
      state := waitingKeyDown;
    ELSIF (clk'EVENT AND clk='1') THEN
      CASE state IS
        WHEN waitingKeyDown =>
          IF (xSync='0') THEN
            state := keyDownDebouncing;
          END IF;
        WHEN keyDownDebouncing =>
          IF (timerEnd='1') THEN
            state := waitingKeyUp;
          END IF;
        WHEN waitingKeyUp =>
          IF (xSync='1') THEN
            state := KeyUpDebouncing;
          END IF;
        WHEN KeyUpDebouncing =>
          IF (timerEnd='1') THEN
            state := waitingKeyDown;
          END IF;
      END CASE;
    END IF;
  END PROCESS controller;  

END debouncerArch;
