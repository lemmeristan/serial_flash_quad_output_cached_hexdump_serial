LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY serial IS
  GENERIC (
    base_address : STD_LOGIC_VECTOR(31 DOWNTO 0) := X"00000000";
    baud_rate : INTEGER := 115200; -- 115200 bits per second
    clk_freq : INTEGER := 25000000 -- 25 MHz
  );
  PORT (
    clk_25mhz : IN STD_LOGIC;
    ftdi_rxd : OUT STD_LOGIC; -- FPGA transmits to ftdi
    ftdi_txd : IN STD_LOGIC;
    btn : IN STD_LOGIC_VECTOR(6 DOWNTO 0);
    led : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);

    flash_csn : OUT STD_LOGIC;
    flash_mosi : INOUT STD_LOGIC; -- io(0)
    flash_miso : IN STD_LOGIC; -- io(1)
    flash_wpn : INOUT STD_LOGIC; -- io(2)
    flash_holdn : INOUT STD_LOGIC -- io(3)

  );

END serial;

ARCHITECTURE behavioural OF serial IS

  COMPONENT quadflash_cache
    GENERIC (
      vendor : STD_LOGIC; -- 0 => xilinx, 1 => lattice

      base_address : STD_LOGIC_VECTOR(31 DOWNTO 0)
    );

    PORT (
      reset : IN STD_LOGIC;
      clk : IN STD_LOGIC;

      mem_clk : IN STD_LOGIC;
      mem_re : IN STD_LOGIC;
      mem_addr : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
      mem_rdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
      mem_rdy : OUT STD_LOGIC;
      spi_csn, spi_sck, spi_di, spi_wpn, spi_holdn : OUT STD_LOGIC;
      spi_do : IN STD_LOGIC;

      spi_io : IN STD_LOGIC_VECTOR(3 DOWNTO 0);

      spi_reading : OUT STD_LOGIC;
      led : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
    );
  END COMPONENT;

  COMPONENT USRMCLK
    PORT (
      USRMCLKI : IN STD_ULOGIC;
      USRMCLKTS : IN STD_ULOGIC
    );
  END COMPONENT;
  ATTRIBUTE syn_noprune : BOOLEAN;
  ATTRIBUTE syn_noprune OF USRMCLK : COMPONENT IS true;

  CONSTANT COUNTER_MAX : INTEGER := (clk_freq/baud_rate) - 1;

  SIGNAL counter : INTEGER := COUNTER_MAX; -- 115207 bps
  SIGNAL state, n_state : INTEGER RANGE 0 TO 15;
  SIGNAL rst, txd : STD_LOGIC;

  SIGNAL charcounter, n_charcounter : INTEGER RANGE 0 TO 8 := 0;

  SIGNAL chartobesent : STD_LOGIC_VECTOR(7 DOWNTO 0);

  SIGNAL p_btn, do_start, mem_rdy, mem_re : STD_LOGIC;

  SIGNAL mem_addr, n_mem_addr, mem_rdata : STD_LOGIC_VECTOR(31 DOWNTO 0);
  SIGNAL flash_clk : STD_LOGIC;

  SIGNAL spi_csn, spi_clk, spi_di, spi_do, spi_wpn, spi_holdn, spi_reading : STD_LOGIC;

  SIGNAL spi_io : STD_LOGIC_VECTOR(3 DOWNTO 0);

  SIGNAL half_clk : STD_LOGIC;
BEGIN

  rst <= NOT btn(0);
  inst_quadflash_cache : quadflash_cache GENERIC MAP(
    vendor => '1',
    base_address => base_address
    ) PORT MAP(
    reset => rst,
    clk => half_clk, --clk_25mhz,

    mem_clk => clk_25mhz,
    mem_re => mem_re,
    mem_addr => mem_addr,
    mem_rdata => mem_rdata,
    mem_rdy => mem_rdy,

    spi_csn => spi_csn, spi_sck => spi_clk,
    spi_di => spi_di, spi_do => spi_do, spi_wpn => spi_wpn, spi_holdn => spi_holdn,

    spi_io => spi_io, spi_reading => spi_reading,
    led => led
  );

  spi_io <= flash_holdn & flash_wpn & flash_miso & flash_mosi;

  PROCESS (spi_reading, spi_holdn, spi_wpn, spi_di)
  BEGIN
    IF spi_reading = '1' THEN
      flash_mosi <= 'Z';
      flash_wpn <= 'Z';
      flash_holdn <= 'Z';
    ELSE
      flash_mosi <= spi_di;
      flash_wpn <= spi_wpn;
      flash_holdn <= spi_holdn;
    END IF;
  END PROCESS;

  flash_csn <= spi_csn;
  spi_do <= flash_miso;

  u1 : USRMCLK PORT MAP(
    USRMCLKI => spi_clk,
    USRMCLKTS => rst);
  PROCESS (rst, clk_25mhz)
  BEGIN
    IF rst = '1' THEN
      counter <= COUNTER_MAX; -- counter counts down one bit length
      state <= 10; -- RS232 transmission finite state machine (11 states: start, b0..b7, stop)
      charcounter <= 0;

      p_btn <= '0';
      mem_addr <= X"00400000"; --base_address;
      half_clk <= '0';

    ELSIF rising_edge(clk_25mhz) THEN
      half_clk <= NOT half_clk;

      p_btn <= btn(1);

      IF counter = 0 THEN
        mem_addr <= n_mem_addr;
        charcounter <= n_charcounter;

        state <= n_state; -- assign next state
        counter <= COUNTER_MAX; -- reset counter
      ELSE
        counter <= counter - 1; -- continue counting down
      END IF;

    END IF;
  END PROCESS;

  PROCESS (mem_rdy, state, charcounter, mem_addr, p_btn, btn)
  BEGIN
    do_start <= '0';
    n_charcounter <= charcounter;
    n_mem_addr <= mem_addr;

    IF mem_rdy = '1' THEN
      IF state = 10 THEN -- did we arrive at a stop bit?
        IF charcounter < 8 THEN -- did we finish the entire string?
          n_charcounter <= charcounter + 1; -- if not, increment index
          do_start <= '1';
        ELSIF btn(1) = '1' THEN
          n_charcounter <= 0; -- if so, start over at index 0
          n_mem_addr <= mem_addr + X"00000004";
        ELSIF btn(2) = '1' THEN
          n_mem_addr <= X"00400000";
          n_charcounter <= 0;
        END IF;
      END IF;

    END IF;

  END PROCESS;
  PROCESS (state, do_start, chartobesent)
  BEGIN
    n_state <= state;

    CASE state IS
      WHEN 0 =>
        n_state <= state + 1;
        txd <= '0'; -- start bit
      WHEN 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 =>
        n_state <= state + 1;
        txd <= chartobesent(state - 1);
      WHEN 10 =>
        txd <= '1'; -- stop bit
        IF do_start = '1' THEN
          n_state <= 0;
        END IF;
      WHEN OTHERS =>
        n_state <= 10;
        txd <= '1';
    END CASE;
  END PROCESS;

  ftdi_rxd <= txd;
  --ftdi_rxd <= ftdi_txd;

  PROCESS (charcounter)
  BEGIN
    chartobesent <= (OTHERS => '1');
    CASE charcounter IS

      WHEN 1 =>
        chartobesent <= mem_addr(31 DOWNTO 24);
      WHEN 2 =>
        chartobesent <= mem_addr(23 DOWNTO 16);
      WHEN 3 =>
        chartobesent <= mem_addr(15 DOWNTO 8);
      WHEN 4 =>
        chartobesent <= mem_addr(7 DOWNTO 0);
      WHEN 5 =>
        chartobesent <= mem_rdata(7 DOWNTO 0);
      WHEN 6 =>
        chartobesent <= mem_rdata(15 DOWNTO 8);
      WHEN 7 =>
        chartobesent <= mem_rdata(23 DOWNTO 16);
      WHEN 8 =>
        chartobesent <= mem_rdata(31 DOWNTO 24);
      WHEN OTHERS =>

    END CASE;
  END PROCESS;

  mem_re <= '1';
END ARCHITECTURE behavioural;