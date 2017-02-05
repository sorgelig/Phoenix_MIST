---------------------------------------------------------------------------------
-- DE2-35 Top level for Phoenix by Dar (darfpga@aol.fr) (April 2016)
-- http://darfpga.blogspot.fr
--
-- Main features
--  PS2 keyboard input
--  wm8731 sound output
--  NO board SRAM used
--
-- sw 0: on/off hdmi-audio
--
-- Board switch : ---- todo fixme switches note
--   1 - 4 : dip switch
--             0-1 : lives 3-6
--             3-2 : bonus life 30K-60K
--               4 : coin 1-2
--             6-5 : unkonwn
--               7 : upright-cocktail  
--   8 -10 : sound_select
--             0XX : all mixed (normal)
--             100 : sound1 only 
--             101 : sound2 only
--             110 : sound3 only
--             111 : melody only 
-- Board key :
--      0 : reset
--   
---------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.ALL;
use ieee.numeric_std.all;

entity phoenix_mist is
port
(
  CLOCK_27			: in std_logic;
  LED					: out std_logic;
  VGA_R				: out std_logic_vector(5 downto 0); 
  VGA_G				: out std_logic_vector(5 downto 0);
  VGA_B				: out std_logic_vector(5 downto 0);
  VGA_HS				: out std_logic;
  VGA_VS				: out std_logic;
--  sw: in std_logic_vector(17 downto 0);
  SPI_SCK 			: in std_logic;
  SPI_DI 			: in std_logic;
  SPI_DO 			: out std_logic;
  SPI_SS2 			: in std_logic;
  SPI_SS3 			: in std_logic;
  CONF_DATA0		: in std_logic;
  AUDIO_L 			: out std_logic;
  AUDIO_R 			: out std_logic
);
end;

architecture struct of phoenix_mist is
  signal clk_pixel: std_logic;

  signal audio: std_logic_vector(11 downto 0);
  signal sound_string : std_logic_vector(35 downto 0);
  signal S_vga_r, S_vga_g, S_vga_b: std_logic_vector(1 downto 0);
  signal S_vga_r8, S_vga_g8, S_vga_b8: std_logic_vector(7 downto 0);
  signal S_vga_vsync, S_vga_hsync: std_logic;


  signal tmds_d: std_logic_vector(3 downto 0);
  signal tx_in: std_logic_vector(29 downto 0);


  signal kbd_intr      : std_logic;
  signal kbd_scancode  : std_logic_vector(7 downto 0);
  signal JoyPCFRLDU    : std_logic_vector(7 downto 0);

  signal coin         : std_logic;
  signal player_start : std_logic_vector(1 downto 0);
  signal button_left, button_right, button_protect, button_fire: std_logic;

  signal reset        : std_logic;
  signal clock_stable : std_logic;
  signal dip_switch   : std_logic_vector(7 downto 0);-- := (others => '0');
  signal audio_select : std_logic_vector(2 downto 0);-- is sw(10 downto 8);
  
  
  -- User IO
  signal switches   : std_logic_vector(1 downto 0);
  signal buttons    : std_logic_vector(1 downto 0);
  signal joy0       : std_logic_vector(7 downto 0);
  signal joy1       : std_logic_vector(7 downto 0);
  signal status     : std_logic_vector(7 downto 0);
  signal ascii_new  : std_logic;
  signal ascii_code : STD_LOGIC_VECTOR(6 DOWNTO 0);
  signal clk14k     : std_logic;
  signal clk8m     : std_logic;
  signal ps2Clk     : std_logic;
  signal ps2Data    : std_logic;
  signal ps2_scancode : std_logic_vector(7 downto 0);
  signal kbd_joy0 		: std_logic_vector(7 downto 0);

--  signal io_index : std_logic_vector(4 downto 0);
  signal scandoubler_disable : std_logic;

  signal hsync_out : std_logic;
  signal vsync_out : std_logic;



-- config string used by the io controller to fill the OSD
  constant CONF_STR : string := "PHOENIX;;T1,Coin;T2,Player 1 Start;T3,Player 2 Start;T5,Reset;V,v0.1;";

  function to_slv(s: string) return std_logic_vector is
    constant ss: string(1 to s'length) := s;
    variable rval: std_logic_vector(1 to 8 * s'length);
    variable p: integer;
    variable c: integer;
  
  begin  
    for i in ss'range loop
      p := 8 * i;
      c := character'pos(ss(i));
      rval(p - 7 to p) := std_logic_vector(to_unsigned(c,8));
    end loop;
    return rval;

  end function;
  
  component pll27
	PORT
	(
		inclk0		: IN STD_LOGIC  := '0';
		c0		: OUT STD_LOGIC ;
		c1		: OUT STD_LOGIC ;
		c2		: OUT STD_LOGIC ;
		c3		: OUT STD_LOGIC ;
		locked		: OUT STD_LOGIC 
	);
end component;

  
  component user_io
	 generic ( STRLEN : integer := 0 );
    port (
      SPI_CLK, SPI_SS_IO, SPI_MOSI :in std_logic;
      SPI_MISO : out std_logic;
      conf_str : in std_logic_vector(8*STRLEN-1 downto 0);
      switches : out std_logic_vector(1 downto 0);
      buttons : out std_logic_vector(1 downto 0);
      scandoubler_disable : out std_logic;
      joystick_0 : out std_logic_vector(7 downto 0);
      joystick_1 : out std_logic_vector(7 downto 0);
      status : out std_logic_vector(7 downto 0);
      ps2_clk : in std_logic;
      ps2_kbd_clk : out std_logic;
      ps2_kbd_data : out std_logic
    );
  end component user_io;


  component osd
    port (
      pclk, sck, ss, sdi, hs_in, vs_in, scanline_ena_h : in std_logic;
      red_in, blue_in, green_in : in std_logic_vector(5 downto 0);
      red_out, blue_out, green_out : out std_logic_vector(5 downto 0);
      hs_out, vs_out : out std_logic
    );
  end component osd;
  
   component keyboard
	PORT(
	clk : in std_logic;
	reset : in std_logic;
	ps2_kbd_clk : in std_logic;
	ps2_kbd_data : in std_logic;
	joystick : out std_logic_vector (7 downto 0)
	);
end component;



begin
 
--   SWITCH 1:     SWITCH 2:    NUMBER OF SPACESHIPS:
--   ---------     ---------    ---------------------
--     OFF           OFF                  6
--     ON            OFF                  5
--     OFF           ON                   4
--     ON            ON                   3
--                               FIRST FREE     SECOND FREE
--   SWITCH 3:     SWITCH 4:     SHIP SCORE:    SHIP SCORE:
--  ---------     ---------     -----------    -----------
--     OFF           OFF           6,000          60,000
--     ON            OFF           5,000          50,000
--     OFF           ON            4,000          40,000
--     ON            ON            3,000          30,000
 
--Cocktail,Factory,Factory,Factory,Bonus2,Bonus1,Ships2,Ships1
dip_switch <= "00001111";

--  OSD
  VGA_HS <= not hsync_out;
  VGA_VS <= not vsync_out;
  
  osd_inst : osd
    port map (
      pclk => clk8m,
      sdi => SPI_DI,
      sck => SPI_SCK,
      ss => SPI_SS3,
      red_in => S_vga_r & S_vga_r & S_vga_r,
      green_in => S_vga_g & S_vga_g & S_vga_g,
      blue_in => S_vga_b & S_vga_b & S_vga_b,
      hs_in => S_vga_hsync,
      vs_in => S_vga_vsync,
      scanline_ena_h => status(3),
      red_out => VGA_R,
      green_out => VGA_G,
      blue_out => VGA_B,
      hs_out => hsync_out,
      vs_out => vsync_out
    );
	 
	 user_io_inst : user_io
 	generic map (STRLEN => CONF_STR'length)
   port map (
      SPI_CLK => SPI_SCK,
      SPI_SS_IO => CONF_DATA0,
      SPI_MOSI => SPI_DI,
      SPI_MISO => SPI_DO,
      conf_str => to_slv(CONF_STR),
      switches => switches,
      buttons  => buttons,
      scandoubler_disable  => scandoubler_disable,
      joystick_1 => joy1,
      joystick_0 => joy0,
      status => status,
      ps2_clk => clk14k,
      ps2_kbd_clk => ps2Clk,
      ps2_kbd_data => ps2Data
    );
    

sound_string <= audio & audio & audio;

  --
  -- Audio
  --
  u_dac1 : entity work.dac
    port  map(
      clk_i     => clk_pixel,
      res_n_i   => not reset,
      dac_i  => sound_string,
      dac_o => AUDIO_L
    );
	 
	u_dac2 : entity work.dac
    port  map(
      clk_i     => clk_pixel,
      res_n_i   => not reset,
      dac_i  => sound_string,
      dac_o => AUDIO_R
    );
    
 
  clkgen_sdr: pll27
  port map(
      inclk0 => CLOCK_27, 
		c0 => clk_pixel,
		c1 => clk14k,
		c2 => clk8m,
      locked => clock_stable
  );

reset <= status(0) or status(5) or buttons(1) or not clock_stable; 

-- dip_switch(3 downto 0) <= sw(4 downto 1);

u_keyboard : keyboard
    port  map(
	clk 				=> clk_pixel,
	reset 			=> reset,
	ps2_kbd_clk 	=> ps2Clk,
	ps2_kbd_data 	=> ps2Data,
	joystick 		=> kbd_joy0
);

  phoenix : entity work.phoenix
  port map
  (
    clk_pixel    => clk_pixel,
    reset        => reset,
    dip_switch   => dip_switch,
    btn_coin     => kbd_joy0(3) or status(1),--ESC
    btn_player_start(0) => kbd_joy0(1) or status(2),--1
    btn_player_start(1) => kbd_joy0(2) or status(3),--2 
    btn_left     => joy0(2) or kbd_joy0(5) or kbd_joy0(6),
    btn_right    => joy0(3) or kbd_joy0(4) or kbd_joy0(7),
    btn_barrier  => joy0(0) or kbd_joy0(2),--TAB
    btn_fire     => joy0(4) or kbd_joy0(0),
    video_r      => S_vga_r,
    video_g      => S_vga_g,
    video_b      => S_vga_b,
    video_hs     => S_vga_hsync,
    video_vs     => S_vga_vsync,
    audio_select => audio_select,
    audio        => audio
  );

end struct;
