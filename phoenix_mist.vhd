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
  signal buttons    : std_logic_vector(1 downto 0);
  signal joy        : std_logic_vector(7 downto 0);
  signal joy0       : std_logic_vector(7 downto 0);
  signal joy1       : std_logic_vector(7 downto 0);
  signal status     : std_logic_vector(31 downto 0);
  signal ascii_new  : std_logic;
  signal ascii_code : STD_LOGIC_VECTOR(6 DOWNTO 0);
  signal clk44m     : std_logic;
  signal clk50m     : std_logic;
  signal clk10m     : std_logic;
  signal ps2Clk     : std_logic;
  signal ps2Data    : std_logic;
  signal ps2_scancode : std_logic_vector(7 downto 0);
  signal kbd_joy0 		: std_logic_vector(7 downto 0);

  signal scandoubler_disable : std_logic;
  signal ypbpr     : std_logic;
  signal ce_pix    : std_logic;

  signal video_vblank    : std_logic;
  signal video_hblank_bg : std_logic;
  signal video_hblank_fg : std_logic;
  
  signal R,G,B      : std_logic_vector(2 downto 0);


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

   component mist_io
		generic ( STRLEN : integer := 0 );
		port (
			clk_sys :in std_logic;
			SPI_SCK, CONF_DATA0, SPI_DI :in std_logic;
			SPI_DO : out std_logic;
			conf_str : in std_logic_vector(8*STRLEN-1 downto 0);
			buttons : out std_logic_vector(1 downto 0);
			joystick_0 : out std_logic_vector(7 downto 0);
			joystick_1 : out std_logic_vector(7 downto 0);
			status : out std_logic_vector(31 downto 0);
			scandoubler_disable, ypbpr : out std_logic;
			ps2_kbd_clk : out std_logic;
			ps2_kbd_data : out std_logic
		);
	end component mist_io;

	component video_mixer
		generic ( LINE_LENGTH : integer := 352; HALF_DEPTH : integer := 1 );
		port (
			clk_sys, ce_pix, ce_pix_actual : in std_logic;
			SPI_SCK, SPI_SS3, SPI_DI : in std_logic;
			scanlines : in std_logic_vector(1 downto 0);
			scandoubler_disable, hq2x, ypbpr, ypbpr_full : in std_logic;

			R, G, B : in std_logic_vector(2 downto 0);
			HSync, VSync, line_start, mono : in std_logic;

			VGA_R,VGA_G, VGA_B : out std_logic_vector(5 downto 0);
			VGA_VS, VGA_HS : out std_logic
		);
	end component video_mixer;

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

	mist_io_inst : mist_io
	generic map (STRLEN => CONF_STR'length)
	port map (
		clk_sys => clk_pixel,
		SPI_SCK => SPI_SCK,
		CONF_DATA0 => CONF_DATA0,
		SPI_DI => SPI_DI,
		SPI_DO => SPI_DO,
		conf_str => to_slv(CONF_STR),
		buttons  => buttons,
		scandoubler_disable => scandoubler_disable,
		ypbpr => ypbpr,
		joystick_1 => joy1,
		joystick_0 => joy0,
		status => status,
		ps2_kbd_clk => ps2Clk,
		ps2_kbd_data => ps2Data
	);

	joy <= joy0 or joy1;

  --
  -- Audio
  --
	u_dac1 : entity work.dac
	port  map(
		clk_i   => clk_pixel,
		res_n_i => not reset,
		dac_i   => audio,
		dac_o   => AUDIO_L
	);
	 
	u_dac2 : entity work.dac
	port  map(
		clk_i   => clk_pixel,
		res_n_i => not reset,
		dac_i   => audio,
		dac_o   => AUDIO_R
	);
    
 
	pll: entity work.pll27
	port map(
      inclk0 => CLOCK_27, 
		c0 => clk44m,
		c1 => clk_pixel,
		c2 => clk50m,
		c3 => clk10m,
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
		clk50m       => clk50m,
		clk10m       => clk10m,
		reset        => reset,
		ce_pix       => ce_pix,
		dip_switch   => dip_switch,
		btn_coin     => kbd_joy0(3) or status(1),--ESC
		btn_player_start(0) => kbd_joy0(1) or status(2),--1
		btn_player_start(1) => kbd_joy0(2) or status(3),--2 
		btn_left     => joy(2) or kbd_joy0(5) or kbd_joy0(6),
		btn_right    => joy(3) or kbd_joy0(4) or kbd_joy0(7),
		btn_barrier  => joy(0) or kbd_joy0(2),--TAB
		btn_fire     => joy(4) or kbd_joy0(0),
		video_r      => S_vga_r,
		video_g      => S_vga_g,
		video_b      => S_vga_b,
		video_hs     => S_vga_hsync,
		video_vs     => S_vga_vsync,
		audio_select => audio_select,
		audio        => audio,
		video_vblank => video_vblank,
		video_hblank_bg => video_hblank_bg,
		video_hblank_fg => video_hblank_fg
	);
	
	R <= "000" when (video_vblank or video_hblank_bg or video_hblank_fg) = '1' else S_vga_r & S_vga_r(1);
	G <= "000" when (video_vblank or video_hblank_bg or video_hblank_fg) = '1' else S_vga_g & S_vga_g(1);
	B <= "000" when (video_vblank or video_hblank_bg or video_hblank_fg) = '1' else S_vga_b & S_vga_b(1);

	vmixer : video_mixer
	port map (
		clk_sys => clk44m,
		ce_pix  => ce_pix,
		ce_pix_actual => ce_pix,

		SPI_SCK => SPI_SCK,
		SPI_SS3 => SPI_SS3,
		SPI_DI => SPI_DI,

		scanlines => "00", -- scanlines,
		scandoubler_disable => scandoubler_disable,
		hq2x => '1', --hq2x,
		ypbpr => ypbpr,
		ypbpr_full => '1',

		R => R,
		G => G,
		B => B,
		HSync => S_vga_hsync,
		VSync => S_vga_vsync,
		line_start => '0',
		mono => '0',

		VGA_R => VGA_R,
		VGA_G => VGA_G,
		VGA_B => VGA_B,
		VGA_VS => VGA_VS,
		VGA_HS => VGA_HS
	);


end struct;
