-- This is a wrapper file, that instantiates both the datapath and the controller

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
library work;
use work.all;

entity lab5 is
  port(
    key  : in  std_logic_vector(3 downto 0);  -- pushbutton switches
    sw   : in  std_logic_vector(17 downto 0);  -- slide switches
    ledg : out std_logic_vector(7 downto 0); -- green LED's
    ledr : out std_logic_vector(17 downto 0)); -- red LED's
end lab5 ;

architecture behavioural of lab5 is
  signal datapath_in         : std_logic_vector(7 downto 0);  -- The wire to 'datapath'
  signal to_PC               : std_logic_vector (7 downto 0);  -- 
  signal mux_sel, funct      : std_logic_vector(1 downto 0);  -- msb is 'a', lsb is 'b'
  signal load_xyz            : std_logic_vector (2 downto 0);  -- self-explanitory?
  signal clk, write, load_PC : std_logic;
  signal write_to, read_to   : std_logic_vector(2 downto 0);  -- Addresses
  signal instruction         : std_logic_vector (15 downto 0);
begin
  clk <= key(0);
  
  u2: memory
    port map (
      clk             => clk,
      reset           => key(3),
      loadPC          => get_next,
      import_PC       => load_PC,
      PC_in           => to_PC,
      instruction_out => instruction);  -- What the controller should do next
  
  u0: controller
    port map (
      clk            => clk,
      instruction_in => instruction,    -- From the ROM
      datapath_in    => datapath_in,    -- To the datapath, not controller
      mux_sel        => mux_sel,
      load_xyz       => load_xyz,
      write          => write,
      write_to       => write_to,
      read_to        => read_to,
      funct          => funct,
      get_next       => ledr(17))
  
  u1: datapath
    port map( 
      clk           => clk,
      datapath_in   => datapath_in,
      muxasel       => mux_sel(1),
      muxbsel       => mux_sel(0),
      loadx         => load_xyz(2),
      loady         => load_xyz(1),
      loadz         => load_xyz(0),
      write         => write,
      writenum      => write_to,
      readnum       => read_to,
      funct         => funct,
      datapath_out  => ledg,
      datapath_out2 => ledr);

end behavioural;

