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
  signal clk, write          : std_logic;                      -- From controller to datapath
  signal datapath_in         : std_logic_vector(7 downto 0);
  signal mux_sel, funct      : std_logic_vector(1 downto 0);   -- msb is 'a', lsb is 'b'
  signal load_xyz            : std_logic_vector (2 downto 0);  -- self-explanitory?
  signal write_to, read_to   : std_logic_vector(2 downto 0);   -- Addresses
  
  signal instruction         : std_logic_vector (15 downto 0); -- From mem to controller
  signal to_PC               : std_logic_vector (7 downto 0);  -- From the controller to the mem
  signal get_next, import_PC : std_logic;
begin
  clk <= key(0);
  ledr(17) <= get_next;
  
  u2: memory
    port map (
      -- INPUT
      clk             => clk,
      reset           => key(3),
      loadPC          => get_next,      -- From the controller
      import_PC       => import_PC,
      PC_in           => to_PC,
      -- OUTPUT
      instruction_out => instruction);  -- To the controller
  
  u0: controller
    port map (
      -- INPUT
      clk            => clk,
      instruction_in => instruction,    -- From the mem
      -- OUTPUT
      get_next       => get_next,       -- To the mem
      set_PC         => import_PC,
      set_PC_to      => to_PC,
      mux_sel        => mux_sel,        -- To the datapath
      load_xyz       => load_xyz,
      write          => write,
      write_to       => write_to,
      read_to        => read_to,
      funct          => funct,
      datapath_in    => datapath_in)
  
  u1: datapath
    port map(
      -- INPUT
      clk           => clk,
      datapath_in   => datapath_in,     -- From the controller
      muxasel       => mux_sel(1),
      muxbsel       => mux_sel(0),
      loadx         => load_xyz(2),
      loady         => load_xyz(1),
      loadz         => load_xyz(0),
      write         => write,
      writenum      => write_to,
      readnum       => read_to,
      funct         => funct,
      -- OUTPUT
      datapath_out  => ledg,            -- To display
      datapath_out2 => ledr);

end behavioural;

