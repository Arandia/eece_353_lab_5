-- This impliments the program counter and program memory for the controller

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity PC is
  
  port (
    clk       : in  std_logic;
    reset     : in  std_logic;
    loadPC    : in  std_logic;          -- This sends the PC to the ROM
    import_PC : in  std_logic;          -- This tells the PC to take the value from PC_in
    PC_in     : in  std_logic_vector (7 downto 0);  -- This comes from the controller, for the jump instruction
    PC_out    : out std_logic_vector (7 downto 0));
  
end PC;

architecture pc_arch of PC is
  variable bank : std_logic_vector (7 downto 0) := "00000000";  -- The current PC value
begin  -- pc_arch

  -- purpose: This implements the register bank that is the PC
  -- type   : sequential
  -- inputs : clk, reset
  -- outputs: bank
  process (clk, reset)
  begin  -- process
    if reset = '1' then                 -- asynchronous reset (active low)
      bank := "00000000";
    elsif clk'event and clk = '1' then  -- rising clock edge
      if import_PC = '1' then
        bank := PC_in;
      elsif loadPC = '1' then
        bank := bank + 1;
      end if;
    end if;
  end process;

  PC_out <= bank;
  
end pc_arch;


library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
library lpm;
use lpm.lpm_components.all;

entity memory is
  
  port (
    clk             : in  std_logic;
    reset           : in  std_logic;
    loadPC          : in  std_logic;    -- From controller
    import_PC       : in  std_logic;    -- This tells the PC to take the value from PC_in
    PC_in           : in  std_logic_vector (7 downto 0);  -- This comes from the controller, for the jump instruction
    instruction_out : out std_logic_vector (15 downto 0));  -- To controller

end memory;

architecture mem of memory is
  signal clkinv : std_logic;
  signal PC_out : std_logic_vector (7 downto 0);

  component PC
    generic (
      clk       : in  std_logic;
      reset     : in  std_logic;
      loadPC    : in  std_logic;          -- This sends the PC to the ROM
      import_PC : in  std_logic;          -- This tells the PC to take the value from PC_in
      PC_in     : in  std_logic_vector (7 downto 0);  -- This comes from the controller, for the jump instruction
      PC_out    : out std_logic_vector (7 downto 0));
  end component;

begin  -- mem

  clkinv <= not clk;                    -- The inverse of the clock
  
  u0 : lpm_rom
    generic map (
      lpm_widthad         => 8,
      lpm_width           => 16,
      lpm_address_control => "Registered",
      lpm_outdata         => "Unregistered",
      lpm_file            => "program.mif")
    port map (
      address => PC_out,
      q       => instruction_out,
      inclock => clkinv);

  u1 : PC
    port map (
      clk       => clk,
      reset     => reset,
      loadPC    => loadPC,
      import_PC => import_PC,
      PC_in     => PC_in,
      PC_out    => PC_out);
  
end mem;
