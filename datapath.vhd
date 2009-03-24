
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity multiplexer is
  
  port (
    selector       : in  std_logic;
    input0, input1 : in  std_logic_vector(7 downto 0);
    result         : out std_logic_vector(7 downto 0));

end multiplexer;

architecture multi of multiplexer is

begin  -- multi

  result <= input0 when selector = '0'
            else input1;

end multi;

library ieee;
use ieee.std_logic_1164.all;
entity register_8bit is
  
  port (
    input     : in  std_logic_vector(7 downto 0);
    load, clk : in  std_logic;
    output    : out std_logic_vector(7 downto 0));

end register_8bit;

architecture reg of register_8bit is

begin  -- reg

  process (clk)
  begin  -- process
    if clk = '1' then  -- rising clock edge
	  if load = '1' then
        output <= input;
      end if;
    end if;
  end process;

end reg;

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
entity ALU is
  
  port (
    funct : in std_logic_vector(1 downto 0);   -- 00 is A-B, 01 is A+B, 10 is A bitwise and B, 11 is the lower 8 bits of A*B
    A, B  : in std_logic_vector(7 downto 0);
    C     : out std_logic_vector(7 downto 0));  -- The output
end ALU;

architecture alu_arch of ALU is
  signal temp : std_logic_vector(15 downto 0) := "0000000000000000";  -- This holds the result of a*b
begin  -- alu_arch

  temp <= (A * B) when funct = "11"
          else "0000000000000000";

  with funct select
    C <= 
      A - B when "00",
      A + B when "01",
      A and B when "10",
      temp(7 downto 0) when others;

end alu_arch;

library ieee;
use ieee.std_logic_1164.all;
entity RAM is
  
  port (
    data_in           : in  std_logic_vector(7 downto 0);
    write, clk        : in  std_logic;
    writenum, readnum : in  std_logic_vector(2 downto 0);
                                        -- These specify the register bank to read from / write to
    data_out          : out std_logic_vector(7 downto 0));

end RAM;

architecture RAM_arch of RAM is
  signal bank0, bank1, bank2, bank3, bank4, bank5, bank6, bank7 : std_logic_vector(7 downto 0) := "00000000";
begin  -- RAM_arch

  -- purpose: Read from the RAM, according to readnum
  -- type   : combinational
  -- inputs : readnum, bank0 ... bank7
  -- outputs: data_out
  process (readnum, bank0, bank1, bank2, bank3, bank4, bank5, bank6, bank7)
  begin  -- process
    case readnum is
      when "000" => data_out <= bank0;
      when "001" => data_out <= bank1;
      when "010" => data_out <= bank2;
      when "011" => data_out <= bank3;
      when "100" => data_out <= bank4;
      when "101" => data_out <= bank5;
      when "110" => data_out <= bank6;
      when others => data_out <= bank7;
    end case;
  end process;

  -- purpose: Write into the RAM, according to writenum
  -- type   : sequential
  -- inputs : writenum, clk, data_in
  -- outputs: bank0 ... bank7
  process (clk)
  begin  -- process
    if clk = '1' then
      if write = '1' then
        case writenum is
          when "000" => bank0 <= data_in;
          when "001" => bank1 <= data_in;
          when "010" => bank2 <= data_in;
          when "011" => bank3 <= data_in;
          when "100" => bank4 <= data_in;
          when "101" => bank5 <= data_in;
          when "110" => bank6 <= data_in;
          when others => bank7 <= data_in;
        end case;
      end if;
    end if;
  end process;

end RAM_arch;

library ieee;
use ieee.std_logic_1164.all;
entity datapath is
     port(datapath_in  : in std_logic_vector(7 downto 0);
	  muxasel      : in std_logic;
	  write        : in std_logic;
	  writenum     : in std_logic_vector(2 downto 0);
	  readnum      : in std_logic_vector(2 downto 0);
	  loadx        : in std_logic;
	  loady        : in std_logic;
	  muxbsel      : in std_logic;
	  funct        : in std_logic_vector(1 downto 0);
	  loadz        : in std_logic;
	  clk          : in std_logic;
	  datapath_out : out std_logic_vector(7 downto 0);
	  datapath_out2: out std_logic_vector(16 downto 0));
end datapath ;


architecture behavioural of datapath is

  -- Your code goes here.
  component multiplexer
    port (
      selector       : in  std_logic;
      input0, input1 : in  std_logic_vector(7 downto 0);
      result         : out std_logic_vector(7 downto 0));
  end component;

  component register_8bit
    port (
      input     : in  std_logic_vector(7 downto 0);
      load, clk : in  std_logic;
      output    : out std_logic_vector(7 downto 0));
  end component;

  component ALU
    port (
      funct : in std_logic_vector(1 downto 0);   -- 00 is A-B, 01 is A+B, 10 is A bitwise and B, 11 is the lower 8 bits of A*B
      A, B  : in std_logic_vector(7 downto 0);
      C     : out std_logic_vector(7 downto 0));  -- The output
  end component;

  component RAM
    port (
    data_in           : in  std_logic_vector(7 downto 0);
    write, clk        : in  std_logic;
    writenum, readnum : in  std_logic_vector(2 downto 0);
                                        -- These specify the register bank to read from / write to
    data_out          : out std_logic_vector(7 downto 0));
  end component;

  -- These are the internal busses between the components
  signal preRAM, postRAM, x, y, x2, postALU, z : std_logic_vector(7 downto 0) := "00000000";
begin  
  m1: multiplexer
    port map (
      selector => muxasel,
      input0   => datapath_in,
      input1   => z,
      result   => preRAM);

  r1: RAM
    port map (
      data_in  => preRAM,
      write    => write,
      writenum => writenum,
      clk      => clk,
      readnum  => readnum,
      data_out => postRAM);

  reg1: register_8bit
    port map (
      input  => postRAM,
      load   => loadx,
      clk    => clk,
      output => x);

  reg2: register_8bit
    port map (
      input  => postRAM,
      load   => loady,
      clk    => clk,
      output => y);

  m2: multiplexer
    port map (
      selector => muxbsel,
      input0   => "00000000",
      input1   => x,
      result   => x2);

  a1: ALU
    port map (
      funct => funct,
      A     => X2,
      B     => Y,
      C     => postALU);

  reg3: register_8bit
    port map (
      input  => postALU,
      load   => loadz,
      clk    => clk,
      output => z);

  datapath_out <= z;
  datapath_out2(7 downto 0) <= postRAM;
  datapath_out2(15 downto 8) <= preRAM;
  datapath_out2(16) <= '0';
  
end behavioural;

