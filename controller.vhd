-- This impliments the controller for the datapath

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity inst_reg is
  
  port (
    instruction_in : in  std_logic_vector (15 downto 0);
    load_ir        : in  std_logic;
    clk            : in  std_logic;
    instruction    : out std_logic_vector (15 downto 0));

end inst_reg;

architecture reg of inst_reg is
  signal bank : std_logic_vector (15 downto 0) := "0000000000000000";
begin  -- reg
  process (clk)
  begin  -- process
    if clk'event and clk = '1' then  -- rising clock edge
      if load_ir = '1' then
        bank <= instruction_in;
      end if;
    end if;
  end process;

  instruction <= bank;                  -- Send the register contents to output

end reg;

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity decode is
  
  port (
    instruction       : in  std_logic_vector (15 downto 0);
    state             : in  std_logic_vector (2 downto 0);
    datapath          : in  std_logic_vector (7 downto 0);  -- Input from datapath
    datapath_in       : out std_logic_vector (7 downto 0);  -- Output to datapath
    mux_sel           : out std_logic_vector (1 downto 0);
    load_xyz, funct   : out std_logic_vector (2 downto 0);
    write             : out std_logic;
    write_to, read_to : out std_logic_vector (2 downto 0);
    set_PC            : out std_logic;
    set_PC_to         : out std_logic_vector (7 downto 0));

end decode;

architecture lut of decode is

begin  -- lut

  -- purpose: This serves as the LUT to directly control the datapath inputs, given an instruction set and state
  -- type   : combinational
  -- inputs : instruction, state
  -- outputs: EVERYTHING!
  process (instruction, state)
  begin  -- process
    -- Make sure it's synthesizable...
    datapath_in <= "00000000";
    mux_sel <= "00";
    load_xyz <= "000";
    funct <= "000";
    write <= '0';
    write_to <= "000";
    read_to <= "000";
    set_PC <= '0';
    set_PC_to <= "00000000";
    
    case instruction(15 downto 11) is
      when "01000" =>                   -- Load
        if state = "010" then
          datapath_in <= instruction(7 downto 0);
          write_to <= instruction(10 downto 8);
          write <= '1';
        end if;
      when "11000" =>                   -- Move
        if state = "010" then
          load_xyz <= "010";
          read_to <= instruction(4 downto 2);
        elsif state = "011" then
          funct <= "01";                --  '+'
          load_xyz <= "001";
        elsif state = "100" then
          mux_sel = "10";
          write_to <= instruction(10 downto 8);
          write <= '1';
        end if;
      when "11010" =>                   -- Neg
        if state = "010" then
          load_xyz <= "010";
          read_to <= instruction(4 downto 2);
        elsif state = "011" then
          funct <= "00";                -- '-'
          load_xyz <= "001";
        elsif state = "100" then
          mux_sel <= "10";
          write <= '1';
          write_to <= instruction(10 downto 8);
        end if;
      when "10001" =>                   -- Add
        if state = "010" then
          load_xyz <= "100";
          read_to <= instruction(7 downto 5);
        elsif state = "011" then
          load_xyz <= "010";
          read_to <= instruction(4 downto 2);
        elsif state = "100" then
          funct <= "01";                -- '+'
          mux_sel <= "01";
          load_xyz <= "001";
        elsif state = "101" then
          mux_sel <= "10";
          write = '1';
          write_to <= instruction(10 downto 8);
        end if;
      when "10010" =>                   -- Sub
        if state = "010" then
          load_xyz <= "100";
          read_to <= instruction(7 downto 5);
        elsif state = "011" then
          load_xyz <= "010";
          read_to <= instruction(4 downto 2);
        elsif state = "100" then
          funct <= "00";                -- '-'
          mux_sel <= "01";
          load_xyz <= "001";
        elsif state =  "101" then
          mux_sel <= "10";
          write = '1';
          write_to <= instruction(10 downto 8);
        end if;
      when "10101" =>                   -- Mul
        if state = "010" then
          load_xyz <= "100";
          read_to <= instruction(7 downto 5);
        elsif state = "011" then
          load_xyz <= "010";
          read_to <= instruction(4 downto 2);
        elsif state = "100" then
          funct <= "11";                -- '*'
          mux_sel <= "01";
          load_xyz <= "001";
        elsif state =  "101" then
          mux_sel <= "10";
          write = '1';
          write_to <= instruction(10 downto 8);
        end if;
      when "10111" =>                   -- And
        if state = "010" then
          load_xyz <= "100";
          read_to <= instruction(7 downto 5);
        elsif state = "011" then
          load_xyz <= "010";
          read_to <= instruction(4 downto 2);
        elsif state = "100" then
          funct <= "10";                -- '&'
          mux_sel <= "01";
          load_xyz <= "001";
        elsif state =  "101" then
          mux_sel <= "10";
          write = '1';
          write_to <= instruction(10 downto 8);
        end if;
      when "11001" =>                   -- Jump
        if state = "010" then
          set_PC_to <= instruction(7 downto 0);
          set_PC <= '1';
        end if;
      when others =>
        if instruction(15 downto 14) = "00" then  -- BEQ
          -- The first two bits id, the next three are used to point
          if state = "010" then
            load_xyz <=  "100";
            read_to <= instruction(13 downto 11);
          elsif state  = "011" then
            load_xyz <=  "010";
            read_to <= instruction(10 downto 8);
          elsif state = "100" then
            funct <=  "00";             -- '-'
            load_xyz <= "001";
          elsif state = "101" then
            if datapath = "00000000" then  -- jump to specified location
              set_PC_to <=  instruction(7 downto 0);
              set_PC <= '1';
            end if;
          end if;
        end if;
        null;
    end case;
  end process;
end lut;

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity state_machine is
  
  port (
    instruction          : in  std_logic_vector (15 downto 0);
    clk                  : in  std_logic;
    get_next_instruction : out std_logic;
    load_ir              : out std_logic;
    state_out            : out std_logic_vector (2 downto 0));

end state_machine;

architecture behavioural of state_machine is
  signal state : std_logic_vector (2 downto 0) := "000";
begin  -- behavioural
  state_out <= state;

  -- purpose: This implements the state machine
  -- type   : sequential
  -- inputs : clk, instruction
  -- outputs: state
  process (clk)
  begin  -- process
    if clk = '1' then
      case state is
        when "000" =>
          state <= "001";
          get_next_instruction <= '0';
        when "001" => state <= "010";
        when "010" =>                   -- Filter out type 1's
          if (instruction(15 downto 11) = "01000") or (instruction(15 downto 11) = "11001") then
            state <= "000";
            get_next_instruction <= '1';
          else
            state <=  "011";
          end if;
        when "011" => state <= "100";
        when "100" =>                   -- Filter out type 2's
          if (instruction(15 downto 11) = "11000") or (instruction(15 downto 11) = "11010") then
            state <= "000";
            get_next_instruction <= '1';
          else
            state <= "101";
          end if;
        when others =>
          state <= "000";  -- Must be type 3's
          get_next_instruction <= '1';
      end case;

      case state is
        when "000" =>
          load_ir <= '1';
        when "001" => load_ir <= '0';
        when others => null;
      end case;
    end if;
  end process;

end behavioural;

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity controller is
  
  port (
    -- INPUT
    clk               : in  std_logic;
    instruction_in    : in  std_logic_vector (15 downto 0); -- from mem
    datapath          : in  std_logic_vector (7 downto 0);  -- from datapath
    -- OUTPUT
    get_next, set_PC  : out std_logic;                      -- to mem
    set_PC_to         : out std_logic_vector (7 downto 0);
    write             : out std_logic;                      -- to datapath
    datapath_in       : out std_logic_vector (7 downto 0);
    mux_sel, funct    : out std_logic_vector (1 downto 0);
    load_xyz          : out std_logic_vector (2 downto 0);
    write_to, read_to : out std_logic_vector (2 downto 0));

end controller;

architecture control of controller is
  signal load_ir : std_logic;
  signal instruction : std_logic_vector (15 downto 0);
  signal state : std_logic_vector (2 downto 0);
begin  -- control

  p1 : inst_reg port map (
    instruction_in => instruction_in,
    load_ir        => load_ir,
    clk            => clk,
    instruction    => instruction);

  p2 : decode port map (
    instruction => instruction,
    state       => state,
    datapath_in => datapath_in,
    mux_sel     => mux_sel,
    load_xyz    => load_xyz,
    funct       => funct,
    write       => write,
    write_to    => write_to,
    read_to     => read_to,
    set_PC      => set_PC,
    set_PC_to   => set_PC_to,
    datapath    => datapath);

  p3 : state_machine port map (
    instruction          => instruction,
    clk                  => clk,
    get_next_instruction => get_next_instruction,
    load_ir              => load_ir,
    state_out            => state);
  
end control;
