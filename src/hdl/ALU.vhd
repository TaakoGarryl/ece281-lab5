--+----------------------------------------------------------------------------
--|
--| NAMING CONVENSIONS :
--|
--|    xb_<port name>           = off-chip bidirectional port ( _pads file )
--|    xi_<port name>           = off-chip input port         ( _pads file )
--|    xo_<port name>           = off-chip output port        ( _pads file )
--|    b_<port name>            = on-chip bidirectional port
--|    i_<port name>            = on-chip input port
--|    o_<port name>            = on-chip output port
--|    c_<signal name>          = combinatorial signal
--|    f_<signal name>          = synchronous signal
--|    ff_<signal name>         = pipeline stage (ff_, fff_, etc.)
--|    <signal name>_n          = active low signal
--|    w_<signal name>          = top level wiring signal
--|    g_<generic name>         = generic
--|    k_<constant name>        = constant
--|    v_<variable name>        = variable
--|    sm_<state machine type>  = state machine type definition
--|    s_<signal name>          = state name
--|
--+----------------------------------------------------------------------------
--|
--| ALU OPCODES:
--|
--|     ADD     001  
--|     SUB     010  
--|     Shift_L 011  
--|     Shift_R 100  
--|     Bit_and 101  
--|     Bit_or  110
--+----------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ALU is
    Port(
        i_A : in std_logic_vector(7 downto 0);
        i_B : in std_logic_vector(7 downto 0);
        i_op : in std_logic_vector(2 downto 0);
        o_op_result : out  std_logic_vector(7 downto 0);
        o_flag_C : out std_logic;
        o_flag_Z : out std_logic;
        o_flag_S : out std_logic
    );
end ALU;

architecture behavioral of ALU is
    signal w_and, w_or, w_shifted : std_logic_vector(7 downto 0);
    signal w_add, w_neg : std_logic_vector( 7downto 0);
    signal w_flag_Z, w_flag_neg : std_logic;
    signal w_chosen : std_logic_vector( 7 downto 0) := "00000001";
    signal w_add_carry : std_logic;
    signal w_sub_carry : std_logic;
    
  --  component FullAdder is
    --    port(
     --       i_1, i_2 : in std_logic_vector(7 downto 0);
    --        i_op : in std_logic_vector( 2 downto 0);
       --     o_Sum : out std_logic_vector(7 downto 0);
     --       o_CarryOut : out std_logic
   --     );
 --   end component FullAdder;

    component Bitcompare is
        port(
            i_1, i_2 : in std_logic_vector(7 downto 0);
            o_Sum_AND : out std_logic_vector(7 downto 0);
            o_Sum_OR : out std_logic_vector(7 downto 0)
        );
    end component Bitcompare;

    component shifty is
        Port(
            i_A : in std_logic_vector(7 downto 0);
            i_B : in std_logic_vector(7 downto 0);
            i_D : in std_logic;
            o_shifted : out std_logic_vector(7 downto 0)
        );
    end component shifty;

begin
   -- FA_inst : FullAdder port map (
     --   i_1 => i_A,
       -- i_2 => i_B,
    --    i_op => i_op,
      --  o_Sum => w_add,
     --   o_CarryOut => w_flag_C
  --  );

    AND_OR_inst : Bitcompare port map (
        i_1 => i_A,
        i_2 => i_B,
        o_Sum_AND => w_and,
        o_Sum_OR => w_or
    );

    shift_inst : shifty port map(
        i_A => i_A,
        i_B => i_B,
        i_D => i_op(2),
        o_shifted => w_shifted
    );

    alu_operation : process(i_op, i_A, i_B, w_neg, w_shifted, w_and, w_or, w_add)
    begin
        case i_op is
            when "001" =>
                w_chosen <= std_logic_vector(signed(i_A) + signed(i_B));
                if (i_A(6) = '1' and i_B(6) = '1') or  
                       (i_A(6) = '1' and w_chosen(6) = '0') or  
                       (i_B(6) = '1' and w_chosen(6) = '0') then 
                        -- Carry occurred
                        w_add_carry <= '1';
                    else
                        -- No carry occurred
                        w_add_carry <= '0';
                    end if;
            when "010" => -- Subtract
                w_neg <= std_logic_vector(signed(not i_B) + 1);
                
                w_chosen <= std_logic_vector(signed(i_A) + signed(w_neg));
               if signed(w_chosen) > signed(i_A) then
                        -- Carry occurred
                        w_sub_carry <= '1';
                    else
                        -- No carry occurred
                       w_sub_carry <= '0';
                    end if;
            when "011" => -- Shift left
                w_chosen <= w_shifted;
            when "100" => -- Shift right
                w_chosen <= w_shifted;
            when "101" => -- Bitwise AND
                w_chosen <= w_and;
            when "110" => -- Bitwise OR
                w_chosen <= w_or;
            when others => -- Default case: Addition
                w_chosen <= w_add;
        end case;
    end process alu_operation;
    
    o_op_result <= w_chosen;

    w_flag_Z <= '1' when  (w_chosen = "00000000")  else '0';
    w_flag_neg <= w_chosen(7) or i_A(7) or i_B(7);
    o_flag_C <= w_add_carry or w_sub_carry;
    o_flag_S <= w_flag_neg;
    o_flag_Z <= w_flag_Z;
end behavioral;
