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
        i_A : std_logic_vector( 7 downto 0);
        i_B : std_logic_vector( 7 downto 0);
        i_op : std_logic_vector( 2 downto 0);
        o_op_result : std_logic_vector( 7 downto 0);
        o_flag_C : std_logic;
        o_flag_Z : std_logic;
        o_flag_S : std_logic
        );
end ALU;

architecture behavioral of ALU is 
     
	-- declare components and signals
    component FullAdder is
        port(
           i_1, i_2: in std_logic_vector(7 downto 0);
                -- LEDs
                o_Sum: out std_logic_vector(7 downto 0);
                o_CarryOut: out std_logic
        );
    end component FullAdder;
    
    component twoscomp_decimal is
        port (
            i_binary: in std_logic_vector(7 downto 0);
            o_negative: out std_logic;
            o_hundreds: out std_logic_vector(3 downto 0);
            o_tens: out std_logic_vector(3 downto 0);
            o_ones: out std_logic_vector(3 downto 0)
        );
    end component twoscomp_decimal;
    
   signal w_operand1 : std_logic_vector( 7 downto 0);
   signal w_operand2_pos : std_logic_vector( 7 downto 0);
   signal w_operand2_neg : std_logic_vector( 7 downto 0);
   signal w_result : std_logic_vector( 7 downto 0);
   signal w_neg : std_logic;
   signal w_flag_C :std_logic;
   signal w_flag_Z : std_logic;
begin
	-- PORT MAPS ----------------------------------------
	 FA_inst : FullAdder port map (
           i_1 => w_operand1,
           i_2 => w_operand2_pos,
           o_Sum => w_result, -- added value
           o_CarryOut => w_flag_C -- Set the carry-out flag
       );
	
	
	 SUB_inst: twoscomp_decimal port map (
              i_binary => w_operand2_neg,
              o_negative => w_neg, -- Output negative flag
              o_hundreds => open, -- Unused for now
              o_tens => open,     -- Unused for now
              o_ones => open      -- Unused for now
          );
	
	-- CONCURRENT STATEMENTS ----------------------------
	
	
	add_sub : process (i_op, i_B, i_A)
        begin
        if i_op(1) = '1' then
             w_operand2_pos  <= w_operand2_neg;
             if i_A = i_B then
                w_flag_Z <= '1';
             end if;
        else
            w_operand2_pos <= i_B;
        end if;
    end process add_sub;
        
    w_flag_C <= o_flag_C;
    w_result <= o_op_result;
    w_flag_Z <= o_flag_Z;
end behavioral;
