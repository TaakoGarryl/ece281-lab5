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
        i_A_B : std_logic_vector( 7 downto 0);
        i_op : std_logic_vector( 2 downto 0);
        o_flag_C : std_logic;
        o_flag_Z : std_logic;
        o_flag_S : std_logic
        );
end ALU;

architecture behavioral of ALU is 
     
	-- declare components and signals
    component FullAdder is
        port(
            -- Switches
            sw        :    in  std_logic_vector(7 downto 0)
            -- LEDs
          --  led        :    out    std_logic_vector(1 downto 0)
        );
    end component FullAdder;
    
   signal w_operand1 : std_logic_vector( 7 downto 0);
   signal w_operand2 : std_logic
begin
	-- PORT MAPS ----------------------------------------
    
	
	
	-- CONCURRENT STATEMENTS ----------------------------
	
	
	
end behavioral;
