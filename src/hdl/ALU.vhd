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
    
    component Bitcompare is
        port(
            -- Switches
            i_1, i_2: in std_logic_vector(7 downto 0);
            -- LEDs
            o_Sum_AND: out std_logic_vector(7 downto 0);
            o_Sum_OR: out std_logic_vector(7 downto 0)
          --  o_CarryOut: out std_logic
        );
    end component Bitcompare;
    
    component shifty is
     Port(
        i_A : std_logic_vector( 7 downto 0);
        i_B : std_logic_vector( 7 downto 0);
        i_D : std_logic;
        o_shifted : std_logic_vector( 7 downto 0)
     );
    end component shifty;
    
   signal w_operand1 : std_logic_vector( 7 downto 0);
   signal w_operand2_pos : std_logic_vector( 7 downto 0);

   signal w_and : std_logic_vector( 7 downto 0);
   signal w_or : std_logic_vector( 7 downto 0);
   signal w_add : std_logic_vector( 7 downto 0);
   signal w_neg : std_logic_vector( 7downto 0);
   signal w_shifted : std_logic_vector( 7 downto 0);
   
   signal w_flag_C :std_logic;
   signal w_flag_Z : std_logic;
   signal w_flag_neg : std_logic;
begin
	-- PORT MAPS ----------------------------------------
	 FA_inst : FullAdder port map (
           i_1 => w_operand1,
           i_2 => w_operand2_pos,
           o_Sum => w_add, -- added value
           o_CarryOut => w_flag_C -- Set the carry-out flag
       );
	
	
	
          
     AND_OR_inst: Bitcompare port map (
        i_1 => w_operand1,
        i_2 => w_operand2_pos,
        o_Sum_AND => w_and,
        o_Sum_OR => w_or
        );
            
	shift_inst : shifty port map(
	   i_A => w_operand1,
	   i_B => w_operand2_pos,
	   i_D => i_op(2),
	   o_shifted => w_shifted
	
	);
	-- CONCURRENT STATEMENTS ----------------------------
	
	
	add_sub_AO : process (i_op, i_B, i_A)
        begin
        if i_op = "010" then  -- Subtract
                w_neg <= i_B;
                w_operand2_pos <= std_logic_vector(unsigned(not w_neg) + 1);
                w_add <= o_op_result;
                if i_A = i_B then
                    w_flag_Z <= '1';
                else
                    w_flag_Z <= '0';
                end if;
                
            elsif i_op = "011" then -- Shift left
                w_shifted <= o_op_result;
                
            elsif i_op = "100" then -- Shift right
                w_shifted <= o_op_result;
                
            elsif i_op = "101" then -- Bitwise AND
                w_add <= o_op_result;
                
            elsif i_op = "110" then -- Bitwise OR
                w_or <= o_op_result;
                
            else  -- Default case: Addition
                w_operand2_pos <= i_B;
                w_add <= o_op_result;
            end if;
    end process add_sub_AO;
    w_flag_neg <= o_op_result(7);   
    w_flag_C <= o_flag_C;
    w_flag_neg <= o_flag_S;
    w_flag_Z <= o_flag_Z;
    
end behavioral;
