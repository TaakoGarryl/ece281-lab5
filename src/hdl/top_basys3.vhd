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
library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;


entity top_basys3 is port(
    seg :   out std_logic_vector(6 downto 0);
    an  :   out std_logic_vector(3 downto 0);
    clk     :   in std_logic;
    sw  	:   in std_logic_vector(15 downto 0);
    led 	:   out std_logic_vector(15 downto 0);
    btnU    :   in std_logic;
    btnC    :   in std_logic
    );
end top_basys3;

architecture top_basys3_arch of top_basys3 is 
  
	-- declare components and signals
    component TDM4 is
        generic ( constant k_WIDTH : natural  := 4); -- bits in input and output
            Port ( i_clk        : in  STD_LOGIC;
                   i_reset        : in  STD_LOGIC; -- asynchronous
                   i_D3         : in  STD_LOGIC_VECTOR (k_WIDTH - 1 downto 0);
                   i_D2         : in  STD_LOGIC_VECTOR (k_WIDTH - 1 downto 0);
                   i_D1         : in  STD_LOGIC_VECTOR (k_WIDTH - 1 downto 0);
                   i_D0         : in  STD_LOGIC_VECTOR (k_WIDTH - 1 downto 0);
                   o_data        : out STD_LOGIC_VECTOR (k_WIDTH - 1 downto 0);
                   o_sel        : out STD_LOGIC_VECTOR (3 downto 0)    -- selected data line (one-cold)
            );
        end component TDM4;
        
        
    component sevenSegDecoder is
        Port ( 
              i_D : in STD_LOGIC_VECTOR (3 downto 0);
              o_S : out STD_LOGIC_VECTOR (6 downto 0) );
        end  component sevenSegDecoder;
        
    component twoscomp_decimal is
            port (
                i_binary: in std_logic_vector(7 downto 0);
                o_negative: out std_logic;
                o_hundreds: out std_logic_vector(3 downto 0);
                o_tens: out std_logic_vector(3 downto 0);
                o_ones: out std_logic_vector(3 downto 0)
            );
        end component twoscomp_decimal;  
       
        
    component clock_divider is
    generic ( constant k_DIV : natural := 50000000	); -- How many clk cycles until slow clock toggles Goal HZ of 2
                port (
                        i_clk    : in std_logic;
                        i_reset  : in std_logic;           -- asynchronous
                        o_clk    : out std_logic           -- divided (slow) clock
                );
            end component clock_divider;
            
    component ALU is
                Port(
                    i_A : std_logic_vector( 7 downto 0);
                    i_B : std_logic_vector( 7 downto 0);
                    i_op : std_logic_vector( 2 downto 0);
                    o_op_result : std_logic_vector( 7 downto 0);
                    o_flag_C : std_logic := '0';
                    o_flag_Z : std_logic := '0';
                    o_flag_S : std_logic := '0'
                    );
            end component ALU;

            
    
    signal w_A : std_logic_vector(7 downto 0);
    signal w_B : std_logic_vector(7 downto 0);
    signal w_state : std_logic_vector(3 downto 0);
    signal w_op : std_logic_vector(2 downto 0);
    signal w_flag : std_logic_vector(2 downto 0);
    signal w_op_result : std_logic_vector( 7 downto 0);
    
    signal w_bin : std_logic_vector( 7 downto 0);
    signal w_sign :std_logic; 
    signal w_hund :std_logic_vector( 3 downto 0);
    signal w_ten : std_logic_vector( 3 downto 0); 
    signal w_one : std_logic_vector( 3 downto 0); 
    
    signal w_display_num : std_logic_vector(3 downto 0);
    signal w_display_light : std_logic;          
    signal w_clk : std_logic;
    signal w_reset : std_logic;
    
    signal f_Q : std_logic_vector( 2 downto 0) := "000";
    signal f_Q_next:  std_logic_vector( 2 downto 0) := "000";   
begin
	-- PORT MAPS ----------------------------------------
    ALU_inst : ALU
        port map(
            i_A => w_A,
            i_B => w_B,
            i_op => w_op,
            o_op_result => w_op_result,
            o_flag_Z => w_flag(0),
            o_flag_S => w_flag(1),
            o_flag_C => w_flag(2)
	);
	
	twos_inst :  twoscomp_decimal
	   port map(
	     i_binary => w_bin,
	     o_negative => w_sign,
	     o_hundreds => w_hund,
	     o_tens => w_ten,
	     o_ones  => w_one
	);
	
	clk_inst : clock_divider
	   port map(
	       i_clk => clk,
	       i_reset => btnU,
	       o_clk => w_clk
	   );
	   
	   TDM_inst : TDM4
	       port map(
	       i_clk => w_clk,
           i_reset => btnU,
           i_D3 => w_flag,
           i_D2 => w_hund,
           i_D1 => w_ten,
           i_D0 => w_one,
           o_data => w_display_num,
           o_sel => an
           
	       );
	   
	   decode_inst : sevenSegDecoder
	   port map(
	   i_D => w_display_num,
	   o_S => seg
	   );
	-- CONCURRENT STATEMENTS ----------------------------
	f_Q_next(0) <= btnU or (f_Q(3) and btnC);
	f_Q_next(1) <= (f_Q(0) and btnC);
	f_Q_next(2) <= (f_Q(1) and btnC);
	f_Q_next(3) <= (f_Q(2) and btnC);
      	
	
end top_basys3_arch;
