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


entity top_basys3 is
-- TODO
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
            
            
    signal w_display_num : std_logic_vector(3 downto 0);
    signal w_display_light : std_logic;          
    signal w_clk : std_logic;
    signal w_reset_clk : std_logic;
    
            
begin
	-- PORT MAPS ----------------------------------------

	
	
	-- CONCURRENT STATEMENTS ----------------------------
	
	
	
end top_basys3_arch;
