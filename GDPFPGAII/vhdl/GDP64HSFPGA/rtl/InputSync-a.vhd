-- -----------------------------------------------
-- Title:    Synchronisierung eines async. Inputs, 2- Stufig
-- file:     dff.vhd
-- language: VHDL 93
-- author:       HSSE / Andreas Voggeneder
-- comments: 
-- history: 
--   11.2001 creation
-- -----------------------------------------------

architecture rtl of InputSync is
  signal q1 : std_ulogic_vector(levels_g-1 downto 0);
begin
  process (clk, clr_n)
  begin
    if clr_n = '0' then
      q1 <= (others => ResetValue_g);       -- Reset Condition
    elsif clk'event and clk = '1' then
      q1(0) <= Input;                       -- D Pegel latchen
      for i in 1 to levels_g-1 loop
        q1(i)<=q1(i-1);
      end loop;
    end if;
  end process;
  
  q<=q1(levels_g-1);
end rtl;



