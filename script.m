switch ::= On | Off

show_message :: switch -> [char]
show_message On = show "Light is on!"
show_message Off = show "Light is off."
    
