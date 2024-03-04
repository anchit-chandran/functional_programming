whiletrue pred state ff 
                        = whiletrue pred (ff state) ff, if pred state
                        = state, otherwise
