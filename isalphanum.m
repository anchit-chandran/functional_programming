|| isalphanum letter = code('a') - code('a')
|| isalphanum 'b'
isalphanum :: char -> bool
isalphanum letter = (48 <= code(letter) <= 57) 
                        \/ (65 <= code(letter) <= 90)
                        \/ (97 <= code(letter) <= 122)

