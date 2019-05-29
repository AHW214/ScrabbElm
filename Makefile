JS = main.js
ELM = src/elm/Main.elm

$(JS): $(ELM)
	elm make $(ELM) --output=$(JS)

clean:
	rm -f $(JS)