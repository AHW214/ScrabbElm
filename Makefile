JS = main.js
ELM = src/Main.elm

$(JS): $(ELM)
	elm make $(ELM) --output=$(JS)

clean:
	rm -f $(JS)