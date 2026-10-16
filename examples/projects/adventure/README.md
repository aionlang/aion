# adventure

A small text adventure: look around, pick things up, find the key to the locked door, and take the treasure.

* `data/world.json` — the map: rooms, exits, items and locks. Edit it to build your own world.
* `lib/world.aion` — reads the JSON file into `Room` and `World` objects.
* `lib/game.aion` — the rules: what each command does.
* `main.aion` — reads commands from the keyboard and prints what happens.

```text
cd examples/projects/adventure
aion                      # play: look, go north, take key, inventory, drop key, quit
```

The data file is read relative to the folder you run it from.
