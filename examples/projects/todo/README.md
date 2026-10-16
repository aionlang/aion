# todo

A to-do list for the command line, stored in a JSON file. It shows a project with several modules:

| File | Purpose |
|------|---------|
| `main.aion` | the entry point: option parsing with `cli`, the commands, and a demo session |
| `lib/task.aion` | the `Task` type and its JSON form |
| `lib/store.aion` | the list of tasks, loading and saving with `fs` and `json` |
| `lib/render.aion` | how tasks are drawn on the screen |

```text
cd examples/projects/todo
aion                                   # a demonstration in a temporary folder
aion -- add "write the report"         # a real task, kept in ./todo.json
aion -- -p 1 add "urgent thing"
aion -- list
aion -- done 1
aion -- --help
```

(The `--` keeps `aion` itself from reading the options meant for the program.)
