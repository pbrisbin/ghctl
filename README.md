# ghctl

CLI for maintaining GitHub settings as [IaC][].

[iac]: https://en.wikipedia.org/wiki/Infrastructure_as_code

![](./files/example.png)

## Installation

```console
TODO
```

## Usage

```console
% ghctl --help
Usage: ghctl [-p|--path FILE] [--apply] [--fail-on-diff]
             [--fail-on-diff-exit-code NUMBER] [OWNER/NAME]

  Maintain GitHub settings

Available options:
  -p,--path FILE           Path to repositories definition file
                           (default: repositories.yaml)
  --apply                  Apply changes to make current state look like desired
  --fail-on-diff           Fail if there are un-applied differences
  --fail-on-diff-exit-code NUMBER
                           Exit code for --fail-on-diff (default: 228)
  OWNER/NAME               Limit processing to the given repositories
  -h,--help                Show this help text
```

## `repositories.yaml` Schema

The `repositories.yaml` file must contain a `repositories` key, and optionally a
`defaults` key.

These are treated as schema-less values for the purpose of applying `defaults`
to each element in the `repositories` list. Defaults are applied as a recursive
merge, biased towards the `repositories` value.

**After defaults are applied**, each value must satisfy the [`Repository`
schema](./repository.schema.json).

## Examples

I've started documenting my own repositories [here](./repositories.yaml).

## License

This project is licensed AGPLv3. See [COPYING](./COPYING).
