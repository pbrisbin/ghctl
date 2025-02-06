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
Usage: ghctl [-p|--path FILE] COMMAND

Available options:
  -p,--path FILE           Path to repositories definition file
                           (default: repositories.yaml)
  -h,--help                Show this help text

Available commands:
  plan                     show differences in desired and current state
  apply                    apply differences to current state
  import                   import repository definitions
```

```console
% ghctl plan --help
Usage: ghctl plan 

  show differences in desired and current state

Available options:
  -h,--help                Show this help text
```

```console
% ghctl apply --help
Usage: ghctl apply 

  apply differences to current state

Available options:
  -h,--help                Show this help text
```

```console
% ghctl import --help
Usage: ghctl import OWNER/REPO [OWNER/REPO]

  import repository definitions

Available options:
  OWNER/REPO               Repository full name
  OWNER/REPO               Repository full name
  -h,--help                Show this help text
```

## Templates

[Example](./repositories.yaml)

## License

This project is licensed AGPLv3. See [COPYING](./COPYING).
