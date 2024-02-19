# Feather

Feather is a WIP toy scripting language meant for use with the [Argus game engine](https://github.com/caseif/Argus).

## Components

Feather will eventually consist of a number of separate components:

- A language specification
- A standard library specification and implementation
- A bytecode specification
- A bytecode compiler
- A VM
- A bytecode interpreter
- Potentially a JIT native compiler

Ideally, the VM will be capable of accepting bytecode directly or generating it on-the-fly from Feather code.

## Design Goals

- Syntax should be as simple as possible while still being expressive
- Optional dynamic typing while retaining support for static typing
- QoL language features:
  - Native tuple types (anonymous structs)
  - Array/map literal syntax
- Language should provide first-class support for domain-specific QoL features like automatic event handler registration
  via type/function attributes
- Binding interface should allow for explicit registration of constructs like namespaces and struct and enum types
  (rather than needing to hack in support, e.g. in liblua)
- Standard library should provide utilities specific to game programming (e.g. vector/matrix types, lerp functions)

## License

All resources and code related to Feather are made available under the LGPLv3 license.
