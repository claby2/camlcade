# 🐫🎮 camlcade

> [!WARNING]
> Still in development. Not ready for production.

camlcade is an OCaml game engine. It features an archetype-based entity-component system (ECS) and an OpenGL-based renderer.

## Features

- Archetype storage
- Composable plugins
- User-defined GLSL shaders
- Custom meshes

## Examples

To see how to use camlcade, see [examples/](examples/).

Run an example with:
```sh
# Runs the "shapes" example
dune exec shapes
```

https://github.com/user-attachments/assets/8d5ea29c-ec0d-452f-b60a-a0c7129d0e10

https://github.com/user-attachments/assets/95a5e881-ff3a-4c3d-b545-769d68b85b8e

## Development

### Project Structure

```
lib
├── ecs       # Entity-component system
├── graphics  # OpenGL-based renderer
├── input     # Input and event handling
├── math      # Math utilities
├── storage   # Sparse storage
└── transform # Transform component
```

### Quick Start

- Build: `dune build`
- Test: `dune test`
- Benchmark: `dune exec bench`
- Documentation: `dune build @doc` (see `_build/default/_doc/_html/index.html`)
