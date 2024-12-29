# 🐫🎮 camlcade

> [!WARNING]
> Still in development. Not ready for production.

camlcade is an OCaml game engine. It features an archetype-based entity-component system (ECS) and an OpenGL-based renderer.

To see how to use camlcade, see [examples/](examples/).

## Features

- Archetype storage
- Composable plugins
- User-defined GLSL shaders
- Custom meshes

## Project Structure

```
lib
├── ecs       # Entity-component system
├── graphics  # OpenGL-based renderer
├── input     # Input and event handling
├── math      # Math utilities
├── storage   # Sparse storage (currently unused)
└── transform # Transform component
```
