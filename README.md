# Hack-style Type Checker

A functional-style static type checker inspired by HHVM/Hack, implemented in TypeScript.

## Features

- Strong emphasis on immutability and pure functions
- Functional design patterns (pattern matching, algebraic data types)
- Persistent data structures for type environments
- Clear type error messages with context
- Support for optional types and basic subtype compatibility

## Type System

The type checker supports the following types:
- Primitive types: `int`, `string`, `bool`, `float`, `void`, `null`
- Optional types: `?T`
- List types: `List<T>`
- Map types: `Map<K, V>`
- Function types: `function(T1, ..., Tn): R`

## Project Structure

```
src/
  ├── types.ts     # Core type definitions
  ├── ast.ts       # Abstract Syntax Tree definitions
  └── checker.ts   # Type checking logic (coming soon)
```

## Setup

1. Install dependencies:
   ```bash
   npm install
   ```

2. Build the project:
   ```bash
   npm run build
   ```

3. Run type checking:
   ```bash
   npm run typecheck
   ```

## Development

The project uses:
- TypeScript for type safety
- Immutable.js for persistent data structures
- Jest for testing

## License

MIT 