/**
 * Core type definitions for the Hack-style type checker
 */

// Location information for error reporting
export interface Location {
  line: number;
  column: number;
}

// Type definitions
export type Type =
  | { kind: 'int' }
  | { kind: 'string' }
  | { kind: 'bool' }
  | { kind: 'float' }
  | { kind: 'void' }
  | { kind: 'null' }
  | { kind: 'optional'; inner: Type }
  | { kind: 'list'; elementType: Type }
  | { kind: 'map'; keyType: Type; valueType: Type }
  | { kind: 'function'; paramTypes: Type[]; returnType: Type };

// Expression definitions
export type Expr =
  | { kind: 'int'; value: number; loc: Location }
  | { kind: 'string'; value: string; loc: Location }
  | { kind: 'bool'; value: boolean; loc: Location }
  | { kind: 'float'; value: number; loc: Location }
  | { kind: 'null'; loc: Location }
  | { kind: 'var'; name: string; loc: Location }
  | { kind: 'binary'; op: string; left: Expr; right: Expr; loc: Location }
  | { kind: 'call'; name: string; args: Expr[]; loc: Location }
  | { kind: 'list'; elements: Expr[]; loc: Location }
  | { kind: 'map'; entries: Array<{ key: Expr; value: Expr }>; loc: Location };

// Statement definitions
export type Stmt =
  | { kind: 'varDecl'; name: string; type: Type; init: Expr; loc: Location }
  | { kind: 'funcDecl'; name: string; params: Array<[string, Type]>; returnType: Type; body: Stmt[]; loc: Location }
  | { kind: 'if'; condition: Expr; thenBranch: Stmt[]; elseBranch?: Stmt[]; loc: Location }
  | { kind: 'return'; value?: Expr; loc: Location };

// Type environment using immutable Map
export type TypeEnv = {
  variables: Map<string, Type>;
  functions: Map<string, FunctionType>;
};

// Type error with location information
export type TypeError = {
  message: string;
  location: {
    line: number;
    column: number;
  };
};

// Helper functions to create types
export const createPrimitiveType = (name: PrimitiveType["name"]): PrimitiveType => ({
  kind: "Primitive",
  name,
});

export const createOptionalType = (baseType: Type): OptionalType => ({
  kind: "Optional",
  baseType,
});

export const createListType = (elementType: Type): ListType => ({
  kind: "List",
  elementType,
});

export const createMapType = (keyType: Type, valueType: Type): MapType => ({
  kind: "Map",
  keyType,
  valueType,
});

export const createFunctionType = (
  parameterTypes: Type[],
  returnType: Type
): FunctionType => ({
  kind: "Function",
  parameterTypes,
  returnType,
}); 