import { Type } from "./types";

// Expression types
export type Expr =
  | LiteralExpr
  | VarExpr
  | BinaryOpExpr
  | CallExpr;

export type LiteralExpr = {
  kind: "Literal";
  value: number | string | boolean | null;
  location: Location;
};

export type VarExpr = {
  kind: "Var";
  name: string;
  location: Location;
};

export type BinaryOpExpr = {
  kind: "BinaryOp";
  op: string;
  left: Expr;
  right: Expr;
  location: Location;
};

export type CallExpr = {
  kind: "Call";
  fn: Expr;
  args: Expr[];
  location: Location;
};

// Statement types
export type Stmt =
  | VarDeclStmt
  | FunctionDeclStmt
  | ReturnStmt
  | IfStmt;

export type VarDeclStmt = {
  kind: "VarDecl";
  name: string;
  type: Type;
  init: Expr;
  location: Location;
};

export type FunctionDeclStmt = {
  kind: "FunctionDecl";
  name: string;
  params: Array<[string, Type]>;
  returnType: Type;
  body: Stmt[];
  location: Location;
};

export type ReturnStmt = {
  kind: "Return";
  expr: Expr;
  location: Location;
};

export type IfStmt = {
  kind: "If";
  cond: Expr;
  thenBranch: Stmt[];
  elseBranch?: Stmt[];
  location: Location;
};

// Location information for error reporting
export type Location = {
  line: number;
  column: number;
};

// Program is the root AST node
export type Program = {
  statements: Stmt[];
};

// Helper functions to create AST nodes
export const createLiteralExpr = (
  value: number | string | boolean | null,
  location: Location
): LiteralExpr => ({
  kind: "Literal",
  value,
  location,
});

export const createVarExpr = (
  name: string,
  location: Location
): VarExpr => ({
  kind: "Var",
  name,
  location,
});

export const createBinaryOpExpr = (
  op: string,
  left: Expr,
  right: Expr,
  location: Location
): BinaryOpExpr => ({
  kind: "BinaryOp",
  op,
  left,
  right,
  location,
});

export const createCallExpr = (
  fn: Expr,
  args: Expr[],
  location: Location
): CallExpr => ({
  kind: "Call",
  fn,
  args,
  location,
});

export const createVarDeclStmt = (
  name: string,
  type: Type,
  init: Expr,
  location: Location
): VarDeclStmt => ({
  kind: "VarDecl",
  name,
  type,
  init,
  location,
});

export const createFunctionDeclStmt = (
  name: string,
  params: Array<[string, Type]>,
  returnType: Type,
  body: Stmt[],
  location: Location
): FunctionDeclStmt => ({
  kind: "FunctionDecl",
  name,
  params,
  returnType,
  body,
  location,
});

export const createReturnStmt = (
  expr: Expr,
  location: Location
): ReturnStmt => ({
  kind: "Return",
  expr,
  location,
});

export const createIfStmt = (
  cond: Expr,
  thenBranch: Stmt[],
  elseBranch: Stmt[] | undefined,
  location: Location
): IfStmt => ({
  kind: "If",
  cond,
  thenBranch,
  elseBranch,
  location,
}); 