import { Type, Expr, Stmt, Location, TypeEnv } from './types';
import { Result, ok, err } from './result';

// Type checking monad
type TypeCheckResult<T> = Result<T, string>;

// Type environment management
class TypeEnvironment {
  private variables: Map<string, Type>;
  private functions: Map<string, Type>;
  private parent: TypeEnvironment | null;

  constructor(parent: TypeEnvironment | null = null) {
    this.variables = new Map();
    this.functions = new Map();
    this.parent = parent;
  }

  lookupVar(name: string): Type | undefined {
    return this.variables.get(name) || this.parent?.lookupVar(name);
  }

  lookupFunc(name: string): Type | undefined {
    return this.functions.get(name) || this.parent?.lookupFunc(name);
  }

  extendVar(name: string, type: Type): void {
    this.variables.set(name, type);
  }

  extendFunc(name: string, type: Type): void {
    this.functions.set(name, type);
  }

  createChild(): TypeEnvironment {
    return new TypeEnvironment(this);
  }
}

// Type compatibility checking
function checkTypeCompatibility(t1: Type, t2: Type, loc: Location): TypeCheckResult<boolean> {
  if (t1 === t2) return ok(true);

  // Optional type compatibility
  if (t1.kind === 'optional' && t2.kind === 'optional') {
    return checkTypeCompatibility(t1.inner, t2.inner, loc);
  }
  if (t1.kind === 'optional') {
    return checkTypeCompatibility(t1.inner, t2, loc);
  }
  if (t2.kind === 'optional') {
    return checkTypeCompatibility(t1, t2.inner, loc);
  }

  // List type compatibility
  if (t1.kind === 'list' && t2.kind === 'list') {
    return checkTypeCompatibility(t1.elementType, t2.elementType, loc);
  }

  // Map type compatibility
  if (t1.kind === 'map' && t2.kind === 'map') {
    const keyCompat = checkTypeCompatibility(t1.keyType, t2.keyType, loc);
    if (!keyCompat.ok) return keyCompat;
    return checkTypeCompatibility(t1.valueType, t2.valueType, loc);
  }

  // Function type compatibility
  if (t1.kind === 'function' && t2.kind === 'function') {
    if (t1.paramTypes.length !== t2.paramTypes.length) {
      return err(`Function parameter count mismatch at ${loc.line}:${loc.column}`);
    }

    // Check parameter types (contravariant)
    for (let i = 0; i < t1.paramTypes.length; i++) {
      const paramCompat = checkTypeCompatibility(t2.paramTypes[i], t1.paramTypes[i], loc);
      if (!paramCompat.ok) return paramCompat;
    }

    // Check return type (covariant)
    return checkTypeCompatibility(t1.returnType, t2.returnType, loc);
  }

  return err(`Type mismatch at ${loc.line}:${loc.column}: ${JSON.stringify(t1)} is not compatible with ${JSON.stringify(t2)}`);
}

// Expression type checking
function typeCheckExpr(expr: Expr, env: TypeEnvironment): TypeCheckResult<Type> {
  switch (expr.kind) {
    case 'int':
      return ok({ kind: 'int' });
    case 'string':
      return ok({ kind: 'string' });
    case 'bool':
      return ok({ kind: 'bool' });
    case 'float':
      return ok({ kind: 'float' });
    case 'null':
      return ok({ kind: 'null' });

    case 'var':
      const varType = env.lookupVar(expr.name);
      if (!varType) {
        return err(`Undefined variable '${expr.name}' at ${expr.loc.line}:${expr.loc.column}`);
      }
      return ok(varType);

    case 'binary':
      const leftType = typeCheckExpr(expr.left, env);
      if (!leftType.ok) return leftType;
      const rightType = typeCheckExpr(expr.right, env);
      if (!rightType.ok) return rightType;

      switch (expr.op) {
        case '+':
        case '-':
        case '*':
        case '/':
        case '%':
          if (leftType.value.kind !== 'int' || rightType.value.kind !== 'int') {
            return err(`Arithmetic operation requires integer operands at ${expr.loc.line}:${expr.loc.column}`);
          }
          return ok({ kind: 'int' });

        case '==':
        case '!=':
        case '<':
        case '>':
        case '<=':
        case '>=':
          if (leftType.value.kind !== rightType.value.kind) {
            return err(`Comparison requires operands of the same type at ${expr.loc.line}:${expr.loc.column}`);
          }
          return ok({ kind: 'bool' });

        default:
          return err(`Unknown operator '${expr.op}' at ${expr.loc.line}:${expr.loc.column}`);
      }

    case 'call':
      const funcType = env.lookupFunc(expr.name);
      if (!funcType) {
        return err(`Undefined function '${expr.name}' at ${expr.loc.line}:${expr.loc.column}`);
      }
      if (funcType.kind !== 'function') {
        return err(`'${expr.name}' is not a function at ${expr.loc.line}:${expr.loc.column}`);
      }

      if (expr.args.length !== funcType.paramTypes.length) {
        return err(`Function '${expr.name}' expects ${funcType.paramTypes.length} arguments, got ${expr.args.length} at ${expr.loc.line}:${expr.loc.column}`);
      }

      for (let i = 0; i < expr.args.length; i++) {
        const argType = typeCheckExpr(expr.args[i], env);
        if (!argType.ok) return argType;

        const paramCompat = checkTypeCompatibility(argType.value, funcType.paramTypes[i], expr.loc);
        if (!paramCompat.ok) return paramCompat;
      }

      return ok(funcType.returnType);

    case 'list':
      if (expr.elements.length === 0) {
        return ok({ kind: 'list', elementType: { kind: 'null' } });
      }

      const firstType = typeCheckExpr(expr.elements[0], env);
      if (!firstType.ok) return firstType;

      for (let i = 1; i < expr.elements.length; i++) {
        const elemType = typeCheckExpr(expr.elements[i], env);
        if (!elemType.ok) return elemType;

        const compat = checkTypeCompatibility(elemType.value, firstType.value, expr.loc);
        if (!compat.ok) return compat;
      }

      return ok({ kind: 'list', elementType: firstType.value });

    case 'map':
      if (expr.entries.length === 0) {
        return ok({ kind: 'map', keyType: { kind: 'null' }, valueType: { kind: 'null' } });
      }

      const firstKeyType = typeCheckExpr(expr.entries[0].key, env);
      if (!firstKeyType.ok) return firstKeyType;
      const firstValueType = typeCheckExpr(expr.entries[0].value, env);
      if (!firstValueType.ok) return firstValueType;

      for (let i = 1; i < expr.entries.length; i++) {
        const keyType = typeCheckExpr(expr.entries[i].key, env);
        if (!keyType.ok) return keyType;
        const valueType = typeCheckExpr(expr.entries[i].value, env);
        if (!valueType.ok) return valueType;

        const keyCompat = checkTypeCompatibility(keyType.value, firstKeyType.value, expr.loc);
        if (!keyCompat.ok) return keyCompat;
        const valueCompat = checkTypeCompatibility(valueType.value, firstValueType.value, expr.loc);
        if (!valueCompat.ok) return valueCompat;
      }

      return ok({ kind: 'map', keyType: firstKeyType.value, valueType: firstValueType.value });
  }
}

// Statement type checking
function typeCheckStmt(stmt: Stmt, env: TypeEnvironment): TypeCheckResult<void> {
  switch (stmt.kind) {
    case 'varDecl':
      const initType = typeCheckExpr(stmt.init, env);
      if (!initType.ok) return initType;

      const compat = checkTypeCompatibility(initType.value, stmt.type, stmt.loc);
      if (!compat.ok) return compat;

      env.extendVar(stmt.name, stmt.type);
      return ok(undefined);

    case 'funcDecl':
      const childEnv = env.createChild();
      for (const [name, type] of stmt.params) {
        childEnv.extendVar(name, type);
      }

      for (const bodyStmt of stmt.body) {
        const result = typeCheckStmt(bodyStmt, childEnv);
        if (!result.ok) return result;
      }

      const funcType: Type = {
        kind: 'function',
        paramTypes: stmt.params.map(([_, type]) => type),
        returnType: stmt.returnType
      };

      env.extendFunc(stmt.name, funcType);
      return ok(undefined);

    case 'if':
      const condType = typeCheckExpr(stmt.condition, env);
      if (!condType.ok) return condType;

      if (condType.value.kind !== 'bool') {
        return err(`If condition must be boolean at ${stmt.loc.line}:${stmt.loc.column}`);
      }

      const thenEnv = env.createChild();
      for (const thenStmt of stmt.thenBranch) {
        const result = typeCheckStmt(thenStmt, thenEnv);
        if (!result.ok) return result;
      }

      if (stmt.elseBranch) {
        const elseEnv = env.createChild();
        for (const elseStmt of stmt.elseBranch) {
          const result = typeCheckStmt(elseStmt, elseEnv);
          if (!result.ok) return result;
        }
      }

      return ok(undefined);

    case 'return':
      if (!stmt.value) {
        return err(`Return statement must have a value at ${stmt.loc.line}:${stmt.loc.column}`);
      }

      const returnType = typeCheckExpr(stmt.value, env);
      if (!returnType.ok) return returnType;

      // TODO: Check against function's return type
      return ok(undefined);
  }
}

// Main type checking function
export function typeCheck(ast: Stmt[]): TypeCheckResult<void> {
  const env = new TypeEnvironment();
  for (const stmt of ast) {
    const result = typeCheckStmt(stmt, env);
    if (!result.ok) return result;
  }
  return ok(undefined);
} 