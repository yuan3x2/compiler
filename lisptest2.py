import ply.lex as lex
import ply.yacc as yacc

# =======================
# Lexical Analyzer (Lexer)

# 不與 ID 重複的關鍵字
keywords = {
    'print-num': 'PRINT_NUM',
    'print-bool': 'PRINT_BOOL',
    'define': 'DEFINE',
    'if': 'IF',
    'fun': 'FUN',
    'and': 'AND',
    'or': 'OR',
    'not': 'NOT',
    'mod': 'MOD',
    '>': 'GREATER',
    '<': 'SMALLER',
    '=': 'EQUAL',
}

tokens = (
    'LPAREN', 'RPAREN', 'NUMBER', 'BOOL', 'ID',
    'PRINT_NUM', 'PRINT_BOOL', 'IF', 'DEFINE', 'FUN',
    'AND', 'OR', 'NOT',
    'GREATER', 'SMALLER', 'EQUAL',
    'PLUS', 'MINUS', 'MULTIPLY', 'DIVIDE', 'MOD', 
)

# Regular expression rules for simple tokens

t_LPAREN = r'\('
t_RPAREN = r'\)'
t_PRINT_NUM = r'print-num'
t_PRINT_BOOL = r'print-bool'
t_DEFINE = r'define'
t_FUN = r'fun'
t_IF = r'if'
t_AND = r'and'
t_OR = r'or'
t_NOT = r'not'
t_PLUS = r'\+'
t_MINUS = r'-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'/'
t_MOD = r'mod'
t_GREATER = r'>'
t_SMALLER = r'<'
t_EQUAL = r'='

def t_NUMBER(t):
    r'-?\d+'
    t.value = int(t.value)
    return t

def t_BOOL(t):
    r'\#t|\#f'
    t.value = True if t.value == '#t' else False
    return t

def t_ID(t):
    r'[a-zA-Z][a-zA-Z_0-9\-]*'
    if t.value in keywords:    
        t.type = keywords[t.value]
    return t

t_ignore = ' \t\n'

def t_error(t):
    print(f"Illegal character '{t.value[0]}'")
    t.lexer.skip(1)

lexer = lex.lex()

# =======================
# AST Node Classes

class ASTNode:
    pass

class NumberNode(ASTNode):
    def __init__(self, value):
        self.value = value

class BoolNode(ASTNode):
    def __init__(self, value):
        self.value = value

class IdentifierNode(ASTNode):
    def __init__(self, name):
        self.name = name
        self.value = None

class DefineNode(ASTNode):
    def __init__(self, identifier, value):
        self.identifier = identifier
        self.value = value

class PrintNode(ASTNode):
    def __init__(self, value, is_num):
        self.value = value
        self.is_num = is_num

class BinaryOpNode(ASTNode):
    def __init__(self, operator, left, right):
        self.operator = operator
        self.left = left
        self.right = right
        self.value = None

class LogicalOpNode(ASTNode):
    def __init__(self, operator, operands):
        self.operator = operator
        self.operands = operands
        self.value = None

class IfNode(ASTNode):
    def __init__(self, condition, true_branch, false_branch):
        self.condition = condition
        self.true_branch = true_branch
        self.false_branch = false_branch
        self.value = None

class FunNode(ASTNode):
    def __init__(self, params, body):
        self.params = params
        self.body = body
        self.value = None

class FunCallNode(ASTNode):
    def __init__(self, function, arguments):
        self.function = function
        self.arguments = arguments
        self.value = None

# =======================
# Syntax Analyzer (Parser)
precedence = (
    ('left', 'AND', 'OR'),
    ('left', 'GREATER', 'SMALLER', 'EQUAL'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'MULTIPLY', 'DIVIDE', 'MOD'),
    ('right', 'NOT')
)

def p_program(p):
    'program : stmt_list'
    p[0] = p[1]

def p_stmt_list(p):
    '''stmt_list : stmt
                 | stmt stmt_list'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[2]

def p_stmt(p):
    '''stmt : exp
            | print_stmt
            | define_stmt'''
    p[0] = p[1]

def p_define_stmt(p):
    'define_stmt : LPAREN DEFINE ID exp RPAREN'
    p[0] = DefineNode(IdentifierNode(p[3]), p[4])

def p_print_stmt(p):
    '''print_stmt : LPAREN PRINT_NUM exp RPAREN
                  | LPAREN PRINT_BOOL exp RPAREN'''
    if p[2] == 'print-num':
        is_num = True
        p[0] = PrintNode(p[3], is_num)
    elif p[2] == 'print-bool':
        is_num = False
        p[0] = PrintNode(p[3], is_num)

def p_exp(p):
    '''exp : NUMBER
           | BOOL
           | num_op
           | logical_op
           | if_exp
           | fun_exp
           | fun_call
           | ID'''
    if isinstance(p[1], int):
        p[0] = NumberNode(p[1])
    elif isinstance(p[1], bool):
        p[0] = BoolNode(p[1])
    elif isinstance(p[1], str):
        p[0] = IdentifierNode(p[1])
    else:
        p[0] = p[1]

def p_num_op(p):
    '''num_op : LPAREN PLUS exp exp_list RPAREN
              | LPAREN MINUS exp exp RPAREN
              | LPAREN MULTIPLY exp exp_list RPAREN
              | LPAREN DIVIDE exp exp RPAREN
              | LPAREN MOD exp exp RPAREN
              | LPAREN GREATER exp exp RPAREN
              | LPAREN SMALLER exp exp RPAREN
              | LPAREN EQUAL exp exp_list RPAREN'''
    p[0] = BinaryOpNode(p[2], p[3], p[4])

def p_logical_op(p):
    '''logical_op : LPAREN AND exp exp_list RPAREN
                  | LPAREN OR exp exp_list RPAREN
                  | LPAREN NOT exp RPAREN'''
    if p[2] == 'not':
        p[0] = LogicalOpNode(p[2], [p[3]])
    else:
        p[0] = LogicalOpNode(p[2], [p[3]] + p[4])

def p_if_exp(p):
    'if_exp : LPAREN IF exp exp exp RPAREN'
    p[0] = IfNode(p[3], p[4], p[5])

def p_fun_exp(p):
    '''fun_exp : LPAREN FUN LPAREN id_list RPAREN exp RPAREN
               | LPAREN FUN LPAREN RPAREN exp RPAREN'''
    if len(p) == 8:
        p[0] = FunNode(p[4], p[6])
    else:   
        p[0] = FunNode([], p[5])

def p_fun_call(p):
    '''fun_call : LPAREN fun_exp exp_list RPAREN
                | LPAREN fun_exp RPAREN
                | LPAREN ID exp_list RPAREN
                | LPAREN ID RPAREN'''
    if len(p) == 5 and isinstance(p[2], FunNode):
        p[0] = FunCallNode(p[2], p[3])
    elif len(p) == 4 and p[2] == 'fun':
        p[0] = FunCallNode(p[2], [])
    elif len(p) == 5:
        p[0] = FunCallNode(IdentifierNode(p[2]), p[3])
    else:
        p[0] = FunCallNode(IdentifierNode(p[2]), [])

def p_id_list(p):
    '''id_list : ID
               | ID id_list'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[2]

def p_exp_list(p):
    '''exp_list : exp
                | exp exp_list'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[2]

def p_error(p):
    print("Syntax error")

parser = yacc.yacc()

env={}
parms = []
args = []

def postorder(node):
    if isinstance(node, list):
        for n in node:
            postorder(n)
    if isinstance(node, NumberNode):
        print("", end="")
    elif isinstance(node, BoolNode):
        print("", end="")
    elif isinstance(node, IdentifierNode):
        if node.name in env:
            node.value = env[node.name]
    elif isinstance(node, DefineNode):
        # print("define")
        postorder(node.identifier)
        postorder(node.value)
        node.identifier.value = node.value.value
        env[node.identifier.name] = node.identifier.value
        
    elif isinstance(node, PrintNode):    
        postorder(node.value)
        if(node.is_num):
            print(node.value.value)
        else:
            if node.value.value:
                print("#t")
            else:
                print("#f")
        # print("print")
    elif isinstance(node, BinaryOpNode):
        if node.left is not None:
            postorder(node.left)
        if node.right is not None:
            postorder(node.right)
        
        if node.operator == '+':
            # print("+")
            if isinstance(node.right, list):
                sum=0
                for num in node.right:
                    # print(num.value)
                    sum += num.value
                node.value = node.left.value + sum
            else:
                node.value = node.left.value + node.right.value
        elif node.operator == '-':
            # print("-")
            node.value = node.left.value - node.right.value
        elif node.operator == '*':
            # print("*")
            if isinstance(node.right, list):
                product = 1
                for num in node.right:
                    product *= num.value
                node.value = node.left.value * product
            else:
                node.value = node.left.value * node.right.value
        elif node.operator == '/':
            # print("/")
            node.value = node.left.value // node.right.value
        elif node.operator == 'mod':
            # print(node.operator)
            node.value = node.left.value % node.right.value
        elif node.operator == '>':
            node.value = node.left.value > node.right.value
        elif node.operator == '<':
            node.value = node.left.value < node.right.value
        elif node.operator == '=':
            if isinstance(node.right, list):
                flag = True
                for num in node.right:
                    if node.left.value != num.value:
                        flag = False
                        break   
                node.value = flag
            else:
                node.value = node.left.value == node.right.value             

    elif isinstance(node, LogicalOpNode):
        # print(node.operator)
        if node.operands is not None:
            for operand in node.operands:
                postorder(operand)

        if node.operator == 'and':
            flag = True
            for operand in node.operands:
                # print(operand.value)
                if flag == False:
                    break
                flag = flag and operand.value
            node.value = flag
        elif node.operator == 'or':
            flag = False
            for operand in node.operands:
                # print(operand.value)
                if flag == True:
                    break
                flag = flag or operand.value
            node.value = flag
        elif node.operator == 'not':
            node.value = not node.operands[0].value

    elif isinstance(node, IfNode):
        # print("if")
        postorder(node.condition)
        postorder(node.true_branch)
        postorder(node.false_branch)
        node.value = node.true_branch.value if node.condition.value else node.false_branch.value

    elif isinstance(node, FunNode):
        # print("fun")
        if len(node.params) > 0:
            parms.clear()
            for param in node.params:
                parms.append(param)
                # print("parm:",param)

            if len(parms) == len(args):
                env_copy = env.copy()
                env.clear()

                for i in range(len(parms)):
                    env[parms[i]] = args[i]
                postorder(node.body)

                env.clear()
                env.update(env_copy)

                node.value = node.body.value
            else:
                node.value = node
                # print(type(node.value))
        else:
            postorder(node.body)
            node.value = node.body.value

    elif isinstance(node, FunCallNode):
        # print("fun_call")
        if isinstance(node.function, FunNode):
            args.clear()
            for arg in node.arguments:
                postorder(arg)
                args.append(arg.value)
                # print("call:",arg.value)
            postorder(node.function)
            node.value = node.function.value
        else:
            args.clear()
            for arg in node.arguments:
                postorder(arg)
                args.append(arg.value)
                # print("call:",arg.value)

            if node.function.name in env:
                # print("env")
                node.function.value = env[node.function.name]
                postorder(node.function.value)
                if(type(node.function.value) == FunNode):
                    node.value = node.function.value.value
                else:
                    node.value = node.function.value
        

if __name__ == "__main__":
    for i in range(1, 9):
        for j in range(1, 3):
            env.clear()
            args.clear()
            parms.clear()
            file = open(f"public_test_data/0{i}_{j}.lsp", "r")
            print(f"public_test_data/0{i}_{j}.lsp")
            data = file.read()
            if data:
                ast = parser.parse(data)
                postorder(ast)
            print("")
    
    # file = open("public_test_data/test.txt", "r")
    # data = file.read()
    # if data:
    #     ast = parser.parse(data)   
    #     postorder(ast)
