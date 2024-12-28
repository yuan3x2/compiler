import ply.lex as lex
import ply.yacc as yacc

# =======================
# Lexical Analyzer (Lexer)

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
    r'[a-zA-Z_][a-zA-Z_0-9-]*'
    if t.value in keywords:    
        t.type = keywords[t.value]
    return t

t_ignore = ' \t\n'


def t_error(t):
    print(f"Illegal character '{t.value[0]}'")
    t.lexer.skip(1)

lexer = lex.lex()

# ==========================
# Syntax Analyzer (Parser)
precedence = (
    ('left', 'AND', 'OR'),
    ('left', 'GREATER', 'SMALLER', 'EQUAL'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'MULTIPLY', 'DIVIDE', 'MOD'),
    ('right', 'NOT')
)

env = {}

def p_program(p):
    'program : stmt_list'

def p_stmt_list(p):
    '''stmt_list : stmt
                 | stmt stmt_list'''

def p_stmt(p):
    '''stmt : exp
            | print_stmt
            | define_stmt'''
    
def p_define_stmt(p):
    'define_stmt : LPAREN DEFINE ID exp RPAREN'
    env[p[3]] = p[4]

def p_print_stmt(p):
    '''print_stmt : LPAREN PRINT_NUM exp RPAREN
                  | LPAREN PRINT_BOOL exp RPAREN'''
    if p[2] == 'print-num':
        print(p[3])
    elif p[2] == 'print-bool':
        if p[3] == True:
            print('#t')
        else:
            print('#f')

def p_exp(p):
    '''exp : NUMBER
           | BOOL
           | num_op
           | logical_op
           | if_exp
           | fun_exp
           | fun_call
           | ID'''
    if isinstance(p[1], str) and p[1] in env:
        p[0] = env[p[1]]
    else:
        p[0] = p[1]
    # p[0] = p[1]

def p_exp_list(p):
    '''exp_list : exp
                | exp exp_list'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[2]

def p_num_op(p):
    '''num_op : LPAREN PLUS exp exp_list RPAREN
              | LPAREN MINUS exp exp RPAREN
              | LPAREN MULTIPLY exp exp_list RPAREN
              | LPAREN DIVIDE exp exp RPAREN
              | LPAREN MOD exp exp RPAREN
              | LPAREN GREATER exp exp RPAREN
              | LPAREN SMALLER exp exp RPAREN
              | LPAREN EQUAL exp exp_list RPAREN'''
    if p[2] == '+':
        p[0] = p[3] + sum(p[4])
    elif p[2] == '-':
        p[0] = p[3] - p[4]
    elif p[2] == '*':
        result = p[3]
        for val in p[4]:
            result *= val
        p[0] = result
    elif p[2] == '/':
        p[0] = p[3] // p[4]
    elif p[2] == 'mod':
        p[0] = p[3] % p[4]
    elif p[2] == '>':
        p[0] = p[3] > p[4]
    elif p[2] == '<':
        p[0] = p[3] < p[4]
    elif p[2] == '=':
        flag = True
        for val in p[4]:
            if p[3] != val:
                flag = False
                break
        p[0] = flag

def p_logical_op(p):
    '''logical_op : LPAREN AND exp exp_list RPAREN
                  | LPAREN OR exp exp_list RPAREN
                  | LPAREN NOT exp RPAREN'''
    if p[2] == 'and':
        p[0] = p[3] and all(p[4])   
    elif p[2] == 'or':
        p[0] = p[3] or any(p[4])
    elif p[2] == 'not':
        p[0] = not p[3]

def p_fun_exp(p):
    'fun_exp : LPAREN FUN LPAREN fun_ids RPAREN exp RPAREN'
    p[0] = ('fun', p[4], p[6])
    
def p_fun_ids(p):
    '''fun_ids :
               | id_list'''
    if len(p) == 1:
        p[0] = []
    else:
        p[0] = p[1]

def p_fun_call(p):
    '''fun_call : LPAREN fun_exp exp_list RPAREN
                | LPAREN fun_exp RPAREN
                | LPAREN ID exp_list RPAREN
                | LPAREN ID RPAREN'''
    if isinstance(p[2], tuple) and p[2][0] == 'fun':
        if len(p) == 4:
            body = p[2][2]
        else:
            params = p[2][1]
            body = p[2][2]
            if len(params) != len(p[3]):
                raise ValueError("Incorrect number of arguments")
            local_env = dict(zip(params, p[3]))
            p[0] = eval_exp(body, local_env)
    elif p[2] in env and isinstance(env[p[2]], tuple) and env[p[2]][0] == 'fun':
        params = env[p[2]][1]
        body = env[p[2]][2]
        if len(params) != len(p[3]):
            raise ValueError("Incorrect number of arguments")
        local_env = dict(zip(params, p[3]))
        p[0] = eval_exp(body, local_env)
    else:
        raise ValueError("Undefined function")
        

def p_id_list(p):
    '''id_list : ID
               | ID id_list'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[2]

def p_if_exp(p):
    'if_exp : LPAREN IF exp exp exp RPAREN'
    p[0] = p[4] if p[3] else p[5]

def p_error(p):
    print("Syntax error")

parser = yacc.yacc()

def eval_exp(exp, local_env):
    global env
    env_backup = env.copy()
    env.update(local_env)
    result = parser.parse(exp)
    env = env_backup
    return result

if __name__ == "__main__":
    file = open("public_test_data/07_1.lsp", "r")
    data = file.read()
    if data :
        parser.parse(data)  
        