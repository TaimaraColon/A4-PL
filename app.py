import ply.lex as lex
import ply.yacc as yacc
import time
import json
import hashlib
import sys
from io import StringIO 
from typing import List, Dict, Any, Tuple
from flask import Flask, render_template, request, jsonify

# ==============================================================================
# 1. LEXICAL ANALYZER DEFINITION
# ==============================================================================

tokens = ['BLOCK','ADD','PRINT','VIEW',
          'RUN','MINE','EXPORT','STR','INT',
          'LONG','FLOAT','LIST','TUPLE','DICT',
          'ID','STRING','NUMBER','ASSIGN','TYPEASSIGN',
          'SEPARATOR','LPAREN','RPAREN']

keywords = {'block':'BLOCK','add':'ADD','print':'PRINT',
            'view':'VIEW','run':'RUN','mine':'MINE',
            'export':'EXPORT','str':'STR','int':'INT',
            'long':'LONG','float':'FLOAT','List':'LIST',
            'Tuple':'TUPLE','Dict':'DICT'}

t_ignore = ' \t\r' 

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_COMMENT(t):
    r'//[^\n]*'
    pass

t_SEPARATOR = r','
t_ASSIGN = r'='
t_TYPEASSIGN = r':'
t_LPAREN = r'\('
t_RPAREN = r'\)'

def t_ID(t):
    r'[A-Za-z][A-Za-z0-9_]*'
    t.type = keywords.get(t.value, 'ID') 
    return t

def t_STRING(t):
    r'"[^"\n]*"'
    t.value = t.value[1:-1]
    return t

def t_NUMBER(t):
    r'(\d+\.\d+) | (\.\d+) | (\d+)'
    
    # PLY tokenization error fix: use only the matched value, remove spaces
    val = t.value.replace(' ', '')

    if '.' in val:
        t.value = float(val)
    else:
        t.value = int(val)
    return t

def t_error(t):
    global captured_output
    captured_output.write(f'Illegal character {t.value[0]!r} in row {t.lexer.lineno}\n')
    t.lexer.skip(1)

# ==============================================================================
# 2. SYNTAX ANALYSIS DEFINITION (PLYA/YACC) - MODIFIED FOR MULTIPLE BLOCKS
# ==============================================================================

def p_start(p):
    'start : statements'
    p[0] = p[1]

def p_statements(p):
    '''statements : statement
                  | statements statement'''
    if len(p) == 2: 
        p[0] = [p[1]]
    else: 
        p[0] = p[1] + [p[2]] 

def p_statement(p):
    '''statement : block_definition
                 | block_operation'''
    p[0] = p[1]

def p_block_definition(p):
    'block_definition : BLOCK ID ASSIGN LPAREN attributes RPAREN'
    # Renamed output type to distinguish from operations
    p[0] = ('block_def', p[2], p[5]) 

def p_block_operation(p):
    '''block_operation : ADD ID ASSIGN LPAREN new_atts RPAREN
                       | PRINT ID 
                       | RUN ID
                       | MINE ID
                       | EXPORT ID
                       | VIEW ID'''
    block_name = p[2]
    if p[1] == "add":
        p[0] = ("AddOp", block_name, p[5])
    elif p[1] == "print":
        p[0] = ("PrintOp", block_name)
    elif p[1] == "run":
        p[0] = ("RunOp", block_name)
    elif p[1] == "export":
        p[0] = ("ExportOp", block_name)
    elif p[1] == "view":
        p[0] = ("ViewOp", block_name)
    elif p[1] == "mine":
        p[0] = ("MineOp", block_name)

def p_type(p):
    '''type : STR
            | INT
            | LONG
            | FLOAT
            | LIST
            | TUPLE
            | DICT'''
    p[0] = p[1]

def p_attribute(p):
    'attribute : ID TYPEASSIGN type'
    p[0] = (p[1], p[3])

def p_attributes(p):
    '''attributes : attribute 
                  | attributes SEPARATOR attribute'''
    if len(p) == 4:
        p[0] = p[1] + [p[3]]
    else:
        p[0] = [p[1]]

def p_new_att(p):
    '''new_att : ID TYPEASSIGN STRING
               | ID TYPEASSIGN NUMBER'''
    p[0] = (p[1], p[3])

def p_new_atts(p):
    '''new_atts : new_att
                | new_atts SEPARATOR new_att'''
    if len(p) == 4:
        p[0] = p[1]+[p[3]]
    else:
        p[0] = [p[1]]

def p_error(p):
    global captured_output
    if p is None:
        captured_output.write("Syntax error at EOF\n")
    else:
        captured_output.write(f"Syntax error at {p.value!r} (line {p.lineno})\n")

# ==============================================================================
# 3. BLOCKCHAIN CLASS AND SEMANTIC FUNCTIONS
# ==============================================================================

class Blockchain:
    def __init__(self, block_name: str, schema: Dict[str, str]):
        self.name = block_name
        self.schema = schema
        self.chain: List[Dict[str, Any]] = []
        self.current_data: List[Dict[str, Any]] = []
        
        self.new_block(proof='1', previous_hash='1')
        print(f" Initialized Blockchain '{self.name}' and created Block.")

    def hash(self, block: Dict[str, Any]) -> str:
        block_string = json.dumps(block, sort_keys=True).encode()
        return hashlib.sha256(block_string).hexdigest()

    def new_block(self, proof: str, previous_hash: str = None) -> Dict[str, Any]:
        block = {
            'index': len(self.chain),
            'timestamp': time.time(),
            'data': self.current_data,
            'proof': proof,
            'previous_hash': previous_hash or self.hash(self.chain[-1]) 
  
        }
        
        self.current_data = [] 
        self.chain.append(block)
        return block

    def add(self, data: Dict[str, Any]) -> None:
        self.current_data.append(data)

    def mine(self) -> Dict[str, Any]:
        proof = "simple_proof_" + str(time.time()) 
        last_hash = self.hash(self.chain[-1])
        new_block = self.new_block(proof, last_hash)
        return new_block
    
    def print(self):
        print(f"\n--- Block: {self.name}---")
        if not self.chain:
            print("Chain is empty.")
            return

        # Get the latest block
        block = self.chain[-1]
        
        # Calculate the hash for the latest block
        block_hash = self.hash(block) 
        
        # Convert the block to a readable JSON string
        block_str = json.dumps(block, indent=4) 
        
        # Print the block data
        print(block_str)
        
        # Explicitly display the hash of this block
        print(f"Hash: {block_hash}\n")

    def export(self, filename: str):
        with open(filename, 'w') as f:
            json.dump(self.chain, f, indent=4)
        print(f" Exported blockchain '{self.name}' to {filename}.")
        
    def run(self):
        print(f" Displaying chain: ")
        for block in self.chain:
            # Calculate the hash for the current block
            block_hash = self.hash(block) 
            
            # Convert the block to a readable JSON string
            block_str = json.dumps(block, indent=4) 
            
            # Print the block data
            print(block_str)
            
            # Explicitly display the hash of this block
            print(f"Hash: {block_hash}\n")

# Global state to hold blockchains
BLOCKCHAINS: Dict[str, Blockchain] = {} 
# Global object to capture output from print statements (<- FIX: Initialization)
captured_output = StringIO() 

def get_py_type(type_str):
    if type_str == 'int': return int
    if type_str == 'str': return str
    if type_str == 'float': return float
    return str 

def do_block_definition(block_name: str, schema_list: List[Tuple[str, str]]):
    if block_name in BLOCKCHAINS:
        print(f"Error: Block '{block_name}' already defined.")
        return
    
    schema = dict(schema_list)
    BLOCKCHAINS[block_name] = Blockchain(block_name, schema)


def do_add_operation(block_name: str, data_list: List[Tuple[str, Any]]):
    if block_name not in BLOCKCHAINS:
        print(f"Error: Block '{block_name}' is not defined.")
        return

    blockchain = BLOCKCHAINS[block_name]
    new_data = {}
    is_valid_transaction = True

    for key, value in data_list:
        if key not in blockchain.schema:
            print(f" Semantic Error: Attribute '{key}' not in schema for '{block_name}'.")
            is_valid_transaction = False
            continue 

        expected_type_str = blockchain.schema[key]
        expected_type_py = get_py_type(expected_type_str)

        if not isinstance(value, expected_type_py):
            print(f" Semantic Error: '{key}' expected {expected_type_str}, got {type(value).__name__}.")
            is_valid_transaction = False

        new_data[key] = value 
    
    if is_valid_transaction:
        blockchain.add(new_data)
        print(f" Added Data to '{block_name}' (Pending Mining).")
    else:
        print(f" Transaction on '{block_name}' rejected due to semantic error.")
    

def do_mine_operation(block_name: str):
    if block_name not in BLOCKCHAINS:
        print(f"Error: Block '{block_name}' is not defined.")
        return
        
    blockchain = BLOCKCHAINS[block_name]
    new_block = blockchain.mine()
    print(f" Block '{new_block['index']}' mined and chained in '{block_name}'.")


def do_print_operation(block_name: str):
    if block_name not in BLOCKCHAINS:
        print(f"Error: Block '{block_name}' is not defined.")
        return

    BLOCKCHAINS[block_name].print()


def do_export_operation(block_name: str):
    if block_name not in BLOCKCHAINS:
        print(f"Error: Block '{block_name}' is not defined.")
        return
    
    filename = f"{block_name}_blockchain.json"
    BLOCKCHAINS[block_name].export(filename)


def do_run_operation(block_name: str):
    if block_name not in BLOCKCHAINS:
        print(f"Error: Block '{block_name}' is not defined.")
        return
    
    BLOCKCHAINS[block_name].run()

# ==============================================================================
# 4. EXECUTION DISPATCHER & FLASK INTEGRATION (MODIFIED)
# ==============================================================================

def execute_command(code: str) -> str:
    """ Parses, executes, and captures the output of a single command block. """
    
    global captured_output
    old_stdout = sys.stdout
    sys.stdout = captured_output
    
    try:
        # The parser now returns a list of statements (definitions or operations)
        ast = parser.parse(code, lexer=lexer)

        if ast is None:
            # Syntax error handling is done in p_error
            return captured_output.getvalue()

        # Process all statements in the AST
        for statement in ast:
            op_type = statement[0]
            block_name = statement[1]
            
            if op_type == "block_def": # Handle the block definition statement
                schema_list = statement[2]
                do_block_definition(block_name, schema_list)
            
            elif op_type == "AddOp":
                do_add_operation(block_name, statement[2])
            elif op_type == "MineOp":
                do_mine_operation(block_name)
            elif op_type == "PrintOp" or op_type == "ViewOp":
                do_print_operation(block_name)
            elif op_type == "ExportOp":
                do_export_operation(block_name)
            elif op_type == "RunOp":
                do_run_operation(block_name)
        
    except Exception as e:
        # Catch unexpected Python errors during execution
        print(f"\nExecution Error: {type(e).__name__}: {e}")
    
    finally:
        output = captured_output.getvalue()
        captured_output = StringIO() # Reset for next command
        sys.stdout = old_stdout
    
    return output


# Initialize Lexer and Parser globally
lexer = lex.lex()
parser = yacc.yacc()
app = Flask(__name__)


@app.route('/')
def index():
    """Renders the main command prompt UI."""
    return render_template('index.html')

@app.route('/execute', methods=['POST'])
def handle_command():
    """Endpoint to receive and execute the command from the UI."""
    data = request.get_json()
    command_input = data.get('command', '')

    # Execution logic handles the parsing, semantics, and output capture
    output = execute_command(command_input)
    
    return jsonify({'output': output})


if __name__ == '__main__':
    # Run the Flask app
    app.run(debug=True)