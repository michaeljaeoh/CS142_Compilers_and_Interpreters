package crux;


import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

import ast.*;

public class Parser {
    public static String studentName = "Michael Oh";
    public static String studentID = "64776531";
    public static String uciNetID = "mjoh";
    
    
    // SymbolTable Management ==========================
    private SymbolTable symbolTable;
    
    private void initSymbolTable()
    {
        symbolTable = new SymbolTable();
    }
    
    private void enterScope()
    {
        symbolTable = new SymbolTable(this.symbolTable);
    }
    
    private void exitScope()
    {
        symbolTable = this.symbolTable.get_parent_table();
    }

    private Symbol tryResolveSymbol(Token ident)
    {
        assert(ident.is(Token.Kind.IDENTIFIER));
        String name = ident.lexeme();
        try {
            return symbolTable.lookup(name);
        } catch (SymbolNotFoundError e) {
            String message = reportResolveSymbolError(name, ident.lineNumber(), ident.charPosition());
            return new ErrorSymbol(message);
        }
    }

    private String reportResolveSymbolError(String name, int lineNum, int charPos)
    {
        String message = "ResolveSymbolError(" + lineNum + "," + charPos + ")[Could not find " + name + ".]";
        errorBuffer.append(message + "\n");
        errorBuffer.append(symbolTable.toString() + "\n");
        return message;
    }

    private Symbol tryDeclareSymbol(Token ident)
    {
        assert(ident.is(Token.Kind.IDENTIFIER));
        String name = ident.lexeme();
        try {
            return symbolTable.insert(name);
        } catch (RedeclarationError re) {
            String message = reportDeclareSymbolError(name, ident.lineNumber(), ident.charPosition());
            return new ErrorSymbol(message);
        }
    }

    private String reportDeclareSymbolError(String name, int lineNum, int charPos)
    {
        String message = "DeclareSymbolError(" + lineNum + "," + charPos + ")[" + name + " already exists.]";
        errorBuffer.append(message + "\n");
        errorBuffer.append(symbolTable.toString() + "\n");
        return message;
    }    

    
// Grammar Rule Reporting ==========================================
    private int parseTreeRecursionDepth = 0;
    private StringBuffer parseTreeBuffer = new StringBuffer();

    public void enterRule(NonTerminal nonTerminal) {
        String lineData = new String();
        for(int i = 0; i < parseTreeRecursionDepth; i++)
        {
            lineData += "  ";
        }
        lineData += nonTerminal.name();
        //System.out.println("descending " + lineData);
        parseTreeBuffer.append(lineData + "\n");
        parseTreeRecursionDepth++;
    }
    
    private void exitRule(NonTerminal nonTerminal)
    {
        parseTreeRecursionDepth--;
    }
    
    public String parseTreeReport()
    {
        return parseTreeBuffer.toString();
    }

// Error Reporting ==========================================
    private StringBuffer errorBuffer = new StringBuffer();
    
    private String reportSyntaxError(NonTerminal nt)
    {
        String message = "SyntaxError(" + lineNumber() + "," + charPosition() + ")[Expected a token from " + nt.name() + " but got " + currentToken.kind() + ".]";
        errorBuffer.append(message + "\n");
        return message;
    }
     
    private String reportSyntaxError(Token.Kind kind)
    {
        String message = "SyntaxError(" + lineNumber() + "," + charPosition() + ")[Expected " + kind + " but got " + currentToken.kind() + ".]";
        errorBuffer.append(message + "\n");
        return message;
    }
    
    public String errorReport()
    {
        return errorBuffer.toString();
    }
    
    public boolean hasError()
    {
        return errorBuffer.length() != 0;
    }
    
    private class QuitParseException extends RuntimeException
    {
        private static final long serialVersionUID = 1L;
        public QuitParseException(String errorMessage) {
            super(errorMessage);
        }
    }
    
    private int lineNumber()
    {
        return currentToken.lineNumber();
    }
    
    private int charPosition()
    {
        return currentToken.charPosition();
    }
// Parser ==========================================
   private Scanner scanner;
   private Token currentToken;
    
   
   public Parser(Scanner scanner)
   {
   	this.scanner = scanner;
   	currentToken = scanner.next();
   }
   
   public ast.Command parse()
    {
        initSymbolTable();
        try {
            return program();
        } catch (QuitParseException q) {
            return new ast.Error(lineNumber(), charPosition(), "Could not complete parsing.");
        }
    }
    
    
    
// Helper Methods ==========================================
    private boolean have(Token.Kind kind)
    {
        return currentToken.is(kind);
    }
    
    private boolean have(NonTerminal nt)
    {
        return nt.firstSet().contains(currentToken.kind());
    }
    
    private boolean accept(Token.Kind kind)
    {
        if (have(kind)) {
            currentToken = scanner.next();
            return true;
        }
        return false;
    }    
    
    private boolean accept(NonTerminal nt)
    {
        if (have(nt)) {
            currentToken = scanner.next();
            return true;
        }
        return false;
    }

    private boolean expect(Token.Kind kind)
    {
        if (accept(kind))
            return true;
        String errormessage = reportSyntaxError(kind);
        throw new QuitParseException(errormessage);
        //return false;
    }
        
    private boolean expect(NonTerminal nt)
    {
        if (accept(nt))
            return true;
        String errorMessage = reportSyntaxError(nt);
        throw new QuitParseException(errorMessage);
        //return false;
    }
     
    private Token expectRetrieve(Token.Kind kind)
    {
        Token tok = currentToken;
        if (accept(kind))
            return tok;
        String errorMessage = reportSyntaxError(kind);
        throw new QuitParseException(errorMessage);
        //return ErrorToken(errorMessage);
    }
        
    private Token expectRetrieve(NonTerminal nt)
    {
        Token tok = currentToken;
        if (accept(nt))
            return tok;
        String errorMessage = reportSyntaxError(nt);
        throw new QuitParseException(errorMessage);
        //return ErrorToken(errorMessage);
    }
   
// Grammar Rules =====================================================
    
    // literal := INTEGER | FLOAT | TRUE | FALSE .
    public ast.Expression literal()
    {
        ast.Expression expr;
        enterRule(NonTerminal.LITERAL);
        
        Token tok = expectRetrieve(NonTerminal.LITERAL);
        expr = Command.newLiteral(tok);
        
        exitRule(NonTerminal.LITERAL);
        return expr;
    }

    // designator := IDENTIFIER { "[" expression0 "]" } .
    public ast.Expression designator()
    {
    	ast.Expression expression;
        enterRule(NonTerminal.DESIGNATOR);
        Token tok = expectRetrieve(Token.Kind.IDENTIFIER);
        Symbol idSymbol = tryResolveSymbol(tok);
        
        expression = new ast.AddressOf(tok.lineNumber(), tok.charPosition(), idSymbol);
        while (accept(Token.Kind.OPEN_BRACKET)) {
            ast.Expression expr0 = expression0();
            expression = new ast.Index(expr0.lineNumber(), expr0.charPosition(), expression, expr0);
            expect(Token.Kind.CLOSE_BRACKET);
        }
        
        exitRule(NonTerminal.DESIGNATOR);
        
        return expression;
    }
    
    // type := IDENTIFIER .
    public void type()
    {
    	enterRule(NonTerminal.TYPE);
    	
    	expect(NonTerminal.TYPE);
    	
    	exitRule(NonTerminal.TYPE);
    }
    
    // call expression := "::" IDENTIFIER "(" expression-list ")" .
    public ast.Call call_expression()
    {
    	ast.Call callExpr;
    	enterRule(NonTerminal.CALL_EXPRESSION);
    	
    	Token callTok = expectRetrieve(Token.Kind.CALL);
    	Token idTok = expectRetrieve(Token.Kind.IDENTIFIER);
    	Symbol callSymbol = tryResolveSymbol(idTok);
    	
    	expect(Token.Kind.OPEN_PAREN);
    	ast.ExpressionList exprList = expression_list();
    	expect(Token.Kind.CLOSE_PAREN);
    	
    	callExpr = new ast.Call(callTok.lineNumber(), callTok.charPosition(), callSymbol, exprList);
    	
    	exitRule(NonTerminal.CALL_EXPRESSION);
    	
    	return callExpr;
    }
    
    
    // op0 := ">=" | "<=" | "!=" | "==" | ">" | "<" .
    public Token op0()
    {
    	enterRule(NonTerminal.OP0);
    	
    	Token op0 = expectRetrieve(NonTerminal.OP0);
    	
    	exitRule(NonTerminal.OP0);
    	
    	return op0;
    }
    
    // op1 := "+" | "-" | "or" .
    public Token op1()
    {
    	enterRule(NonTerminal.OP1);
    	
    	Token op1 = expectRetrieve(NonTerminal.OP1);
    	
    	exitRule(NonTerminal.OP1);
    	
    	return op1;
    }
    
    // op2 := "*" | "/" | "and" .
    public Token op2()
    {
    	enterRule(NonTerminal.OP2);
    	
    	Token op2 = expectRetrieve(NonTerminal.OP2);
    	
    	exitRule(NonTerminal.OP2);
    	
    	return op2;
    }
    
    // expression3 := "not" expression3 | "(" expression0 ")" | designator | call-expression | literal .
    public ast.Expression expression3()
    {
    	ast.Expression expression;
    	enterRule(NonTerminal.EXPRESSION3);
    	
    	if (have(Token.Kind.NOT)){
    		Token notTok = expectRetrieve(Token.Kind.NOT);
    		ast.Expression rhs = expression3();
    		expression = Command.newExpression(rhs, notTok, null);
    	}
    	else if (accept(Token.Kind.OPEN_PAREN)){
    		expression = expression0();
    		expect(Token.Kind.CLOSE_PAREN);
    	}
    	else if (have(NonTerminal.DESIGNATOR)){
    		int lineNum = currentToken.lineNumber();
    		int charPos = currentToken.charPosition();
    		ast.Expression designator = designator();
    		expression = new ast.Dereference(lineNum, charPos, designator);
    	}
    	else if (have(NonTerminal.CALL_EXPRESSION)){
    		expression = call_expression();
    	}
    	else if (have(NonTerminal.LITERAL)){
    		expression = literal();
    	}
    	else{
        	String errormessage = reportSyntaxError(NonTerminal.EXPRESSION3);
        	expression = new ast.Error(currentToken.lineNumber(), currentToken.charPosition(), errormessage);
    	}
    	
    	exitRule(NonTerminal.EXPRESSION3);
    	
    	return expression;
    }
    
    // expression2 := expression3 { op2 expression3 } .
    public ast.Expression expression2()
    {
    	ast.Expression expression;
    	enterRule(NonTerminal.EXPRESSION2);
    	expression = expression3();
    	while (have(NonTerminal.OP2)){
    		ast.Expression lhs = expression;
    		Token op = op2();
    		ast.Expression rhs = expression3();
    		expression = Command.newExpression(lhs, op, rhs);
    	}
    	
    	exitRule(NonTerminal.EXPRESSION2);
    	
    	return expression;
    }
    
    // expression1 := expression2 { op1 expression2 } .
    public ast.Expression expression1()
    {
    	ast.Expression expression;
    	enterRule(NonTerminal.EXPRESSION1);
    	expression = expression2();
    	while (have(NonTerminal.OP1)){
    		ast.Expression lhs = expression;
    		Token op = op1();
    		ast.Expression rhs = expression2();
    		expression = Command.newExpression(lhs, op, rhs);
    	}
    	
    	exitRule(NonTerminal.EXPRESSION1);
    	
    	return expression;
    }
    
    // expression0 := expression1 [ op0 expression1 ] .
    public ast.Expression expression0()
    {
    	ast.Expression expression;
    	enterRule(NonTerminal.EXPRESSION0);
    	expression = expression1();
    	if (have(NonTerminal.OP0)){
    		ast.Expression lhs = expression;
    		Token op = op0();
    		ast.Expression rhs = expression1();
    		expression = Command.newExpression(lhs, op, rhs);
    	}   	
    	exitRule(NonTerminal.EXPRESSION0);
    	
    	return expression;
    }
    
    // expression-list := [ expression0 { "," expression0 } ] .
    public ast.ExpressionList expression_list()
    {
    	ast.ExpressionList exprList = new ast.ExpressionList(currentToken.lineNumber(), currentToken.charPosition());
    	
    	enterRule(NonTerminal.EXPRESSION_LIST);
    	
    	if (have(NonTerminal.EXPRESSION0)){
    		ast.Expression expr0 = expression0();
    		exprList.add(expr0);
    		while (accept(Token.Kind.COMMA)){
    			expr0 = expression0();
    			exprList.add(expr0);
    		}
    	}
    	
    	exitRule(NonTerminal.EXPRESSION_LIST);
    	
    	return exprList;
    }
    
    // parameter := IDENTIFIER ":" type .
    public Symbol parameter()
    {
    	enterRule(NonTerminal.PARAMETER);
    	
    	Symbol idSymbol = tryDeclareSymbol(expectRetrieve(Token.Kind.IDENTIFIER));
    	expect(Token.Kind.COLON);
    	type();
    	
    	exitRule(NonTerminal.PARAMETER);
    	
    	return idSymbol;
    }
    
    // parameter-list := [ parameter { "," parameter } ] .
    public List<Symbol> parameter_list()
    {
    	List<Symbol> paramList = new ArrayList<Symbol>();
    	enterRule(NonTerminal.PARAMETER_LIST);
    	
    	if (have(NonTerminal.PARAMETER)){
    		paramList.add(parameter());
    	
    		while (accept(Token.Kind.COMMA)){
    			paramList.add(parameter());
    		}
    	}
    	
    	exitRule(NonTerminal.PARAMETER_LIST);
    	
    	return paramList;
    }
    
    // variable-declaration := "var" IDENTIFIER ":" type ";" .
    public ast.VariableDeclaration variable_declaration()
    {
    	ast.VariableDeclaration varDeclaration;
    	
    	enterRule(NonTerminal.VARIABLE_DECLARATION);
    	
    	Token varTok = expectRetrieve(Token.Kind.VAR);
    	Token idTok = expectRetrieve(Token.Kind.IDENTIFIER);
    	Symbol idSymbol = tryDeclareSymbol(idTok);
    	
    	varDeclaration = new ast.VariableDeclaration(varTok.lineNumber(), varTok.charPosition(), idSymbol);
    	
    	expect(Token.Kind.COLON);
    	type();
    	expect(Token.Kind.SEMICOLON);
    	
    	exitRule(NonTerminal.VARIABLE_DECLARATION);
    	
    	return varDeclaration;
    }
    
    // array-declaration := "array" IDENTIFIER ":" type "[" INTEGER "]" { "[" INTEGER "]" } ";" .
    public ast.ArrayDeclaration array_declaration()
    {
    	ast.ArrayDeclaration arrayDeclaration;
    	
    	enterRule(NonTerminal.ARRAY_DECLARATION);
    	
    	Token arrTok = expectRetrieve(Token.Kind.ARRAY);
    	Symbol idSymbol = tryDeclareSymbol(expectRetrieve(Token.Kind.IDENTIFIER));
    	arrayDeclaration = new ast.ArrayDeclaration(arrTok.lineNumber(), arrTok.charPosition(), idSymbol);
    	expect(Token.Kind.COLON);
    	type();
    	expect(Token.Kind.OPEN_BRACKET);
    	expect(Token.Kind.INTEGER);
    	expect(Token.Kind.CLOSE_BRACKET);
    	while (accept(Token.Kind.OPEN_BRACKET)){
    		expect(Token.Kind.INTEGER);
    		expect(Token.Kind.CLOSE_BRACKET);
    	}
    	expect(Token.Kind.SEMICOLON);
    	
    	exitRule(NonTerminal.ARRAY_DECLARATION);
    	
    	return arrayDeclaration;
    }
    
    // function-definition := "func" IDENTIFIER "(" parameter-list ")" ":" type statement-block .
    public ast.FunctionDefinition function_definition()
    {
    	ast.FunctionDefinition funcDefinition;
    	
    	enterRule(NonTerminal.FUNCTION_DEFINITION);
    	
    	Token funcTok = expectRetrieve(Token.Kind.FUNC);
    	Symbol idSymbol = tryDeclareSymbol(expectRetrieve(Token.Kind.IDENTIFIER));
    	//funcDefinition = new ast.FunctionDefinition(funcTok.lineNumber(), funcTok.charPosition(), idSymbol);
    	
    	expect(Token.Kind.OPEN_PAREN);
    	enterScope();
    	
    	List<Symbol> paramList = parameter_list();
    	expect(Token.Kind.CLOSE_PAREN);
    	expect(Token.Kind.COLON);
    	type();
    	ast.StatementList stmtList = statement_block();
    	funcDefinition = new ast.FunctionDefinition(funcTok.lineNumber(), funcTok.charPosition(), idSymbol, paramList, stmtList);
    	
    	exitScope();
    	
    	exitRule(NonTerminal.FUNCTION_DEFINITION);
    	
    	return funcDefinition;
    }
    
    // declaration := variable-declaration | array-declaration | function-definition .
    public ast.Declaration declaration()
    {
    	ast.Declaration declare;
    	enterRule(NonTerminal.DECLARATION);
    	
    	if (have(NonTerminal.VARIABLE_DECLARATION)){
    		declare = variable_declaration();
    	}
    	else if (have(NonTerminal.ARRAY_DECLARATION)){
    		declare = array_declaration();
    	}
    	else if (have(NonTerminal.FUNCTION_DEFINITION)){
    		declare = function_definition();
    	}
    	else {
    		String errormessage = reportSyntaxError(NonTerminal.DECLARATION);
    		declare = new ast.Error(currentToken.lineNumber(), currentToken.charPosition(), errormessage);
    		throw new QuitParseException(errormessage);
    	}
    	
    	exitRule(NonTerminal.DECLARATION);
    	
    	return declare;
    }
    
    // declaration-list := { declaration } .
    public ast.DeclarationList declaration_list()
    {
    	ast.DeclarationList dList = new ast.DeclarationList(currentToken.lineNumber(), currentToken.charPosition());
    	
    	enterRule(NonTerminal.DECLARATION_LIST);
    	
    	while (have(NonTerminal.DECLARATION)){
    		ast.Declaration declare = declaration();
    		dList.add(declare);
    	}
    	
    	exitRule(NonTerminal.DECLARATION_LIST);
    	
    	return dList;
    }
    
    // assignment-statement := "let" designator "=" expression0 ";" .
    public ast.Assignment assignment_statement()
    {
    	ast.Assignment assignStmt;
    	enterRule(NonTerminal.ASSIGNMENT_STATEMENT);
    	
    	Token letTok = expectRetrieve(Token.Kind.LET);
    	ast.Expression destination = designator();
    	expect(Token.Kind.ASSIGN);
    	ast.Expression source = expression0();
    	expect(Token.Kind.SEMICOLON);
    	
    	assignStmt = new ast.Assignment(letTok.lineNumber(), letTok.charPosition(), destination, source);
    	exitRule(NonTerminal.ASSIGNMENT_STATEMENT);
    	
    	return assignStmt;
    }
    
    // call-statement := call-expression ";" .
    public ast.Call call_statement()
    {
    	ast.Call callStmt;
    	
    	enterRule(NonTerminal.CALL_STATEMENT);
    	
    	callStmt = call_expression();
    	expect(Token.Kind.SEMICOLON);
    	
    	exitRule(NonTerminal.CALL_STATEMENT);
    	
    	return callStmt;
    }
    
    // if-statement := "if" expression0 statement-block [ "else" statement-block ] .
    public ast.IfElseBranch if_statement()
    {
    	ast.IfElseBranch if_else;
    	enterRule(NonTerminal.IF_STATEMENT);
    	
    	Token ifTok = expectRetrieve(Token.Kind.IF);
    	ast.Expression cond = expression0();
    	enterScope();
    	ast.StatementList thenBlock = statement_block();
    	exitScope();
    	if (accept(Token.Kind.ELSE)){
    		enterScope();
    		ast.StatementList elseBlock = statement_block();
    		exitScope();
    		if_else = new ast.IfElseBranch(ifTok.lineNumber(), ifTok.charPosition(), cond, thenBlock, elseBlock);
    	}
    	else{
    		ast.StatementList elseBlock = new ast.StatementList(currentToken.lineNumber(), currentToken.charPosition());
    		if_else = new ast.IfElseBranch(ifTok.lineNumber(), ifTok.charPosition(), cond, thenBlock, elseBlock);
    	}
    	
    	exitRule(NonTerminal.IF_STATEMENT);
    	
    	return if_else;
    }
    
    // while-statement := "while" expression0 statement-block .
    public ast.WhileLoop while_statement()
    {
    	ast.WhileLoop while_loop;
    	enterRule(NonTerminal.WHILE_STATEMENT);

    	Token whileTok = expectRetrieve(Token.Kind.WHILE);
    	ast.Expression cond = expression0();
    	
    	enterScope();
    	ast.StatementList whileBlock = statement_block();
    	exitScope();
    	
    	exitRule(NonTerminal.WHILE_STATEMENT);
    	while_loop = new ast.WhileLoop(whileTok.lineNumber(), whileTok.charPosition(), cond, whileBlock);
    	
    	return while_loop;
    }
    
    // return-statement := "return" expression0 ";" .
    public ast.Return return_statement()
    {
    	ast.Return return_stmt;
    	enterRule(NonTerminal.RETURN_STATEMENT);
    	
    	Token returnTok = expectRetrieve(Token.Kind.RETURN);
    	ast.Expression to_return = expression0();
    	expect(Token.Kind.SEMICOLON);
    	
    	exitRule(NonTerminal.RETURN_STATEMENT);
    	return_stmt = new ast.Return(returnTok.lineNumber(), returnTok.charPosition(), to_return);
    	
    	return return_stmt;
    }
    
    // statement-block := "{" statement-list "}" .
    public ast.StatementList statement_block()
    {
    	ast.StatementList stmtList;
    	enterRule(NonTerminal.STATEMENT_BLOCK);
    	
    	
    	expect(Token.Kind.OPEN_BRACE);
    	
    	stmtList = statement_list();
    	expect(Token.Kind.CLOSE_BRACE);
    	
    	
    	exitRule(NonTerminal.STATEMENT_BLOCK);
    	
    	return stmtList;
    }
    
    // statement := variable-declaration | call-statement | assignment-statement | if-statement | while-statement | return-statement .
    public ast.Statement statement()
    {
    	ast.Statement stmt;
    	
    	enterRule(NonTerminal.STATEMENT);
    	if (have(NonTerminal.VARIABLE_DECLARATION)){
    		stmt = variable_declaration();
    	}
    	else if (have(NonTerminal.CALL_STATEMENT)){
    		stmt = call_statement();
    	}
    	else if (have(NonTerminal.ASSIGNMENT_STATEMENT)){
    		stmt = assignment_statement();
    	}
    	else if (have(NonTerminal.IF_STATEMENT)){
    		stmt = if_statement();
    	}
    	else if (have(NonTerminal.WHILE_STATEMENT)){
    		stmt = while_statement();
    	}
    	else if (have(NonTerminal.RETURN_STATEMENT)){
    		stmt = return_statement();
    	}
    	else {
    		String errormessage = reportSyntaxError(NonTerminal.STATEMENT);
    		stmt = new ast.Error(currentToken.lineNumber(), currentToken.charPosition(), errormessage);
//    		throw new QuitParseException(errormessage);
    	}
    	
    	exitRule(NonTerminal.STATEMENT);
    	return stmt;
    }
    
    // statement-list := { statement } .
    public ast.StatementList statement_list()
    {
    	ast.StatementList stmtList = new ast.StatementList(currentToken.lineNumber(), currentToken.charPosition());
    	enterRule(NonTerminal.STATEMENT_LIST);
    	
    	while (have(NonTerminal.STATEMENT)){
    		ast.Statement stmt = statement();
    		stmtList.add(stmt);
    	}
    	
    	exitRule(NonTerminal.STATEMENT_LIST);
    	
    	return stmtList;
    }
    
    // program := declaration-list EOF .
    public ast.DeclarationList program()
//    {
//        throw new RuntimeException("add code to each grammar rule, to build as ast.");
//    }
    {
    	ast.DeclarationList decList;
    	
        enterRule(NonTerminal.PROGRAM);
        
        decList = declaration_list();
        
        expect(Token.Kind.EOF);
        
        exitRule(NonTerminal.PROGRAM);
        
        return decList;
    }
    
}
