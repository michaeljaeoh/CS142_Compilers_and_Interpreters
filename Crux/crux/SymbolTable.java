package crux;

import java.util.Map;
import java.util.LinkedHashMap;


public class SymbolTable {
    private Map<String, Symbol> table;
    private SymbolTable parent;
    private int depth;
    
    public SymbolTable()
    {
    	depth = 0;
        table = new LinkedHashMap();
        init_predef_func();
    }
    
    public SymbolTable(SymbolTable parent)
    {
    	depth = parent.depth + 1;
    	table = new LinkedHashMap();
    	this.parent = parent;
    }
    
    private void init_predef_func()
    {
    	Symbol symbol = new Symbol("readInt");
    	table.put("readInt", symbol);
    	symbol = new Symbol("readFloat");
    	table.put("readFloat", symbol);
    	symbol = new Symbol("printBool");
    	table.put("printBool", symbol);
    	symbol = new Symbol("printInt");
    	table.put("printInt", symbol);
    	symbol = new Symbol("printFloat");
    	table.put("printFloat", symbol);
    	symbol = new Symbol("println");
    	table.put("println", symbol);
    }
    
    public Symbol lookup(String name) throws SymbolNotFoundError
    {
        Symbol found = table.get(name);
        if (found == null && this.depth != 0){
        	found = parent.lookup(name);
        }
        else if (found == null && this.depth == 0){
        	throw new SymbolNotFoundError(name);
        }
        return found;
    }
    
    public Symbol insert(String name) throws RedeclarationError
    {
        Symbol symbol = table.get(name);
        if (symbol == null)
        {
        	symbol = new Symbol(name);
        	table.put(name, symbol);
        	return symbol;
        }
        else{
        	throw new RedeclarationError(symbol);
        }
    }
    
    public SymbolTable get_parent_table()
    {
    	return this.parent;
    }
    
    
    public String toString()
    {
        StringBuffer sb = new StringBuffer();
        if (depth > 0)
            sb.append(parent.toString());
        
        String indent = new String();
        for (int i = 0; i < depth; i++) {
            indent += "  ";
        }
        
        for (Symbol s : table.values())
        {
            sb.append(indent + s.toString() + "\n");
        }
        return sb.toString();
    }
}

class SymbolNotFoundError extends Error
{
    private static final long serialVersionUID = 1L;
    private String name;
    
    SymbolNotFoundError(String name)
    {
        this.name = name;
    }
    
    public String name()
    {
        return name;
    }
}

class RedeclarationError extends Error
{
    private static final long serialVersionUID = 1L;

    public RedeclarationError(Symbol sym)
    {
        super("Symbol " + sym + " being redeclared.");
    }
}
