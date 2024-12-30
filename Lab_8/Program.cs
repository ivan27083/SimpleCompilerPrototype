using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Reflection.Emit;
using System.Text.RegularExpressions;

public class HashTable<TKey, TValue>
{
    private List<List<KeyValuePair<TKey, TValue>>> hashTable;
    private int size;

    public HashTable(int size)
    {
        this.size = size;
        hashTable = new List<List<KeyValuePair<TKey, TValue>>>(size);
        for (int i = 0; i < size; i++)
        {
            hashTable.Add(new List<KeyValuePair<TKey, TValue>>());
        }
    }

    private int HashFunc(TKey key)
    {
        return Math.Abs(key.GetHashCode()) % size;
    }
    public void Add(TKey key, TValue value)
    {
        int index = HashFunc(key);
        var bucket = hashTable[index];
        for (int i = 0; i < bucket.Count; i++)
        {
            if (bucket[i].Key.Equals(key))
            {
                bucket[i] = new KeyValuePair<TKey, TValue>(key, value);
                return;
            }
        }
        bucket.Add(new KeyValuePair<TKey, TValue>(key, value));
    }

    public TValue Find(TKey key)
    {
        int index = HashFunc(key);
        var bucket = hashTable[index];

        foreach (var pair in bucket)
        {
            if (pair.Key.Equals(key))
            {
                return pair.Value;
            }
        }
        return default(TValue);
    }

    public bool Remove(TKey key)
    {
        int index = HashFunc(key);
        var bucket = hashTable[index];

        for (int i = 0; i < bucket.Count; i++)
        {
            if (bucket[i].Key.Equals(key))
            {
                bucket.RemoveAt(i);
                return true;
            }
        }
        return false;
    }
    public IEnumerable<KeyValuePair<TKey, TValue>> GetAllEntries()
    {
        foreach (var bucket in hashTable)
        {
            foreach (var pair in bucket)
            {
                yield return pair;
            }
        }
    }
}

public class SwitchCaseNode
{
    public ExpressionNode Expression { get; set; }
    public List<CaseNode> Cases { get; set; }

    public SwitchCaseNode(ExpressionNode expression, List<CaseNode> cases)
    {
        Expression = expression;
        Cases = cases;
    }
    public void GenerateAssembly(List<string> assemblyCode, string endLabel)
    {
        Expression.GenerateAssembly(assemblyCode);
        assemblyCode.Add("MOV EAX, [ESP]");
        assemblyCode.Add("ADD ESP, 4");

        foreach (var caseNode in Cases)
        {
            caseNode.GenerateCondition(assemblyCode, endLabel);
        }

        var defaultCase = Cases.FirstOrDefault(c => c.Value == null);
        if (defaultCase != null)
        {
            assemblyCode.Add($"JMP {defaultCase.Label}");
        }

        foreach (var caseNode in Cases)
        {
            caseNode.GenerateAssembly(assemblyCode, endLabel);
        }

        assemblyCode.Add($"{endLabel}");
    }
    public void Print(string indent = "", bool isLast = true)
    {
        Console.WriteLine($"{indent}{(isLast ? "└──" : "├──")} switch");
        indent += isLast ? "    " : "│   ";

        Console.WriteLine($"{indent}├── Expression:");
        Expression.Print(indent + "│   ", true);

        Console.WriteLine($"{indent}└── Cases:");
        for (int i = 0; i < Cases.Count; i++)
        {
            Cases[i].Print(indent + "    ", i == Cases.Count - 1);
        }
    }
}

public class CaseNode
{
    public ExpressionNode Value { get; set; }
    public List<StatementNode> Statements { get; set; }
    public string Label { get; } = $"case_{Guid.NewGuid().ToString("N")}";
    public CaseNode(ExpressionNode value, List<StatementNode> statements)
    {
        Value = value;
        Statements = statements;
    }
    public void GenerateCondition(List<string> assemblyCode, string endLabel)
    {
        if (Value != null)
        {
            Value.GenerateAssembly(assemblyCode);
            assemblyCode.Add("MOV EBX, [ESP]");
            assemblyCode.Add("ADD ESP, 4");
            assemblyCode.Add("CMP EAX, EBX");
            assemblyCode.Add($"JE {Label}");
        }
    }
    public void GenerateAssembly(List<string> assemblyCode, string endLabel)
    {
        foreach (var statement in Statements)
        {
            statement.GenerateAssembly(assemblyCode, endLabel);
        }
        
    }
    public void Print(string indent = "", bool isLast = true)
    {
        if (Value != null)
        {
            Console.WriteLine($"{indent}{(isLast ? "└──" : "├──")} case");
            indent += isLast ? "    " : "│   ";
            Console.WriteLine($"{indent}├── Value:");
            Value.Print(indent + "│   ", true);
        }
        else
        {
            Console.WriteLine($"{indent}{(isLast ? "└──" : "├──")} default");
            indent += isLast ? "    " : "│   ";
        }

        Console.WriteLine($"{indent}└── Statements:");
        for (int i = 0; i < Statements.Count; i++)
        {
            Statements[i].Print(indent + "    " , i == Statements.Count - 1);
        }
    }
}
public abstract class ExpressionNode
{
    public abstract void Print(string indent = "", bool isLast = true);
    public abstract void GenerateAssembly(List<string> assemblyCode);
}

public class NumberExpressionNode : ExpressionNode
{
    public int Value { get; }

    public NumberExpressionNode(int value)
    {
        Value = value;
    }
    public override void GenerateAssembly(List<string> assemblyCode)
    {
        assemblyCode.Add($"PUSH {Value}");
    }
    public override void Print(string indent = "", bool isLast = true)
    {
        Console.WriteLine($"{indent}{(isLast ? "└──" : "├──")} {Value}");
    }
}

public class IdentifierExpressionNode : ExpressionNode
{
    public string Name { get; }

    public IdentifierExpressionNode(string name)
    {
        Name = name;
    }
    public override void GenerateAssembly(List<string> assemblyCode)
    {
        assemblyCode.Add($"PUSH [{Name}]");
    }
    public override void Print(string indent = "", bool isLast = true)
    {
        Console.WriteLine($"{indent}{(isLast ? "└──" : "├──")} {Name}");
    }
}

public class BinaryExpressionNode : ExpressionNode
{
    public ExpressionNode Left { get; }
    public string Operator { get; }
    public ExpressionNode Right { get; }

    public BinaryExpressionNode(ExpressionNode left, string operatorToken, ExpressionNode right)
    {
        Left = left;
        Operator = operatorToken;
        Right = right;
    }
    public override void GenerateAssembly(List<string> assemblyCode)
    {
        Left.GenerateAssembly(assemblyCode);
        Right.GenerateAssembly(assemblyCode);
        assemblyCode.Add("POP EBX");
        assemblyCode.Add("POP EAX");
        switch (Operator)
        {
            case "+":
                assemblyCode.Add("ADD EAX, EBX");
                break;
            case "-":
                assemblyCode.Add("SUB EAX, EBX");
                break;
            case "*":
                assemblyCode.Add("IMUL EAX, EBX");
                break;
            case "/":
                assemblyCode.Add("XOR EDX, EDX");
                assemblyCode.Add("IDIV EBX");
                break;
        }
        assemblyCode.Add("PUSH EAX");
    }
    public override void Print(string indent = "", bool isLast = true)
    {
        Console.WriteLine($"{indent}{(isLast ? "└──" : "├──")} {Operator}");
        indent += isLast ? "    " : "│   ";
        Left.Print(indent, false);
        Right.Print(indent, true);
    }
}
public class AssignmentExpressionNode : ExpressionNode
{
    public string Variable { get; }
    public ExpressionNode Expression { get; }

    public AssignmentExpressionNode(string variable, ExpressionNode expression)
    {
        Variable = variable;
        Expression = expression;
    }
    public override void GenerateAssembly(List<string> assemblyCode)
    {
        Expression.GenerateAssembly(assemblyCode);
        assemblyCode.Add("POP EAX");
        assemblyCode.Add($"MOV [{Variable}], EAX ");
    }
    public override void Print(string indent = "", bool isLast = true)
    {
        Console.WriteLine($"{indent}{(isLast ? "└──" : "├──")} {Variable} := ");
        Expression.Print(indent + "    ", true);
    }
}
public abstract class StatementNode
{
    public abstract void Print(string indent = "", bool isLast = true);
    public abstract void GenerateAssembly(List<string> assemblyCode, string endLabel = null);
}

public class ExpressionStatementNode : StatementNode
{
    public ExpressionNode Expression { get; }

    public ExpressionStatementNode(ExpressionNode expression)
    {
        Expression = expression;
    }
    public override void GenerateAssembly(List<string> assemblyCode, string endLabel = null)
    {
        Expression.GenerateAssembly(assemblyCode);
    }
    public override void Print(string indent = "", bool isLast = true)
    {
        Console.WriteLine($"{indent}{(isLast ? "└──" : "├──")} Expression:");
        Expression.Print(indent + (isLast ? "    " : "│   "), true);
    }
}

public class BreakStatementNode : StatementNode
{
    public override void GenerateAssembly(List<string> assemblyCode, string endLabel)
    {
        assemblyCode.Add($"JMP {endLabel}");
    }
    public override void Print(string indent = "", bool isLast = true)
    {
        Console.WriteLine($"{indent}{(isLast ? "└──" : "├──")} break");
    }
}
public class SyntaxAnalyzer
{
    private HashTable<int, Tuple<string, string>> tokens;
    private IEnumerator<KeyValuePair<int, Tuple<string, string>>> enumerator;
    private List<string> errors = new List<string>();

    public SyntaxAnalyzer(HashTable<int, Tuple<string, string>> tokens)
    {
        this.tokens = tokens;
        this.enumerator = tokens.GetAllEntries().GetEnumerator();
        MoveNext();
    }

    private bool MoveNext()
    {
        return enumerator.MoveNext();
    }

    private Tuple<string, string> CurrentToken => enumerator.Current.Value;

    public SwitchCaseNode Parse()
    {
        return ParseSwitch();
    }

    private SwitchCaseNode ParseSwitch()
    {
        Match("switch", "keyword");
        Match("(", "delimeter");
        var expression = ParseTerm();
        Match(")", "delimeter");
        Match("{", "delimeter");
        var cases = ParseCases();
        Match("}", "delimeter");
        return new SwitchCaseNode(expression, cases);
    }

    private List<CaseNode> ParseCases()
    {
        var cases = new List<CaseNode>();
        while (CurrentToken.Item1 == "case")
        {
            Match("case", "keyword");
            var value = ParseTerm();
            Match(":", "operator");
            var statements = ParseStatements();
            cases.Add(new CaseNode(value, statements));
        }
        if (CurrentToken.Item1 == "default")
        {
            Match("default", "keyword");
            Match(":", "operator");
            var statements = ParseStatements();
            cases.Add(new CaseNode(null, statements));
        }
        return cases;
    }

    private List<StatementNode> ParseStatements()
    {
        var statements = new List<StatementNode>();
        while (CurrentToken.Item1 != "case" && CurrentToken.Item1 != "default" && CurrentToken.Item1 != "}")
        {
            statements.Add(ParseStatement());
        }
        return statements;
    }

    private StatementNode ParseStatement()
    {
        if (CurrentToken.Item1 == "break")
        {
            MoveNext();
            Match(";", "delimeter");
            return new BreakStatementNode();
        }
        else
        {
            var expression = ParseExpression();
            Match(";", "delimeter");
            return new ExpressionStatementNode(expression);
        }
    }

    private ExpressionNode ParseExpression()
    {
        var token = CurrentToken.Item1;
        if (CurrentToken.Item2 == "identifier")
        {
            MoveNext();
            if (CurrentToken.Item1 == ":=")
            {
                MoveNext();
                var value = ParseTerm();
                return new AssignmentExpressionNode(token, value);
            }

            throw new InvalidOperationException("Ожидался оператор '='");
        }
        return ParseTerm();
    }

    private ExpressionNode ParseTerm()
    {
        var left = ParseFactor();

        while (CurrentToken.Item1 == "+" || CurrentToken.Item1 == "-")
        {
            var operatorToken = CurrentToken.Item1;
            MoveNext();
            var right = ParseFactor();
            left = new BinaryExpressionNode(left, operatorToken, right);
        }

        return left;
    }

    private ExpressionNode ParseFactor()
    {
        var left = ParsePrimary();

        while (CurrentToken.Item1 == "*" || CurrentToken.Item1 == "/")
        {
            var operatorToken = CurrentToken.Item1;
            MoveNext();
            var right = ParsePrimary();
            left = new BinaryExpressionNode(left, operatorToken, right);
        }

        return left;
    }

    private ExpressionNode ParsePrimary()
    {
        if (int.TryParse(CurrentToken.Item1, out int number))
        {
            var node = new NumberExpressionNode(number);
            MoveNext();
            return node;
        }

        if (CurrentToken.Item2 == "identifier")
        {
            var node = new IdentifierExpressionNode(CurrentToken.Item1);
            MoveNext();
            return node;
        }

        if (CurrentToken.Item1 == "(")
        {
            MoveNext();
            var expression = ParseTerm();
            Match(")", "delimeter");
            return expression;
        }

        throw new Exception($"Ошибка синтаксиса: Ожидался идентификатор, число или выражение в скобках, найдено '{CurrentToken.Item1}'");
    }

    private void Match(string expectedValue, string expectedType)
    {
        if (CurrentToken.Item1 == expectedValue && CurrentToken.Item2 == expectedType)
        {
            MoveNext();
        }
        else
        {
            errors.Add($"Ошибка синтаксиса на позиции {enumerator.Current.Key}: ожидалось '{expectedValue}' типа '{expectedType}', получено '{CurrentToken.Item1}' типа '{CurrentToken.Item2}'");
        }
    }
    public void PrintReport()
    {
        if (errors.Count > 0)
        {
            Console.WriteLine("Найдены ошибки синтаксического анализа:");
            foreach (var error in errors)
            {
                Console.WriteLine(error);
            }
        }
        else
        {
            Console.WriteLine("Синтаксический анализ завершен успешно.");
        }
    }
    public static List<string> OptimizeAssemblyCode(List<string> assemblyCode)
    {
        var optimizedCode = new List<string>();

        for (int i = 0; i < assemblyCode.Count; i++)
        {
            string line = assemblyCode[i];

            if (i > 0 &&
                line.StartsWith("ADD ESP,") &&
                assemblyCode[i - 1].StartsWith("ADD ESP,"))
            {
                int prevValue = int.Parse(Regex.Match(assemblyCode[i - 1], @"\d+").Value);
                int currValue = int.Parse(Regex.Match(line, @"\d+").Value);

                optimizedCode[optimizedCode.Count - 1] = $"ADD ESP, {prevValue + currValue}";
                continue;
            }

            if (i > 0 &&
                assemblyCode[i - 1].StartsWith("PUSH") &&
                line.StartsWith("POP"))
            {
                string pushedValue = Regex.Match(assemblyCode[i - 1], @"(?<=PUSH ).+").Value;
                string poppedReg = Regex.Match(line, @"(?<=POP ).+").Value;

                if (pushedValue == poppedReg)
                {
                    optimizedCode.RemoveAt(optimizedCode.Count - 1);
                    continue;
                }
            }

            if (line.EndsWith(":") && !assemblyCode.Any(code => code.Contains(line.TrimEnd(':'))))
            {
                continue;
            }

            optimizedCode.Add(line);
        }

        return optimizedCode;
    }
}
public class LexicalAnalyzer
{
    private static HashTable<string, Dictionary<string, string>> symbolTable = new HashTable<string, Dictionary<string, string>>(50);
    private static HashTable<int, Tuple<string, string>> tokens = new HashTable<int, Tuple<string, string>>(100);
    private static int tokenCounter = 0;

    private static Dictionary<string, string> patterns = new Dictionary<string, string>
    {
        { "keyword", @"(\b(switch|case|default|break)\b)" },
        { "operator", @"(\:=|[\+\-\*/:<>])|==" },
        { "delimeter", @"([(){};])" },
        { "identifier", @"([_@a-zA-Z][_a-zA-Z0-9]*)" },
        { "number", @"(\b\d+(\.\d+)?\b)" }
    };

    private static void AddSymbol(string lexeme, string lexType)
    {
        var existingEntry = symbolTable.Find(lexeme);
        if (existingEntry == null)
        {
            symbolTable.Add(lexeme, new Dictionary<string, string>
        {
            { "type", lexType },
            { "info", lexType == "identifier" ? "переменная" : "константа" }
        });
        }
    }

    private static void AddToken(string lexeme, string type)
    {
        tokens.Add(tokenCounter++, Tuple.Create(lexeme, type));
    }

    public static void LexicalAnalysis(string code)
    {
        string combinedPattern =
            $@"({patterns["keyword"]})|({patterns["operator"]})|({patterns["delimeter"]})|({patterns["identifier"]})|({patterns["number"]})";

        foreach (Match match in Regex.Matches(code, combinedPattern))
        {
            string lexeme = match.Value;

            if (Regex.IsMatch(lexeme, patterns["keyword"]))
            {
                AddToken(lexeme, "keyword");
            }
            else if (Regex.IsMatch(lexeme, patterns["operator"]))
            {
                AddToken(lexeme, "operator");
            }
            else if (Regex.IsMatch(lexeme, patterns["delimeter"]))
            {
                AddToken(lexeme, "delimeter");
            }
            else if (Regex.IsMatch(lexeme, patterns["identifier"]))
            {
                AddToken(lexeme, "identifier");
                AddSymbol(lexeme, "identifier");
            }
            else if (Regex.IsMatch(lexeme, patterns["number"]))
            {
                AddToken(lexeme, "number");
                AddSymbol(lexeme, "number");
            }
        }
    }
    
    public static void Main(string[] args)
    {
        string code = "switch (h){\n\tcase 1:\n\t\ta := 10 + 2;\n\t\tb := a + 7;\n\t\tbreak;\n\tcase 2:\n\t\tc := a + b;\n\t\tbreak;\n\tdefault:\n\t\td := a + b * (c + 6);\n\t\tbreak;\n}";
        Console.WriteLine(code + '\n');

        LexicalAnalysis(code);

        Console.WriteLine("Последовательность лексем:");
        foreach (var token in tokens.GetAllEntries())
        {
            Console.WriteLine($"{token.Key}\t{token.Value.Item1}\t {token.Value.Item2}");
        }

        Console.WriteLine("\nТаблица имен:");
        foreach (var symbol in symbolTable.GetAllEntries())
        {
            Console.WriteLine($"{symbol.Key}:\t {symbol.Value["type"]} \t {symbol.Value["info"]}");
        }
        Console.WriteLine();

        SyntaxAnalyzer syntaxAnalyzer = new SyntaxAnalyzer(tokens);
        var syntaxTree = syntaxAnalyzer.Parse();
        Console.WriteLine("\nДерево разбора:");
        syntaxTree.Print();
        List<string> assemblyCode = new List<string>();
        syntaxTree.GenerateAssembly(assemblyCode, "switch_end");
        Console.WriteLine("Код ассемблера:");
        foreach (var line in assemblyCode)
        {
            Console.WriteLine(line);
        }
        List<string> optimizedAssemblyCode = SyntaxAnalyzer.OptimizeAssemblyCode(assemblyCode);

        Console.WriteLine("\nОптимизированный код ассемблера:");
        foreach (var line in optimizedAssemblyCode)
        {
            Console.WriteLine(line);
        }

        Console.ReadKey();
    }
}