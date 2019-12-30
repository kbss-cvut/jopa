package cz.cvut.kbss.jopa.query.soql;// Generated from soql.g4 by ANTLR 4.7.1
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class soqlLexer extends Lexer {
	static { RuntimeMetaData.checkVersion("4.7.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		SELECT=1, WHERE=2, NOT=3, FROM=4, JOIN=5, AND=6, OR=7, ORDERBY=8, ORDERING=9, 
		GROUPBY=10, ASC=11, DESC=12, DISTINCT=13, COUNT=14, QUERYOPERATOR=15, 
		DOT=16, COMMA=17, QMARK=18, COLON=19, RIGHTPAREN=20, LEFTPAREN=21, TEXT=22, 
		COLONTEXT=23, UPPERCASE=24, LOWERCASE=25, DIGIT=26, NUMBER=27, VALUE=28, 
		WHITESPACE=29;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] ruleNames = {
		"SELECT", "WHERE", "NOT", "FROM", "JOIN", "AND", "OR", "ORDERBY", "ORDERING", 
		"GROUPBY", "ASC", "DESC", "DISTINCT", "COUNT", "QUERYOPERATOR", "DOT", 
		"COMMA", "QMARK", "COLON", "RIGHTPAREN", "LEFTPAREN", "TEXT", "COLONTEXT", 
		"UPPERCASE", "LOWERCASE", "DIGIT", "NUMBER", "VALUE", "WHITESPACE"
	};

	private static final String[] _LITERAL_NAMES = {
		null, "'SELECT'", "'WHERE'", "'NOT'", "'FROM'", "'JOIN'", "'AND'", "'OR'", 
		"'ORDER BY'", null, "'GROUP BY'", "'ASC'", "'DESC'", "'DISTINCT'", "'COUNT'", 
		null, "'.'", "','", "'\"'", "':'", "')'", "'('"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, "SELECT", "WHERE", "NOT", "FROM", "JOIN", "AND", "OR", "ORDERBY", 
		"ORDERING", "GROUPBY", "ASC", "DESC", "DISTINCT", "COUNT", "QUERYOPERATOR", 
		"DOT", "COMMA", "QMARK", "COLON", "RIGHTPAREN", "LEFTPAREN", "TEXT", "COLONTEXT", 
		"UPPERCASE", "LOWERCASE", "DIGIT", "NUMBER", "VALUE", "WHITESPACE"
	};
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}


	public soqlLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "soql.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getChannelNames() { return channelNames; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2\37\u00c3\b\1\4\2"+
		"\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4"+
		"\13\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22"+
		"\t\22\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31"+
		"\t\31\4\32\t\32\4\33\t\33\4\34\t\34\4\35\t\35\4\36\t\36\3\2\3\2\3\2\3"+
		"\2\3\2\3\2\3\2\3\3\3\3\3\3\3\3\3\3\3\3\3\4\3\4\3\4\3\4\3\5\3\5\3\5\3\5"+
		"\3\5\3\6\3\6\3\6\3\6\3\6\3\7\3\7\3\7\3\7\3\b\3\b\3\b\3\t\3\t\3\t\3\t\3"+
		"\t\3\t\3\t\3\t\3\t\3\n\3\n\5\nk\n\n\3\13\3\13\3\13\3\13\3\13\3\13\3\13"+
		"\3\13\3\13\3\f\3\f\3\f\3\f\3\r\3\r\3\r\3\r\3\r\3\16\3\16\3\16\3\16\3\16"+
		"\3\16\3\16\3\16\3\16\3\17\3\17\3\17\3\17\3\17\3\17\3\20\3\20\3\20\3\20"+
		"\3\20\3\20\3\20\3\20\3\20\3\20\5\20\u0098\n\20\3\21\3\21\3\22\3\22\3\23"+
		"\3\23\3\24\3\24\3\25\3\25\3\26\3\26\3\27\3\27\3\27\6\27\u00a9\n\27\r\27"+
		"\16\27\u00aa\3\30\3\30\3\30\3\31\3\31\3\32\3\32\3\33\3\33\3\34\6\34\u00b7"+
		"\n\34\r\34\16\34\u00b8\3\35\3\35\3\36\6\36\u00be\n\36\r\36\16\36\u00bf"+
		"\3\36\3\36\2\2\37\3\3\5\4\7\5\t\6\13\7\r\b\17\t\21\n\23\13\25\f\27\r\31"+
		"\16\33\17\35\20\37\21!\22#\23%\24\'\25)\26+\27-\30/\31\61\32\63\33\65"+
		"\34\67\359\36;\37\3\2\3\4\2>>@@\2\u00cc\2\3\3\2\2\2\2\5\3\2\2\2\2\7\3"+
		"\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r\3\2\2\2\2\17\3\2\2\2\2\21\3\2\2\2"+
		"\2\23\3\2\2\2\2\25\3\2\2\2\2\27\3\2\2\2\2\31\3\2\2\2\2\33\3\2\2\2\2\35"+
		"\3\2\2\2\2\37\3\2\2\2\2!\3\2\2\2\2#\3\2\2\2\2%\3\2\2\2\2\'\3\2\2\2\2)"+
		"\3\2\2\2\2+\3\2\2\2\2-\3\2\2\2\2/\3\2\2\2\2\61\3\2\2\2\2\63\3\2\2\2\2"+
		"\65\3\2\2\2\2\67\3\2\2\2\29\3\2\2\2\2;\3\2\2\2\3=\3\2\2\2\5D\3\2\2\2\7"+
		"J\3\2\2\2\tN\3\2\2\2\13S\3\2\2\2\rX\3\2\2\2\17\\\3\2\2\2\21_\3\2\2\2\23"+
		"j\3\2\2\2\25l\3\2\2\2\27u\3\2\2\2\31y\3\2\2\2\33~\3\2\2\2\35\u0087\3\2"+
		"\2\2\37\u0097\3\2\2\2!\u0099\3\2\2\2#\u009b\3\2\2\2%\u009d\3\2\2\2\'\u009f"+
		"\3\2\2\2)\u00a1\3\2\2\2+\u00a3\3\2\2\2-\u00a8\3\2\2\2/\u00ac\3\2\2\2\61"+
		"\u00af\3\2\2\2\63\u00b1\3\2\2\2\65\u00b3\3\2\2\2\67\u00b6\3\2\2\29\u00ba"+
		"\3\2\2\2;\u00bd\3\2\2\2=>\7U\2\2>?\7G\2\2?@\7N\2\2@A\7G\2\2AB\7E\2\2B"+
		"C\7V\2\2C\4\3\2\2\2DE\7Y\2\2EF\7J\2\2FG\7G\2\2GH\7T\2\2HI\7G\2\2I\6\3"+
		"\2\2\2JK\7P\2\2KL\7Q\2\2LM\7V\2\2M\b\3\2\2\2NO\7H\2\2OP\7T\2\2PQ\7Q\2"+
		"\2QR\7O\2\2R\n\3\2\2\2ST\7L\2\2TU\7Q\2\2UV\7K\2\2VW\7P\2\2W\f\3\2\2\2"+
		"XY\7C\2\2YZ\7P\2\2Z[\7F\2\2[\16\3\2\2\2\\]\7Q\2\2]^\7T\2\2^\20\3\2\2\2"+
		"_`\7Q\2\2`a\7T\2\2ab\7F\2\2bc\7G\2\2cd\7T\2\2de\7\"\2\2ef\7D\2\2fg\7["+
		"\2\2g\22\3\2\2\2hk\5\27\f\2ik\5\31\r\2jh\3\2\2\2ji\3\2\2\2k\24\3\2\2\2"+
		"lm\7I\2\2mn\7T\2\2no\7Q\2\2op\7W\2\2pq\7R\2\2qr\7\"\2\2rs\7D\2\2st\7["+
		"\2\2t\26\3\2\2\2uv\7C\2\2vw\7U\2\2wx\7E\2\2x\30\3\2\2\2yz\7F\2\2z{\7G"+
		"\2\2{|\7U\2\2|}\7E\2\2}\32\3\2\2\2~\177\7F\2\2\177\u0080\7K\2\2\u0080"+
		"\u0081\7U\2\2\u0081\u0082\7V\2\2\u0082\u0083\7K\2\2\u0083\u0084\7P\2\2"+
		"\u0084\u0085\7E\2\2\u0085\u0086\7V\2\2\u0086\34\3\2\2\2\u0087\u0088\7"+
		"E\2\2\u0088\u0089\7Q\2\2\u0089\u008a\7W\2\2\u008a\u008b\7P\2\2\u008b\u008c"+
		"\7V\2\2\u008c\36\3\2\2\2\u008d\u0098\t\2\2\2\u008e\u008f\7@\2\2\u008f"+
		"\u0098\7?\2\2\u0090\u0091\7>\2\2\u0091\u0098\7?\2\2\u0092\u0098\7?\2\2"+
		"\u0093\u0094\7N\2\2\u0094\u0095\7K\2\2\u0095\u0096\7M\2\2\u0096\u0098"+
		"\7G\2\2\u0097\u008d\3\2\2\2\u0097\u008e\3\2\2\2\u0097\u0090\3\2\2\2\u0097"+
		"\u0092\3\2\2\2\u0097\u0093\3\2\2\2\u0098 \3\2\2\2\u0099\u009a\7\60\2\2"+
		"\u009a\"\3\2\2\2\u009b\u009c\7.\2\2\u009c$\3\2\2\2\u009d\u009e\7$\2\2"+
		"\u009e&\3\2\2\2\u009f\u00a0\7<\2\2\u00a0(\3\2\2\2\u00a1\u00a2\7+\2\2\u00a2"+
		"*\3\2\2\2\u00a3\u00a4\7*\2\2\u00a4,\3\2\2\2\u00a5\u00a9\5\63\32\2\u00a6"+
		"\u00a9\5\61\31\2\u00a7\u00a9\5\65\33\2\u00a8\u00a5\3\2\2\2\u00a8\u00a6"+
		"\3\2\2\2\u00a8\u00a7\3\2\2\2\u00a9\u00aa\3\2\2\2\u00aa\u00a8\3\2\2\2\u00aa"+
		"\u00ab\3\2\2\2\u00ab.\3\2\2\2\u00ac\u00ad\5\'\24\2\u00ad\u00ae\5-\27\2"+
		"\u00ae\60\3\2\2\2\u00af\u00b0\4C\\\2\u00b0\62\3\2\2\2\u00b1\u00b2\4c|"+
		"\2\u00b2\64\3\2\2\2\u00b3\u00b4\4\62;\2\u00b4\66\3\2\2\2\u00b5\u00b7\5"+
		"\65\33\2\u00b6\u00b5\3\2\2\2\u00b7\u00b8\3\2\2\2\u00b8\u00b6\3\2\2\2\u00b8"+
		"\u00b9\3\2\2\2\u00b98\3\2\2\2\u00ba\u00bb\5\67\34\2\u00bb:\3\2\2\2\u00bc"+
		"\u00be\7\"\2\2\u00bd\u00bc\3\2\2\2\u00be\u00bf\3\2\2\2\u00bf\u00bd\3\2"+
		"\2\2\u00bf\u00c0\3\2\2\2\u00c0\u00c1\3\2\2\2\u00c1\u00c2\b\36\2\2\u00c2"+
		"<\3\2\2\2\t\2j\u0097\u00a8\u00aa\u00b8\u00bf\3\b\2\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}