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
		ASC=10, DESC=11, DISTINCT=12, LEFTOUTERJOIN=13, QUERYOPERATOR=14, RIGHTPAREN=15, 
		LEFTPAREN=16, DOT=17, COMMA=18, QMARK=19, COLON=20, TEXT=21, COLONTEXT=22, 
		UPPERCASE=23, LOWERCASE=24, DIGIT=25, NUMBER=26, VALUE=27, WHITESPACE=28;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] ruleNames = {
		"SELECT", "WHERE", "NOT", "FROM", "JOIN", "AND", "OR", "ORDERBY", "ORDERING", 
		"ASC", "DESC", "DISTINCT", "LEFTOUTERJOIN", "QUERYOPERATOR", "RIGHTPAREN", 
		"LEFTPAREN", "DOT", "COMMA", "QMARK", "COLON", "TEXT", "COLONTEXT", "UPPERCASE", 
		"LOWERCASE", "DIGIT", "NUMBER", "VALUE", "WHITESPACE"
	};

	private static final String[] _LITERAL_NAMES = {
		null, null, null, null, null, null, null, null, "'ORDER BY'", null, "'ASC'", 
		"'DESC'", "'DISTINCT'", null, null, "')'", "'('", "'.'", "','", "'\"'", 
		"':'"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, "SELECT", "WHERE", "NOT", "FROM", "JOIN", "AND", "OR", "ORDERBY", 
		"ORDERING", "ASC", "DESC", "DISTINCT", "LEFTOUTERJOIN", "QUERYOPERATOR", 
		"RIGHTPAREN", "LEFTPAREN", "DOT", "COMMA", "QMARK", "COLON", "TEXT", "COLONTEXT", 
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
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2\36\u011e\b\1\4\2"+
		"\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4"+
		"\13\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22"+
		"\t\22\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31"+
		"\t\31\4\32\t\32\4\33\t\33\4\34\t\34\4\35\t\35\3\2\3\2\3\2\3\2\3\2\3\2"+
		"\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\5\2N\n\2\3\3\3\3\3\3"+
		"\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\5\3_\n\3\3\4\3\4\3\4"+
		"\3\4\3\4\3\4\3\4\3\4\3\4\5\4j\n\4\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5"+
		"\3\5\3\5\3\5\5\5x\n\5\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6"+
		"\5\6\u0086\n\6\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\5\7\u0091\n\7\3\b\3"+
		"\b\3\b\3\b\3\b\3\b\5\b\u0099\n\b\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3"+
		"\n\3\n\5\n\u00a6\n\n\3\13\3\13\3\13\3\13\3\f\3\f\3\f\3\f\3\f\3\r\3\r\3"+
		"\r\3\r\3\r\3\r\3\r\3\r\3\r\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16"+
		"\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16"+
		"\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16"+
		"\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16\5\16\u00e7\n\16\3\17\3\17\3\17"+
		"\3\17\3\17\3\17\3\17\3\17\3\17\3\17\5\17\u00f3\n\17\3\20\3\20\3\21\3\21"+
		"\3\22\3\22\3\23\3\23\3\24\3\24\3\25\3\25\3\26\3\26\3\26\6\26\u0104\n\26"+
		"\r\26\16\26\u0105\3\27\3\27\3\27\3\30\3\30\3\31\3\31\3\32\3\32\3\33\6"+
		"\33\u0112\n\33\r\33\16\33\u0113\3\34\3\34\3\35\6\35\u0119\n\35\r\35\16"+
		"\35\u011a\3\35\3\35\2\2\36\3\3\5\4\7\5\t\6\13\7\r\b\17\t\21\n\23\13\25"+
		"\f\27\r\31\16\33\17\35\20\37\21!\22#\23%\24\'\25)\26+\27-\30/\31\61\32"+
		"\63\33\65\34\67\359\36\3\2\3\4\2>>@@\2\u0137\2\3\3\2\2\2\2\5\3\2\2\2\2"+
		"\7\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r\3\2\2\2\2\17\3\2\2\2\2\21\3\2"+
		"\2\2\2\23\3\2\2\2\2\25\3\2\2\2\2\27\3\2\2\2\2\31\3\2\2\2\2\33\3\2\2\2"+
		"\2\35\3\2\2\2\2\37\3\2\2\2\2!\3\2\2\2\2#\3\2\2\2\2%\3\2\2\2\2\'\3\2\2"+
		"\2\2)\3\2\2\2\2+\3\2\2\2\2-\3\2\2\2\2/\3\2\2\2\2\61\3\2\2\2\2\63\3\2\2"+
		"\2\2\65\3\2\2\2\2\67\3\2\2\2\29\3\2\2\2\3M\3\2\2\2\5^\3\2\2\2\7i\3\2\2"+
		"\2\tw\3\2\2\2\13\u0085\3\2\2\2\r\u0090\3\2\2\2\17\u0098\3\2\2\2\21\u009a"+
		"\3\2\2\2\23\u00a5\3\2\2\2\25\u00a7\3\2\2\2\27\u00ab\3\2\2\2\31\u00b0\3"+
		"\2\2\2\33\u00e6\3\2\2\2\35\u00f2\3\2\2\2\37\u00f4\3\2\2\2!\u00f6\3\2\2"+
		"\2#\u00f8\3\2\2\2%\u00fa\3\2\2\2\'\u00fc\3\2\2\2)\u00fe\3\2\2\2+\u0103"+
		"\3\2\2\2-\u0107\3\2\2\2/\u010a\3\2\2\2\61\u010c\3\2\2\2\63\u010e\3\2\2"+
		"\2\65\u0111\3\2\2\2\67\u0115\3\2\2\29\u0118\3\2\2\2;<\7U\2\2<=\7G\2\2"+
		"=>\7N\2\2>?\7G\2\2?@\7E\2\2@N\7V\2\2AB\7u\2\2BC\7g\2\2CD\7n\2\2DE\7g\2"+
		"\2EF\7e\2\2FN\7v\2\2GH\7U\2\2HI\7g\2\2IJ\7n\2\2JK\7g\2\2KL\7e\2\2LN\7"+
		"v\2\2M;\3\2\2\2MA\3\2\2\2MG\3\2\2\2N\4\3\2\2\2OP\7Y\2\2PQ\7J\2\2QR\7G"+
		"\2\2RS\7T\2\2S_\7G\2\2TU\7y\2\2UV\7j\2\2VW\7g\2\2WX\7t\2\2X_\7g\2\2YZ"+
		"\7Y\2\2Z[\7j\2\2[\\\7g\2\2\\]\7t\2\2]_\7g\2\2^O\3\2\2\2^T\3\2\2\2^Y\3"+
		"\2\2\2_\6\3\2\2\2`a\7P\2\2ab\7Q\2\2bj\7V\2\2cd\7p\2\2de\7q\2\2ej\7v\2"+
		"\2fg\7P\2\2gh\7q\2\2hj\7v\2\2i`\3\2\2\2ic\3\2\2\2if\3\2\2\2j\b\3\2\2\2"+
		"kl\7H\2\2lm\7T\2\2mn\7Q\2\2nx\7O\2\2op\7h\2\2pq\7t\2\2qr\7q\2\2rx\7o\2"+
		"\2st\7H\2\2tu\7t\2\2uv\7q\2\2vx\7o\2\2wk\3\2\2\2wo\3\2\2\2ws\3\2\2\2x"+
		"\n\3\2\2\2yz\7L\2\2z{\7Q\2\2{|\7K\2\2|\u0086\7P\2\2}~\7l\2\2~\177\7q\2"+
		"\2\177\u0080\7k\2\2\u0080\u0086\7p\2\2\u0081\u0082\7L\2\2\u0082\u0083"+
		"\7q\2\2\u0083\u0084\7k\2\2\u0084\u0086\7p\2\2\u0085y\3\2\2\2\u0085}\3"+
		"\2\2\2\u0085\u0081\3\2\2\2\u0086\f\3\2\2\2\u0087\u0088\7C\2\2\u0088\u0089"+
		"\7P\2\2\u0089\u0091\7F\2\2\u008a\u008b\7c\2\2\u008b\u008c\7p\2\2\u008c"+
		"\u0091\7f\2\2\u008d\u008e\7C\2\2\u008e\u008f\7p\2\2\u008f\u0091\7f\2\2"+
		"\u0090\u0087\3\2\2\2\u0090\u008a\3\2\2\2\u0090\u008d\3\2\2\2\u0091\16"+
		"\3\2\2\2\u0092\u0093\7Q\2\2\u0093\u0099\7T\2\2\u0094\u0095\7q\2\2\u0095"+
		"\u0099\7t\2\2\u0096\u0097\7Q\2\2\u0097\u0099\7t\2\2\u0098\u0092\3\2\2"+
		"\2\u0098\u0094\3\2\2\2\u0098\u0096\3\2\2\2\u0099\20\3\2\2\2\u009a\u009b"+
		"\7Q\2\2\u009b\u009c\7T\2\2\u009c\u009d\7F\2\2\u009d\u009e\7G\2\2\u009e"+
		"\u009f\7T\2\2\u009f\u00a0\7\"\2\2\u00a0\u00a1\7D\2\2\u00a1\u00a2\7[\2"+
		"\2\u00a2\22\3\2\2\2\u00a3\u00a6\5\25\13\2\u00a4\u00a6\5\27\f\2\u00a5\u00a3"+
		"\3\2\2\2\u00a5\u00a4\3\2\2\2\u00a6\24\3\2\2\2\u00a7\u00a8\7C\2\2\u00a8"+
		"\u00a9\7U\2\2\u00a9\u00aa\7E\2\2\u00aa\26\3\2\2\2\u00ab\u00ac\7F\2\2\u00ac"+
		"\u00ad\7G\2\2\u00ad\u00ae\7U\2\2\u00ae\u00af\7E\2\2\u00af\30\3\2\2\2\u00b0"+
		"\u00b1\7F\2\2\u00b1\u00b2\7K\2\2\u00b2\u00b3\7U\2\2\u00b3\u00b4\7V\2\2"+
		"\u00b4\u00b5\7K\2\2\u00b5\u00b6\7P\2\2\u00b6\u00b7\7E\2\2\u00b7\u00b8"+
		"\7V\2\2\u00b8\32\3\2\2\2\u00b9\u00ba\7N\2\2\u00ba\u00bb\7G\2\2\u00bb\u00bc"+
		"\7H\2\2\u00bc\u00bd\7V\2\2\u00bd\u00be\7\"\2\2\u00be\u00bf\7Q\2\2\u00bf"+
		"\u00c0\7W\2\2\u00c0\u00c1\7V\2\2\u00c1\u00c2\7G\2\2\u00c2\u00c3\7T\2\2"+
		"\u00c3\u00c4\7\"\2\2\u00c4\u00c5\7L\2\2\u00c5\u00c6\7Q\2\2\u00c6\u00c7"+
		"\7K\2\2\u00c7\u00e7\7P\2\2\u00c8\u00c9\7n\2\2\u00c9\u00ca\7g\2\2\u00ca"+
		"\u00cb\7h\2\2\u00cb\u00cc\7v\2\2\u00cc\u00cd\7\"\2\2\u00cd\u00ce\7q\2"+
		"\2\u00ce\u00cf\7w\2\2\u00cf\u00d0\7v\2\2\u00d0\u00d1\7g\2\2\u00d1\u00d2"+
		"\7t\2\2\u00d2\u00d3\7\"\2\2\u00d3\u00d4\7l\2\2\u00d4\u00d5\7q\2\2\u00d5"+
		"\u00d6\7k\2\2\u00d6\u00e7\7p\2\2\u00d7\u00d8\7N\2\2\u00d8\u00d9\7g\2\2"+
		"\u00d9\u00da\7h\2\2\u00da\u00db\7v\2\2\u00db\u00dc\7\"\2\2\u00dc\u00dd"+
		"\7Q\2\2\u00dd\u00de\7w\2\2\u00de\u00df\7v\2\2\u00df\u00e0\7g\2\2\u00e0"+
		"\u00e1\7t\2\2\u00e1\u00e2\7\"\2\2\u00e2\u00e3\7L\2\2\u00e3\u00e4\7q\2"+
		"\2\u00e4\u00e5\7k\2\2\u00e5\u00e7\7p\2\2\u00e6\u00b9\3\2\2\2\u00e6\u00c8"+
		"\3\2\2\2\u00e6\u00d7\3\2\2\2\u00e7\34\3\2\2\2\u00e8\u00f3\t\2\2\2\u00e9"+
		"\u00ea\7@\2\2\u00ea\u00f3\7?\2\2\u00eb\u00ec\7>\2\2\u00ec\u00f3\7?\2\2"+
		"\u00ed\u00f3\7?\2\2\u00ee\u00ef\7N\2\2\u00ef\u00f0\7K\2\2\u00f0\u00f1"+
		"\7M\2\2\u00f1\u00f3\7G\2\2\u00f2\u00e8\3\2\2\2\u00f2\u00e9\3\2\2\2\u00f2"+
		"\u00eb\3\2\2\2\u00f2\u00ed\3\2\2\2\u00f2\u00ee\3\2\2\2\u00f3\36\3\2\2"+
		"\2\u00f4\u00f5\7+\2\2\u00f5 \3\2\2\2\u00f6\u00f7\7*\2\2\u00f7\"\3\2\2"+
		"\2\u00f8\u00f9\7\60\2\2\u00f9$\3\2\2\2\u00fa\u00fb\7.\2\2\u00fb&\3\2\2"+
		"\2\u00fc\u00fd\7$\2\2\u00fd(\3\2\2\2\u00fe\u00ff\7<\2\2\u00ff*\3\2\2\2"+
		"\u0100\u0104\5\61\31\2\u0101\u0104\5/\30\2\u0102\u0104\5\63\32\2\u0103"+
		"\u0100\3\2\2\2\u0103\u0101\3\2\2\2\u0103\u0102\3\2\2\2\u0104\u0105\3\2"+
		"\2\2\u0105\u0103\3\2\2\2\u0105\u0106\3\2\2\2\u0106,\3\2\2\2\u0107\u0108"+
		"\5)\25\2\u0108\u0109\5+\26\2\u0109.\3\2\2\2\u010a\u010b\4C\\\2\u010b\60"+
		"\3\2\2\2\u010c\u010d\4c|\2\u010d\62\3\2\2\2\u010e\u010f\4\62;\2\u010f"+
		"\64\3\2\2\2\u0110\u0112\5\63\32\2\u0111\u0110\3\2\2\2\u0112\u0113\3\2"+
		"\2\2\u0113\u0111\3\2\2\2\u0113\u0114\3\2\2\2\u0114\66\3\2\2\2\u0115\u0116"+
		"\5\65\33\2\u01168\3\2\2\2\u0117\u0119\7\"\2\2\u0118\u0117\3\2\2\2\u0119"+
		"\u011a\3\2\2\2\u011a\u0118\3\2\2\2\u011a\u011b\3\2\2\2\u011b\u011c\3\2"+
		"\2\2\u011c\u011d\b\35\2\2\u011d:\3\2\2\2\21\2M^iw\u0085\u0090\u0098\u00a5"+
		"\u00e6\u00f2\u0103\u0105\u0113\u011a\3\b\2\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}