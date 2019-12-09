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
		ASC=10, DESC=11, LEFTOUTERJOIN=12, QUERYOPERATOR=13, RIGHTPAREN=14, LEFTPAREN=15, 
		DOT=16, COMMA=17, QMARK=18, COLON=19, TEXT=20, COLONTEXT=21, UPPERCASE=22, 
		LOWERCASE=23, DIGIT=24, NUMBER=25, VALUE=26, WHITESPACE=27;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] ruleNames = {
		"SELECT", "WHERE", "NOT", "FROM", "JOIN", "AND", "OR", "ORDERBY", "ORDERING", 
		"ASC", "DESC", "LEFTOUTERJOIN", "QUERYOPERATOR", "RIGHTPAREN", "LEFTPAREN", 
		"DOT", "COMMA", "QMARK", "COLON", "TEXT", "COLONTEXT", "UPPERCASE", "LOWERCASE", 
		"DIGIT", "NUMBER", "VALUE", "WHITESPACE"
	};

	private static final String[] _LITERAL_NAMES = {
		null, null, null, null, null, null, null, null, "'ORDER BY'", null, "'ASC'", 
		"'DESC'", null, null, "')'", "'('", "'.'", "','", "'\"'", "':'"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, "SELECT", "WHERE", "NOT", "FROM", "JOIN", "AND", "OR", "ORDERBY", 
		"ORDERING", "ASC", "DESC", "LEFTOUTERJOIN", "QUERYOPERATOR", "RIGHTPAREN", 
		"LEFTPAREN", "DOT", "COMMA", "QMARK", "COLON", "TEXT", "COLONTEXT", "UPPERCASE", 
		"LOWERCASE", "DIGIT", "NUMBER", "VALUE", "WHITESPACE"
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
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2\35\u0113\b\1\4\2"+
		"\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4"+
		"\13\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22"+
		"\t\22\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31"+
		"\t\31\4\32\t\32\4\33\t\33\4\34\t\34\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3"+
		"\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\5\2L\n\2\3\3\3\3\3\3\3\3\3\3\3"+
		"\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\5\3]\n\3\3\4\3\4\3\4\3\4\3\4\3"+
		"\4\3\4\3\4\3\4\5\4h\n\4\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3"+
		"\5\5\5v\n\5\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\5\6\u0084"+
		"\n\6\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\5\7\u008f\n\7\3\b\3\b\3\b\3\b"+
		"\3\b\3\b\5\b\u0097\n\b\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\n\3\n\5\n"+
		"\u00a4\n\n\3\13\3\13\3\13\3\13\3\f\3\f\3\f\3\f\3\f\3\r\3\r\3\r\3\r\3\r"+
		"\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3"+
		"\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r"+
		"\3\r\3\r\3\r\3\r\3\r\5\r\u00dc\n\r\3\16\3\16\3\16\3\16\3\16\3\16\3\16"+
		"\3\16\3\16\3\16\5\16\u00e8\n\16\3\17\3\17\3\20\3\20\3\21\3\21\3\22\3\22"+
		"\3\23\3\23\3\24\3\24\3\25\3\25\3\25\6\25\u00f9\n\25\r\25\16\25\u00fa\3"+
		"\26\3\26\3\26\3\27\3\27\3\30\3\30\3\31\3\31\3\32\6\32\u0107\n\32\r\32"+
		"\16\32\u0108\3\33\3\33\3\34\6\34\u010e\n\34\r\34\16\34\u010f\3\34\3\34"+
		"\2\2\35\3\3\5\4\7\5\t\6\13\7\r\b\17\t\21\n\23\13\25\f\27\r\31\16\33\17"+
		"\35\20\37\21!\22#\23%\24\'\25)\26+\27-\30/\31\61\32\63\33\65\34\67\35"+
		"\3\2\3\4\2>>@@\2\u012c\2\3\3\2\2\2\2\5\3\2\2\2\2\7\3\2\2\2\2\t\3\2\2\2"+
		"\2\13\3\2\2\2\2\r\3\2\2\2\2\17\3\2\2\2\2\21\3\2\2\2\2\23\3\2\2\2\2\25"+
		"\3\2\2\2\2\27\3\2\2\2\2\31\3\2\2\2\2\33\3\2\2\2\2\35\3\2\2\2\2\37\3\2"+
		"\2\2\2!\3\2\2\2\2#\3\2\2\2\2%\3\2\2\2\2\'\3\2\2\2\2)\3\2\2\2\2+\3\2\2"+
		"\2\2-\3\2\2\2\2/\3\2\2\2\2\61\3\2\2\2\2\63\3\2\2\2\2\65\3\2\2\2\2\67\3"+
		"\2\2\2\3K\3\2\2\2\5\\\3\2\2\2\7g\3\2\2\2\tu\3\2\2\2\13\u0083\3\2\2\2\r"+
		"\u008e\3\2\2\2\17\u0096\3\2\2\2\21\u0098\3\2\2\2\23\u00a3\3\2\2\2\25\u00a5"+
		"\3\2\2\2\27\u00a9\3\2\2\2\31\u00db\3\2\2\2\33\u00e7\3\2\2\2\35\u00e9\3"+
		"\2\2\2\37\u00eb\3\2\2\2!\u00ed\3\2\2\2#\u00ef\3\2\2\2%\u00f1\3\2\2\2\'"+
		"\u00f3\3\2\2\2)\u00f8\3\2\2\2+\u00fc\3\2\2\2-\u00ff\3\2\2\2/\u0101\3\2"+
		"\2\2\61\u0103\3\2\2\2\63\u0106\3\2\2\2\65\u010a\3\2\2\2\67\u010d\3\2\2"+
		"\29:\7U\2\2:;\7G\2\2;<\7N\2\2<=\7G\2\2=>\7E\2\2>L\7V\2\2?@\7u\2\2@A\7"+
		"g\2\2AB\7n\2\2BC\7g\2\2CD\7e\2\2DL\7v\2\2EF\7U\2\2FG\7g\2\2GH\7n\2\2H"+
		"I\7g\2\2IJ\7e\2\2JL\7v\2\2K9\3\2\2\2K?\3\2\2\2KE\3\2\2\2L\4\3\2\2\2MN"+
		"\7Y\2\2NO\7J\2\2OP\7G\2\2PQ\7T\2\2Q]\7G\2\2RS\7y\2\2ST\7j\2\2TU\7g\2\2"+
		"UV\7t\2\2V]\7g\2\2WX\7Y\2\2XY\7j\2\2YZ\7g\2\2Z[\7t\2\2[]\7g\2\2\\M\3\2"+
		"\2\2\\R\3\2\2\2\\W\3\2\2\2]\6\3\2\2\2^_\7P\2\2_`\7Q\2\2`h\7V\2\2ab\7p"+
		"\2\2bc\7q\2\2ch\7v\2\2de\7P\2\2ef\7q\2\2fh\7v\2\2g^\3\2\2\2ga\3\2\2\2"+
		"gd\3\2\2\2h\b\3\2\2\2ij\7H\2\2jk\7T\2\2kl\7Q\2\2lv\7O\2\2mn\7h\2\2no\7"+
		"t\2\2op\7q\2\2pv\7o\2\2qr\7H\2\2rs\7t\2\2st\7q\2\2tv\7o\2\2ui\3\2\2\2"+
		"um\3\2\2\2uq\3\2\2\2v\n\3\2\2\2wx\7L\2\2xy\7Q\2\2yz\7K\2\2z\u0084\7P\2"+
		"\2{|\7l\2\2|}\7q\2\2}~\7k\2\2~\u0084\7p\2\2\177\u0080\7L\2\2\u0080\u0081"+
		"\7q\2\2\u0081\u0082\7k\2\2\u0082\u0084\7p\2\2\u0083w\3\2\2\2\u0083{\3"+
		"\2\2\2\u0083\177\3\2\2\2\u0084\f\3\2\2\2\u0085\u0086\7C\2\2\u0086\u0087"+
		"\7P\2\2\u0087\u008f\7F\2\2\u0088\u0089\7c\2\2\u0089\u008a\7p\2\2\u008a"+
		"\u008f\7f\2\2\u008b\u008c\7C\2\2\u008c\u008d\7p\2\2\u008d\u008f\7f\2\2"+
		"\u008e\u0085\3\2\2\2\u008e\u0088\3\2\2\2\u008e\u008b\3\2\2\2\u008f\16"+
		"\3\2\2\2\u0090\u0091\7Q\2\2\u0091\u0097\7T\2\2\u0092\u0093\7q\2\2\u0093"+
		"\u0097\7t\2\2\u0094\u0095\7Q\2\2\u0095\u0097\7t\2\2\u0096\u0090\3\2\2"+
		"\2\u0096\u0092\3\2\2\2\u0096\u0094\3\2\2\2\u0097\20\3\2\2\2\u0098\u0099"+
		"\7Q\2\2\u0099\u009a\7T\2\2\u009a\u009b\7F\2\2\u009b\u009c\7G\2\2\u009c"+
		"\u009d\7T\2\2\u009d\u009e\7\"\2\2\u009e\u009f\7D\2\2\u009f\u00a0\7[\2"+
		"\2\u00a0\22\3\2\2\2\u00a1\u00a4\5\25\13\2\u00a2\u00a4\5\27\f\2\u00a3\u00a1"+
		"\3\2\2\2\u00a3\u00a2\3\2\2\2\u00a4\24\3\2\2\2\u00a5\u00a6\7C\2\2\u00a6"+
		"\u00a7\7U\2\2\u00a7\u00a8\7E\2\2\u00a8\26\3\2\2\2\u00a9\u00aa\7F\2\2\u00aa"+
		"\u00ab\7G\2\2\u00ab\u00ac\7U\2\2\u00ac\u00ad\7E\2\2\u00ad\30\3\2\2\2\u00ae"+
		"\u00af\7N\2\2\u00af\u00b0\7G\2\2\u00b0\u00b1\7H\2\2\u00b1\u00b2\7V\2\2"+
		"\u00b2\u00b3\7\"\2\2\u00b3\u00b4\7Q\2\2\u00b4\u00b5\7W\2\2\u00b5\u00b6"+
		"\7V\2\2\u00b6\u00b7\7G\2\2\u00b7\u00b8\7T\2\2\u00b8\u00b9\7\"\2\2\u00b9"+
		"\u00ba\7L\2\2\u00ba\u00bb\7Q\2\2\u00bb\u00bc\7K\2\2\u00bc\u00dc\7P\2\2"+
		"\u00bd\u00be\7n\2\2\u00be\u00bf\7g\2\2\u00bf\u00c0\7h\2\2\u00c0\u00c1"+
		"\7v\2\2\u00c1\u00c2\7\"\2\2\u00c2\u00c3\7q\2\2\u00c3\u00c4\7w\2\2\u00c4"+
		"\u00c5\7v\2\2\u00c5\u00c6\7g\2\2\u00c6\u00c7\7t\2\2\u00c7\u00c8\7\"\2"+
		"\2\u00c8\u00c9\7l\2\2\u00c9\u00ca\7q\2\2\u00ca\u00cb\7k\2\2\u00cb\u00dc"+
		"\7p\2\2\u00cc\u00cd\7N\2\2\u00cd\u00ce\7g\2\2\u00ce\u00cf\7h\2\2\u00cf"+
		"\u00d0\7v\2\2\u00d0\u00d1\7\"\2\2\u00d1\u00d2\7Q\2\2\u00d2\u00d3\7w\2"+
		"\2\u00d3\u00d4\7v\2\2\u00d4\u00d5\7g\2\2\u00d5\u00d6\7t\2\2\u00d6\u00d7"+
		"\7\"\2\2\u00d7\u00d8\7L\2\2\u00d8\u00d9\7q\2\2\u00d9\u00da\7k\2\2\u00da"+
		"\u00dc\7p\2\2\u00db\u00ae\3\2\2\2\u00db\u00bd\3\2\2\2\u00db\u00cc\3\2"+
		"\2\2\u00dc\32\3\2\2\2\u00dd\u00e8\t\2\2\2\u00de\u00df\7@\2\2\u00df\u00e8"+
		"\7?\2\2\u00e0\u00e1\7>\2\2\u00e1\u00e8\7?\2\2\u00e2\u00e8\7?\2\2\u00e3"+
		"\u00e4\7N\2\2\u00e4\u00e5\7K\2\2\u00e5\u00e6\7M\2\2\u00e6\u00e8\7G\2\2"+
		"\u00e7\u00dd\3\2\2\2\u00e7\u00de\3\2\2\2\u00e7\u00e0\3\2\2\2\u00e7\u00e2"+
		"\3\2\2\2\u00e7\u00e3\3\2\2\2\u00e8\34\3\2\2\2\u00e9\u00ea\7+\2\2\u00ea"+
		"\36\3\2\2\2\u00eb\u00ec\7*\2\2\u00ec \3\2\2\2\u00ed\u00ee\7\60\2\2\u00ee"+
		"\"\3\2\2\2\u00ef\u00f0\7.\2\2\u00f0$\3\2\2\2\u00f1\u00f2\7$\2\2\u00f2"+
		"&\3\2\2\2\u00f3\u00f4\7<\2\2\u00f4(\3\2\2\2\u00f5\u00f9\5/\30\2\u00f6"+
		"\u00f9\5-\27\2\u00f7\u00f9\5\61\31\2\u00f8\u00f5\3\2\2\2\u00f8\u00f6\3"+
		"\2\2\2\u00f8\u00f7\3\2\2\2\u00f9\u00fa\3\2\2\2\u00fa\u00f8\3\2\2\2\u00fa"+
		"\u00fb\3\2\2\2\u00fb*\3\2\2\2\u00fc\u00fd\5\'\24\2\u00fd\u00fe\5)\25\2"+
		"\u00fe,\3\2\2\2\u00ff\u0100\4C\\\2\u0100.\3\2\2\2\u0101\u0102\4c|\2\u0102"+
		"\60\3\2\2\2\u0103\u0104\4\62;\2\u0104\62\3\2\2\2\u0105\u0107\5\61\31\2"+
		"\u0106\u0105\3\2\2\2\u0107\u0108\3\2\2\2\u0108\u0106\3\2\2\2\u0108\u0109"+
		"\3\2\2\2\u0109\64\3\2\2\2\u010a\u010b\5\63\32\2\u010b\66\3\2\2\2\u010c"+
		"\u010e\7\"\2\2\u010d\u010c\3\2\2\2\u010e\u010f\3\2\2\2\u010f\u010d\3\2"+
		"\2\2\u010f\u0110\3\2\2\2\u0110\u0111\3\2\2\2\u0111\u0112\b\34\2\2\u0112"+
		"8\3\2\2\2\21\2K\\gu\u0083\u008e\u0096\u00a3\u00db\u00e7\u00f8\u00fa\u0108"+
		"\u010f\3\b\2\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}