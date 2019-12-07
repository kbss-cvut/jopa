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
		SELECT=1, WHERE=2, NOT=3, FROM=4, JOIN=5, AND=6, OR=7, LEFTOUTERJOIN=8, 
		QUERYOPERATOR=9, RIGHTPAREN=10, LEFTPAREN=11, DOT=12, COMMA=13, QMARK=14, 
		COLON=15, TEXT=16, COLONTEXT=17, UPPERCASE=18, LOWERCASE=19, DIGIT=20, 
		NUMBER=21, VALUE=22, WHITESPACE=23;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] ruleNames = {
		"SELECT", "WHERE", "NOT", "FROM", "JOIN", "AND", "OR", "LEFTOUTERJOIN", 
		"QUERYOPERATOR", "RIGHTPAREN", "LEFTPAREN", "DOT", "COMMA", "QMARK", "COLON", 
		"TEXT", "COLONTEXT", "UPPERCASE", "LOWERCASE", "DIGIT", "NUMBER", "VALUE", 
		"WHITESPACE"
	};

	private static final String[] _LITERAL_NAMES = {
		null, null, null, null, null, null, null, null, null, null, "')'", "'('", 
		"'.'", "','", "'\"'", "':'"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, "SELECT", "WHERE", "NOT", "FROM", "JOIN", "AND", "OR", "LEFTOUTERJOIN", 
		"QUERYOPERATOR", "RIGHTPAREN", "LEFTPAREN", "DOT", "COMMA", "QMARK", "COLON", 
		"TEXT", "COLONTEXT", "UPPERCASE", "LOWERCASE", "DIGIT", "NUMBER", "VALUE", 
		"WHITESPACE"
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
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2\31\u00f5\b\1\4\2"+
		"\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4"+
		"\13\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22"+
		"\t\22\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\3\2"+
		"\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\5"+
		"\2D\n\2\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\5"+
		"\3U\n\3\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\5\4`\n\4\3\5\3\5\3\5\3\5\3"+
		"\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\5\5n\n\5\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3"+
		"\6\3\6\3\6\3\6\3\6\5\6|\n\6\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\5\7\u0087"+
		"\n\7\3\b\3\b\3\b\3\b\3\b\3\b\5\b\u008f\n\b\3\t\3\t\3\t\3\t\3\t\3\t\3\t"+
		"\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3"+
		"\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t"+
		"\3\t\3\t\3\t\5\t\u00be\n\t\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\5\n"+
		"\u00ca\n\n\3\13\3\13\3\f\3\f\3\r\3\r\3\16\3\16\3\17\3\17\3\20\3\20\3\21"+
		"\3\21\3\21\6\21\u00db\n\21\r\21\16\21\u00dc\3\22\3\22\3\22\3\23\3\23\3"+
		"\24\3\24\3\25\3\25\3\26\6\26\u00e9\n\26\r\26\16\26\u00ea\3\27\3\27\3\30"+
		"\6\30\u00f0\n\30\r\30\16\30\u00f1\3\30\3\30\2\2\31\3\3\5\4\7\5\t\6\13"+
		"\7\r\b\17\t\21\n\23\13\25\f\27\r\31\16\33\17\35\20\37\21!\22#\23%\24\'"+
		"\25)\26+\27-\30/\31\3\2\3\4\2>>@@\2\u010d\2\3\3\2\2\2\2\5\3\2\2\2\2\7"+
		"\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r\3\2\2\2\2\17\3\2\2\2\2\21\3\2\2"+
		"\2\2\23\3\2\2\2\2\25\3\2\2\2\2\27\3\2\2\2\2\31\3\2\2\2\2\33\3\2\2\2\2"+
		"\35\3\2\2\2\2\37\3\2\2\2\2!\3\2\2\2\2#\3\2\2\2\2%\3\2\2\2\2\'\3\2\2\2"+
		"\2)\3\2\2\2\2+\3\2\2\2\2-\3\2\2\2\2/\3\2\2\2\3C\3\2\2\2\5T\3\2\2\2\7_"+
		"\3\2\2\2\tm\3\2\2\2\13{\3\2\2\2\r\u0086\3\2\2\2\17\u008e\3\2\2\2\21\u00bd"+
		"\3\2\2\2\23\u00c9\3\2\2\2\25\u00cb\3\2\2\2\27\u00cd\3\2\2\2\31\u00cf\3"+
		"\2\2\2\33\u00d1\3\2\2\2\35\u00d3\3\2\2\2\37\u00d5\3\2\2\2!\u00da\3\2\2"+
		"\2#\u00de\3\2\2\2%\u00e1\3\2\2\2\'\u00e3\3\2\2\2)\u00e5\3\2\2\2+\u00e8"+
		"\3\2\2\2-\u00ec\3\2\2\2/\u00ef\3\2\2\2\61\62\7U\2\2\62\63\7G\2\2\63\64"+
		"\7N\2\2\64\65\7G\2\2\65\66\7E\2\2\66D\7V\2\2\678\7u\2\289\7g\2\29:\7n"+
		"\2\2:;\7g\2\2;<\7e\2\2<D\7v\2\2=>\7U\2\2>?\7g\2\2?@\7n\2\2@A\7g\2\2AB"+
		"\7e\2\2BD\7v\2\2C\61\3\2\2\2C\67\3\2\2\2C=\3\2\2\2D\4\3\2\2\2EF\7Y\2\2"+
		"FG\7J\2\2GH\7G\2\2HI\7T\2\2IU\7G\2\2JK\7y\2\2KL\7j\2\2LM\7g\2\2MN\7t\2"+
		"\2NU\7g\2\2OP\7Y\2\2PQ\7j\2\2QR\7g\2\2RS\7t\2\2SU\7g\2\2TE\3\2\2\2TJ\3"+
		"\2\2\2TO\3\2\2\2U\6\3\2\2\2VW\7P\2\2WX\7Q\2\2X`\7V\2\2YZ\7p\2\2Z[\7q\2"+
		"\2[`\7v\2\2\\]\7P\2\2]^\7q\2\2^`\7v\2\2_V\3\2\2\2_Y\3\2\2\2_\\\3\2\2\2"+
		"`\b\3\2\2\2ab\7H\2\2bc\7T\2\2cd\7Q\2\2dn\7O\2\2ef\7h\2\2fg\7t\2\2gh\7"+
		"q\2\2hn\7o\2\2ij\7H\2\2jk\7t\2\2kl\7q\2\2ln\7o\2\2ma\3\2\2\2me\3\2\2\2"+
		"mi\3\2\2\2n\n\3\2\2\2op\7L\2\2pq\7Q\2\2qr\7K\2\2r|\7P\2\2st\7l\2\2tu\7"+
		"q\2\2uv\7k\2\2v|\7p\2\2wx\7L\2\2xy\7q\2\2yz\7k\2\2z|\7p\2\2{o\3\2\2\2"+
		"{s\3\2\2\2{w\3\2\2\2|\f\3\2\2\2}~\7C\2\2~\177\7P\2\2\177\u0087\7F\2\2"+
		"\u0080\u0081\7c\2\2\u0081\u0082\7p\2\2\u0082\u0087\7f\2\2\u0083\u0084"+
		"\7C\2\2\u0084\u0085\7p\2\2\u0085\u0087\7f\2\2\u0086}\3\2\2\2\u0086\u0080"+
		"\3\2\2\2\u0086\u0083\3\2\2\2\u0087\16\3\2\2\2\u0088\u0089\7Q\2\2\u0089"+
		"\u008f\7T\2\2\u008a\u008b\7q\2\2\u008b\u008f\7t\2\2\u008c\u008d\7Q\2\2"+
		"\u008d\u008f\7t\2\2\u008e\u0088\3\2\2\2\u008e\u008a\3\2\2\2\u008e\u008c"+
		"\3\2\2\2\u008f\20\3\2\2\2\u0090\u0091\7N\2\2\u0091\u0092\7G\2\2\u0092"+
		"\u0093\7H\2\2\u0093\u0094\7V\2\2\u0094\u0095\7\"\2\2\u0095\u0096\7Q\2"+
		"\2\u0096\u0097\7W\2\2\u0097\u0098\7V\2\2\u0098\u0099\7G\2\2\u0099\u009a"+
		"\7T\2\2\u009a\u009b\7\"\2\2\u009b\u009c\7L\2\2\u009c\u009d\7Q\2\2\u009d"+
		"\u009e\7K\2\2\u009e\u00be\7P\2\2\u009f\u00a0\7n\2\2\u00a0\u00a1\7g\2\2"+
		"\u00a1\u00a2\7h\2\2\u00a2\u00a3\7v\2\2\u00a3\u00a4\7\"\2\2\u00a4\u00a5"+
		"\7q\2\2\u00a5\u00a6\7w\2\2\u00a6\u00a7\7v\2\2\u00a7\u00a8\7g\2\2\u00a8"+
		"\u00a9\7t\2\2\u00a9\u00aa\7\"\2\2\u00aa\u00ab\7l\2\2\u00ab\u00ac\7q\2"+
		"\2\u00ac\u00ad\7k\2\2\u00ad\u00be\7p\2\2\u00ae\u00af\7N\2\2\u00af\u00b0"+
		"\7g\2\2\u00b0\u00b1\7h\2\2\u00b1\u00b2\7v\2\2\u00b2\u00b3\7\"\2\2\u00b3"+
		"\u00b4\7Q\2\2\u00b4\u00b5\7w\2\2\u00b5\u00b6\7v\2\2\u00b6\u00b7\7g\2\2"+
		"\u00b7\u00b8\7t\2\2\u00b8\u00b9\7\"\2\2\u00b9\u00ba\7L\2\2\u00ba\u00bb"+
		"\7q\2\2\u00bb\u00bc\7k\2\2\u00bc\u00be\7p\2\2\u00bd\u0090\3\2\2\2\u00bd"+
		"\u009f\3\2\2\2\u00bd\u00ae\3\2\2\2\u00be\22\3\2\2\2\u00bf\u00ca\t\2\2"+
		"\2\u00c0\u00c1\7@\2\2\u00c1\u00ca\7?\2\2\u00c2\u00c3\7>\2\2\u00c3\u00ca"+
		"\7?\2\2\u00c4\u00ca\7?\2\2\u00c5\u00c6\7N\2\2\u00c6\u00c7\7K\2\2\u00c7"+
		"\u00c8\7M\2\2\u00c8\u00ca\7G\2\2\u00c9\u00bf\3\2\2\2\u00c9\u00c0\3\2\2"+
		"\2\u00c9\u00c2\3\2\2\2\u00c9\u00c4\3\2\2\2\u00c9\u00c5\3\2\2\2\u00ca\24"+
		"\3\2\2\2\u00cb\u00cc\7+\2\2\u00cc\26\3\2\2\2\u00cd\u00ce\7*\2\2\u00ce"+
		"\30\3\2\2\2\u00cf\u00d0\7\60\2\2\u00d0\32\3\2\2\2\u00d1\u00d2\7.\2\2\u00d2"+
		"\34\3\2\2\2\u00d3\u00d4\7$\2\2\u00d4\36\3\2\2\2\u00d5\u00d6\7<\2\2\u00d6"+
		" \3\2\2\2\u00d7\u00db\5\'\24\2\u00d8\u00db\5%\23\2\u00d9\u00db\5)\25\2"+
		"\u00da\u00d7\3\2\2\2\u00da\u00d8\3\2\2\2\u00da\u00d9\3\2\2\2\u00db\u00dc"+
		"\3\2\2\2\u00dc\u00da\3\2\2\2\u00dc\u00dd\3\2\2\2\u00dd\"\3\2\2\2\u00de"+
		"\u00df\5\37\20\2\u00df\u00e0\5!\21\2\u00e0$\3\2\2\2\u00e1\u00e2\4C\\\2"+
		"\u00e2&\3\2\2\2\u00e3\u00e4\4c|\2\u00e4(\3\2\2\2\u00e5\u00e6\4\62;\2\u00e6"+
		"*\3\2\2\2\u00e7\u00e9\5)\25\2\u00e8\u00e7\3\2\2\2\u00e9\u00ea\3\2\2\2"+
		"\u00ea\u00e8\3\2\2\2\u00ea\u00eb\3\2\2\2\u00eb,\3\2\2\2\u00ec\u00ed\5"+
		"+\26\2\u00ed.\3\2\2\2\u00ee\u00f0\7\"\2\2\u00ef\u00ee\3\2\2\2\u00f0\u00f1"+
		"\3\2\2\2\u00f1\u00ef\3\2\2\2\u00f1\u00f2\3\2\2\2\u00f2\u00f3\3\2\2\2\u00f3"+
		"\u00f4\b\30\2\2\u00f4\60\3\2\2\2\20\2CT_m{\u0086\u008e\u00bd\u00c9\u00da"+
		"\u00dc\u00ea\u00f1\3\b\2\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}